library(shiny)
 
# Shown in the page footer; update when you publish a new version
LAST_UPDATED <- "October 1, 2026"
APP_VERSION  <- "1.1"

# ---- Fixed settings ----
MAX_AGE <- 400
N_CLASS <- 200
AGES    <- 0:MAX_AGE

# ---- Distribution samplers ----
rtri <- function(n, lo, mode, hi) {
  u  <- runif(n)
  fc <- (mode - lo) / (hi - lo)
  ifelse(u < fc,
         lo + sqrt(u * (hi - lo) * (mode - lo)),
         hi - sqrt((1 - u) * (hi - lo) * (hi - mode)))
}

rpert <- function(n, lo, mode, hi) {
  a1 <- 1 + 4 * (mode - lo) / (hi - lo)
  a2 <- 1 + 4 * (hi - mode) / (hi - lo)
  lo + (hi - lo) * rbeta(n, a1, a2)
}

rtnorm <- function(n, m, s, lo, hi) {
  qnorm(runif(n, pnorm(lo, m, s), pnorm(hi, m, s)), m, s)
}

rtlnorm <- function(n, m, s, lo, hi) {
  sdlog   <- sqrt(log(1 + s^2 / m^2))
  meanlog <- log(m) - sdlog^2 / 2
  qlnorm(runif(n, plnorm(lo, meanlog, sdlog), plnorm(hi, meanlog, sdlog)), meanlog, sdlog)
}

draw <- function(n, s) {
  switch(s$dist,
    Fixed      = rep(s$value, n),
    Uniform    = runif(n, s$min, s$max),
    Triangular = rtri(n, s$min, s$mode, s$max),
    PERT       = rpert(n, s$min, s$mode, s$max),
    Beta       = s$min + (s$max - s$min) * rbeta(n, s$shape1, s$shape2),
    Normal     = rtnorm(n, s$mean, s$sd, s$min, s$max),
    Lognormal  = rtlnorm(n, s$mean, s$sd, s$min, s$max))
}

check_spec <- function(s, label) {
  if (s$dist == "Fixed") return(if (is.na(s$value)) paste(label, "needs a value") else NULL)
  if (anyNA(c(s$min, s$max)) || s$min >= s$max) return(paste(label, "needs min below max"))
  if (s$dist %in% c("Triangular", "PERT") &&
      (is.na(s$mode) || s$mode < s$min || s$mode > s$max))
    return(paste(label, "needs a likeliest value between min and max"))
  if (s$dist == "Beta" && (anyNA(c(s$shape1, s$shape2)) || s$shape1 <= 0 || s$shape2 <= 0))
    return(paste(label, "needs positive Beta shapes"))
  if (s$dist %in% c("Normal", "Lognormal") && (anyNA(c(s$mean, s$sd)) || s$sd <= 0))
    return(paste(label, "needs a mean and a positive SD"))
  if (s$dist == "Lognormal" && s$mean <= 0) return(paste(label, "needs a positive mean"))
  NULL
}

# ---- Model pipeline ----
hazard <- function(model, x, a, b, s) {
  u <- switch(model,
    logistic    = a * exp(b * x) / (1 + (a * s / b) * (exp(b * x) - 1)),
    gompertz    = a * exp(b * x),
    exponential = rep(a, length(x)))
  u[is.nan(u)] <- b / s   # logistic plateau if exp() overflows
  u
}

life_table <- function(u) {
  lx <- pmax(exp(-c(0, cumsum(u)[-length(u)])), 1e-300)
  Tx <- rev(cumsum(rev(lx)))
  list(u = u, px = exp(-u), lx = lx, ex = Tx / lx - 0.5)
}

age_specific_vc <- function(lt, n, MA2, vec_comp = 1) {
  x    <- 0:(N_CLASS - 1)
  L    <- c(0, cumsum(-lt$u))
  surv <- exp(L[x + n + 1] - L[x + 1])
  Cx   <- MA2 * surv * lt$ex[x + n + 1] * vec_comp
  Cx[!is.finite(Cx)] <- 0
  Cx
}

ct_synchronous <- function(Cx) mean(Cx[4:7])

ct_stable <- function(lt, Cx, r, sigma) {
  idx  <- seq_len(N_CLASS)
  w    <- lt$lx[idx] * exp(-r * (idx - 1))
  w    <- w / sum(w)
  keep <- (round(sigma) + 1):N_CLASS
  sum(Cx[keep] * w[keep])
}

# ---- Model check against published values, and r, computed once at startup ----
styer_pars <- list(
  exponential = c(a = 0.0313, b = 0,      s = 0),
  gompertz    = c(a = 0.0066, b = 0.0623, s = 0),
  logistic    = c(a = 0.0018, b = 0.1416, s = 1.0730))
styer_lt <- lapply(names(styer_pars), function(m) {
  p <- styer_pars[[m]]
  life_table(hazard(m, AGES, p[["a"]], p[["b"]], p[["s"]]))
})
names(styer_lt) <- names(styer_pars)
styer_cx <- lapply(styer_lt, age_specific_vc, n = 10, MA2 = 1.5 * 0.75^2)
r_hat <- uniroot(function(r) ct_stable(styer_lt$exponential, styer_cx$exponential, r, 3) - 11.4,
                 c(0, 1))$root

validation <- data.frame(
  Model                        = names(styer_pars),
  `Mean lifespan`              = sapply(styer_lt, function(lt) sum(lt$lx) - 0.5),
  `Ct synchronous`             = sapply(styer_cx, ct_synchronous),
  `Ct synchronous (published)` = c(19.7, 16.0, 15.2),
  `Ct stable`                  = mapply(function(lt, cx) ct_stable(lt, cx, r_hat, 3), styer_lt, styer_cx),
  `Ct stable (published)`      = c(11.4, 8.5, 7.9),
  check.names = FALSE, row.names = NULL)
validation[["Sync. diff (%)"]] <-
  100 * (validation[["Ct synchronous"]] - validation[["Ct synchronous (published)"]]) /
        validation[["Ct synchronous (published)"]]
validation[["Stable diff (%)"]] <-
  100 * (validation[["Ct stable"]] - validation[["Ct stable (published)"]]) /
        validation[["Ct stable (published)"]]
max_dev <- max(abs(unlist(validation[, c("Sync. diff (%)", "Stable diff (%)")])))

# ---- Default assumptions ----
sp <- function(dist, value, min, mode, max, mean, sd, shape1 = 2, shape2 = 2)
  list(dist = dist, value = value, min = min, mode = mode, max = max,
       mean = mean, sd = sd, shape1 = shape1, shape2 = shape2)

vc_specs <- list(
  a_bite   = sp("Fixed", 0.75, 0.25, 0.42,  0.76, 0.45,  0.10, 2, 3),
  n_eip    = sp("Fixed", 10,   4,    6.5,   14,   9,     2.5),
  m_dens   = sp("Fixed", 1.5,  0.42, 0.465, 0.5,  0.465, 0.03),
  vec_comp = sp("Fixed", 1,    0.25, 0.525, 0.8,  0.525, 0.12, 2, 2))

mort_specs <- list(
  logistic = list(
    mort_a = sp("Fixed", 0.0018, 0.0001, 0.0018, 0.01, 0.0018, 0.0004),
    mort_b = sp("Fixed", 0.1416, 0.0001, 0.1416, 0.5,  0.1416, 0.017),
    mort_s = sp("Fixed", 1.0730, 0,    1.0730, 5,    1.0730, 0.234)),
  gompertz = list(
    mort_a = sp("Fixed", 0.0066, 0.0001, 0.0066, 0.03, 0.0066, 0.0015),
    mort_b = sp("Fixed", 0.0623, 0.0001, 0.0623, 0.3,  0.0623, 0.017),
    mort_s = sp("Fixed", 0,      0,    0,      5,    0,      0.1)),
  exponential = list(
    mort_a = sp("Fixed", 0.0313, 0.0001, 0.0313, 0.1,  0.0313, 0.007),
    mort_b = sp("Fixed", 0,      0,    0,      1,    0,      0.1),
    mort_s = sp("Fixed", 0,      0,    0,      5,    0,      0.1)))

# Population age structure. Used only with the stable age distribution. No published range is built in,
# so both start fixed; the min/max/shape values are just starting points if you choose a distribution.
pop_specs <- list(
  growth_r   = sp("Fixed", round(r_hat, 4), 0.05, round(r_hat, 4), 0.25, round(r_hat, 4), 0.03),
  first_bite = sp("Fixed", 3, 1, 3, 5, 3, 1))

# Default distribution for each assumption (literature-based)
lit_dists <- c(a_bite = "Beta", n_eip = "Uniform", m_dens = "Uniform", vec_comp = "Beta",
               mort_a = "Normal", mort_b = "Normal", mort_s = "Normal",
               growth_r = "Fixed", first_bite = "Fixed")

labels <- c(a_bite   = "Biting rate (bites per day)",
            n_eip    = "Extrinsic incubation period (days)",
            m_dens   = "Mosquito density (per person)",
            vec_comp = "Vector competence",
            mort_a   = "Mortality a (initial hazard)",
            mort_b   = "Mortality b (rate of aging)",
            mort_s   = "Mortality s (deceleration)",
            growth_r = "Population growth rate r",
            first_bite = "Age at first bite (days)")

fields       <- c("value", "min", "mode", "max", "mean", "sd", "shape1", "shape2")
all_setting_ids <- c(names(vc_specs), names(mort_specs$logistic), names(pop_specs))
dist_choices <- c("Fixed", "Uniform", "Triangular", "PERT", "Beta", "Normal", "Lognormal")

descs <- c(a_bite   = "Bites on humans per mosquito per day.",
           n_eip    = "Days from infection to infectiousness in the mosquito.",
           m_dens   = "Female mosquitoes per human host.",
           vec_comp = "Chance a mosquito becomes infectious after an infectious blood meal.",
           mort_a   = "Daily mortality rate at emergence (per day).",
           mort_b   = "How quickly mortality rises with age (per day).",
           mort_s   = "Slows the rise in mortality at old ages (logistic model only).",
           growth_r = "Daily growth of the mosquito population; sets how the stable age distribution is weighted.",
           first_bite = "Age in days at which a mosquito first bites a human.")

# Extra provenance notes shown on a card
card_notes <- c(
  growth_r   = "The default is solved so the exponential stable-age case matches the Ct published by Styer et al. (see Model check). It is not an independent field estimate. No range is built in, so it starts fixed.",
  first_bite = "No published range is built in, so it starts fixed.")

# One-line description of a distribution spec, used in downloads
describe_spec <- function(s) {
  g <- function(...) paste(sprintf("%s = %s", c(...), signif(unlist(s[c(...)]), 6)), collapse = ", ")
  switch(s$dist,
    Fixed      = g("value"),
    Uniform    = g("min", "max"),
    Triangular = g("min", "mode", "max"),
    PERT       = g("min", "mode", "max"),
    Beta       = g("min", "max", "shape1", "shape2"),
    Normal     = paste0(g("mean", "sd"), " (truncated ", g("min", "max"), ")"),
    Lognormal  = paste0(g("mean", "sd"), " (truncated ", g("min", "max"), ")"))
}

# Forecast histogram, shared by the on-screen plot and the PNG download.
# With `prev` (a second set of Ct values) the two runs are overlaid as semi-transparent densities.
has_thresh <- function(t) length(t) == 1 && !is.na(t)

# Dotted vertical line at the chosen threshold, labelled with the share of trials above it
draw_threshold <- function(ct, thresh) {
  col <- "#b30000"
  abline(v = thresh, col = col, lwd = 2.5, lty = 3)
  usr <- par("usr")
  text(thresh, usr[3] + 0.72 * (usr[4] - usr[3]),
       sprintf("Ct = %s\n%.1f%% of trials above", format(thresh), 100 * mean(ct > thresh)),
       pos = if (thresh > mean(usr[1:2])) 2 else 4, col = col, cex = 0.85, offset = 0.5)
}

draw_forecast <- function(ct, b, prev = NULL, run_labels = NULL, thresh = NA) {
  if (!is.null(prev)) {
    runs <- list(ct, prev)
    cols <- c("steelblue", "#E69F00")
    all  <- unlist(runs)
    pad  <- max(diff(range(all)) * 0.05, 0.05 * abs(mean(all)), 1e-6)
    drng <- range(all) + c(-pad, pad)
    br   <- seq(drng[1], drng[2], length.out = 51)
    xlim <- if (has_thresh(thresh)) range(drng, thresh) else drng     # keep the threshold on the plot
    # A run whose trials all give the same Ct (all assumptions fixed) is drawn as a line, not bars
    hs   <- lapply(runs, function(x) if (diff(range(x)) > 0) hist(x, breaks = br, plot = FALSE))
    ytop <- max(c(1e-9, unlist(lapply(hs, function(h) if (!is.null(h)) h$density)))) * 1.1
    plot(NA, xlim = xlim, ylim = c(0, ytop), xlab = "Ct", ylab = "Density",
         main = "Forecast of total vectorial capacity")
    for (k in 1:2) {
      if (!is.null(hs[[k]])) {
        plot(hs[[k]], freq = FALSE, add = TRUE, col = adjustcolor(cols[k], 0.55), border = "white")
        abline(v = median(runs[[k]]), lwd = 2, lty = 2, col = cols[k])
      } else segments(runs[[k]][1], 0, runs[[k]][1], ytop * 0.9, lwd = 4, col = cols[k])
    }
    legend("topright", legend = run_labels, fill = adjustcolor(cols, 0.7), border = NA, bty = "n")
    if (has_thresh(thresh)) draw_threshold(ct, thresh)
    return(invisible())
  }
  h  <- hist(ct, breaks = 50, plot = FALSE)
  xl <- range(h$breaks); if (has_thresh(thresh)) xl <- range(xl, thresh)
  plot(h, xlim = xl, col = ifelse(h$mids >= b[1] & h$mids <= b[2], "steelblue", "grey85"),
       border = "white", main = "Forecast of total vectorial capacity", xlab = "Ct")
  abline(v = median(ct), lwd = 2, lty = 2)
  if (has_thresh(thresh)) draw_threshold(ct, thresh)
}

# ---- Sampling and the simulation loop (shared by the app and the tests) ----

# Values for uniform draws u (between 0 and 1) from an assumption's distribution (inverse CDF).
# Used when draws must be linked to each other (correlations).
qdraw <- function(u, s) {
  switch(s$dist,
    Fixed      = rep(s$value, length(u)),
    Uniform    = s$min + u * (s$max - s$min),
    Triangular = { fc <- (s$mode - s$min) / (s$max - s$min)
                   ifelse(u < fc, s$min + sqrt(u * (s$max - s$min) * (s$mode - s$min)),
                          s$max - sqrt((1 - u) * (s$max - s$min) * (s$max - s$mode))) },
    PERT       = { a1 <- 1 + 4 * (s$mode - s$min) / (s$max - s$min)
                   a2 <- 1 + 4 * (s$max - s$mode) / (s$max - s$min)
                   s$min + (s$max - s$min) * qbeta(u, a1, a2) },
    Beta       = s$min + (s$max - s$min) * qbeta(u, s$shape1, s$shape2),
    Normal     = qnorm(pnorm(s$min, s$mean, s$sd) + u * (pnorm(s$max, s$mean, s$sd) - pnorm(s$min, s$mean, s$sd)),
                       s$mean, s$sd),
    Lognormal  = { sdlog <- sqrt(log(1 + s$sd^2 / s$mean^2)); meanlog <- log(s$mean) - sdlog^2 / 2
                   qlnorm(plnorm(s$min, meanlog, sdlog) + u * (plnorm(s$max, meanlog, sdlog) - plnorm(s$min, meanlog, sdlog)),
                          meanlog, sdlog) })
}

# Turn a reported 95% interval (and optionally a reported mean) into the parameters of a distribution.
# `s` holds the card's current Min and Max, which are the support for Beta and the truncation for
# Normal and Lognormal. Returns list(ok, fields, note) where `fields` are the values to put on the card.
fit_range <- function(dist, lo, hi, est, s) {
  fail <- function(msg) list(ok = FALSE, fields = NULL, note = msg)
  if (is.na(lo) || is.na(hi)) return(fail("Enter the lower and upper ends of the reported 95% interval."))
  if (lo >= hi) return(fail("The lower end must be below the upper end."))
  if (!is.na(est) && (est < lo || est > hi)) return(fail("The reported mean should lie inside its interval."))
  z <- qnorm(0.975)
  if (dist == "Uniform") {
    pad <- (hi - lo) * 0.025 / 0.95    # a 95% interval covers the middle 95% of a uniform
    return(list(ok = TRUE, fields = list(min = lo - pad, max = hi + pad),
                note = "Min and Max set so the middle 95% of the range matches the interval."))
  }
  if (dist == "Normal") {
    m  <- if (is.na(est)) (lo + hi) / 2 else est
    sd <- (hi - lo) / (2 * z)
    out <- list(ok = TRUE, fields = list(mean = m, sd = sd), note = "Mean and SD set from the interval.")
    if (!is.na(s$min) && !is.na(s$max) && (lo < s$min || hi > s$max))
      out$note <- paste(out$note, "Part of the interval lies outside this card's Min and Max, which truncate the draws.")
    return(out)
  }
  if (dist == "Lognormal") {
    if (lo <= 0) return(fail("A lognormal needs a positive lower end."))
    sdlog <- (log(hi) - log(lo)) / (2 * z)
    meanlog <- if (is.na(est)) (log(lo) + log(hi)) / 2 else log(est) - sdlog^2 / 2
    m <- exp(meanlog + sdlog^2 / 2)
    out <- list(ok = TRUE, fields = list(mean = m, sd = m * sqrt(exp(sdlog^2) - 1)),
                note = if (is.na(est)) "Mean and SD set so the interval is symmetric on the log scale."
                       else "Mean set to the reported mean and SD to the interval's width on the log scale.")
    if (!is.na(s$min) && !is.na(s$max) && (lo < s$min || hi > s$max))
      out$note <- paste(out$note, "Part of the interval lies outside this card's Min and Max, which truncate the draws.")
    return(out)
  }
  if (dist == "Beta") {
    mn <- s$min; mx <- s$max
    if (is.na(mn) || is.na(mx) || mn >= mx) return(fail("Set Min and Max (the range the Beta is stretched over) first."))
    if (lo < mn || hi > mx) return(fail("The interval must lie inside Min and Max. Widen Min and Max first."))
    u <- function(x) (x - mn) / (mx - mn)
    l <- u(lo); h <- u(hi); e <- if (is.na(est)) NA else u(est)
    loss <- function(p) {
      a <- exp(p[1]); b <- exp(p[2])
      r <- c(qbeta(0.025, a, b) - l, qbeta(0.975, a, b) - h, if (!is.na(e)) a / (a + b) - e)
      sum(r^2) / (h - l)^2
    }
    best <- NULL
    for (st in list(c(0, 0), c(1, 1), c(2, 0.5), c(0.5, 2), c(3, 3)) ) {
      o <- tryCatch(optim(st, loss, method = "Nelder-Mead", control = list(maxit = 2000, reltol = 1e-12)),
                    error = function(e) NULL)
      if (!is.null(o) && (is.null(best) || o$value < best$value)) best <- o
    }
    if (is.null(best)) return(fail("The shapes could not be fitted to that interval."))
    a <- exp(best$par[1]); b <- exp(best$par[2])
    if (a > 1e4 || b > 1e4) return(fail("The interval is too narrow for its position inside Min and Max. Narrow Min and Max."))
    got <- mn + (mx - mn) * qbeta(c(0.025, 0.975), a, b)
    note <- sprintf("Shapes %.2f and %.2f give a 95%% interval of %s to %s and a mean of %s.",
                    a, b, signif(got[1], 3), signif(got[2], 3), signif(mn + (mx - mn) * a / (a + b), 3))
    if (best$value > 1e-3) note <- paste(note, "This is the closest match; the mean and interval do not agree exactly.")
    return(list(ok = TRUE, fields = list(shape1 = a, shape2 = b), note = note))
  }
  fail("Fitting works for Uniform, Normal, Lognormal and Beta. Pick one of those first.")
}

# Uniform draws with the requested rank (Spearman) correlations, using a Gaussian copula.
# `pairs` has columns a, b, rho. If the requested correlations cannot all hold at once (the matrix is
# not positive definite) they are shrunk together by the smallest amount that makes them consistent.
correlated_uniforms <- function(n, ids, pairs) {
  k <- length(ids); R <- diag(k); dimnames(R) <- list(ids, ids)
  for (i in seq_len(nrow(pairs))) {
    r <- 2 * sin(pi * pairs$rho[i] / 6)            # Spearman to the correlation of the underlying normals
    R[pairs$a[i], pairs$b[i]] <- r; R[pairs$b[i], pairs$a[i]] <- r
  }
  shrink <- 0
  if (min(eigen(R, symmetric = TRUE, only.values = TRUE)$values) < 1e-8) {
    lo <- 0; hi <- 1
    for (it in 1:40) { mid <- (lo + hi) / 2
      if (min(eigen((1 - mid) * R + mid * diag(k), symmetric = TRUE, only.values = TRUE)$values) < 1e-8) lo <- mid else hi <- mid }
    shrink <- hi; R <- (1 - shrink) * R + shrink * diag(k)
  }
  z <- matrix(rnorm(n * k), n, k) %*% chol(R)
  u <- pnorm(z); colnames(u) <- ids
  list(u = u, shrink = shrink)
}

# All draws for one run. Independent assumptions use their own distribution; assumptions in `pairs`
# are drawn together with the requested correlations; assumptions found in `uploaded` ($df) take
# whole rows from the uploaded table, which keeps their joint structure.
generate_draws <- function(specs, n, pairs = NULL, uploaded = NULL) {
  ids   <- names(specs); notes <- character(0)
  up_ids <- if (!is.null(uploaded)) intersect(ids, names(uploaded$df)) else character(0)
  varying <- ids[vapply(specs, function(s) s$dist != "Fixed", logical(1))]
  use <- NULL
  if (!is.null(pairs) && nrow(pairs)) {
    ok  <- pairs$a %in% varying & pairs$b %in% varying & !(pairs$a %in% up_ids) & !(pairs$b %in% up_ids)
    if (any(!ok)) notes <- c(notes, sprintf("Correlation skipped (an assumption is fixed, uploaded or not in use): %s and %s",
                                            labels[pairs$a[!ok]], labels[pairs$b[!ok]]))
    use <- pairs[ok, , drop = FALSE]
  }
  corr_ids <- if (!is.null(use) && nrow(use)) intersect(ids, unique(c(use$a, use$b))) else character(0)
  indep    <- setdiff(ids, c(up_ids, corr_ids))
  cols <- lapply(specs[indep], function(s) draw(n, s))
  shrink <- 0
  if (length(corr_ids)) {
    cu <- correlated_uniforms(n, corr_ids, use); shrink <- cu$shrink
    for (id in corr_ids) cols[[id]] <- qdraw(cu$u[, id], specs[[id]])
    if (shrink > 0) notes <- c(notes, sprintf("The requested correlations were not all possible together, so they were reduced by %.0f%% to be consistent.", 100 * shrink))
  }
  if (length(up_ids)) {
    m   <- nrow(uploaded$df)
    idx <- if (m >= n) sample.int(m, n) else sample.int(m, n, replace = TRUE)
    for (id in up_ids) cols[[id]] <- uploaded$df[[id]][idx]
    notes <- c(notes, sprintf("%s taken from the uploaded draws (%s rows, %s).",
                              paste(labels[up_ids], collapse = ", "), format(m, big.mark = ","),
                              if (m >= n) "sampled without replacement" else "sampled with replacement"))
  }
  d <- as.data.frame(cols[ids], check.names = FALSE)
  achieved <- NULL
  if (!is.null(use) && nrow(use))
    achieved <- cbind(use, achieved = vapply(seq_len(nrow(use)), function(i) cor(d[[use$a[i]]], d[[use$b[i]]], method = "spearman"), 0))
  list(draws = d, notes = notes, achieved = achieved)
}

# The age-specific model for a block of trials at once. Rows are trials and columns are ages 0 to
# MAX_AGE, so each loop below runs over ages and every step works on all trials together. It does the
# same arithmetic as hazard(), life_table(), age_specific_vc() and ct_synchronous()/ct_stable() applied
# to one trial at a time (the tests check this). `n_eip` must be whole days between 1 and 150, and
# `r` and `sigma` are needed only for the stable age distribution.
ct_batch <- function(model, a, b, s, n_eip, MA2, vec_comp, r = NULL, sigma = NULL) {
  k  <- length(a); na <- MAX_AGE + 1L
  U  <- if (model == "exponential") matrix(a, k, na) else {
    E <- exp(outer(b, AGES))
    if (model == "gompertz") a * E else a * E / (1 + (a * s / b) * (E - 1))
  }
  bad <- which(is.nan(U))
  if (length(bad)) U[bad] <- (b / s)[(bad - 1L) %% k + 1L]     # logistic plateau if exp() overflows
  H <- matrix(0, k, na)                                         # cumulative hazard before each age
  for (j in 2:na) H[, j] <- H[, j - 1L] + U[, j - 1L]
  rm(U)
  lx <- pmax(exp(-H), 1e-300)
  Tx <- matrix(0, k, na); Tx[, na] <- lx[, na]
  for (j in (na - 1L):1L) Tx[, j] <- Tx[, j + 1L] + lx[, j]
  ex <- Tx / lx - 0.5; rm(Tx)
  x    <- 0:(N_CLASS - 1)
  rows <- rep(seq_len(k), N_CLASS)
  at_n <- cbind(rows, rep(x, each = k) + rep(n_eip, N_CLASS) + 1L)     # the column for age x + n
  Cx   <- MA2 * exp(H[, x + 1L] - matrix(H[at_n], k, N_CLASS)) * matrix(ex[at_n], k, N_CLASS) * vec_comp
  Cx[!is.finite(Cx)] <- 0
  if (is.null(r)) return(rowMeans(Cx[, 4:7, drop = FALSE]))
  w <- lx[, seq_len(N_CLASS)] * exp(-outer(r, x))
  w <- w / rowSums(w)
  w[col(w) < round(sigma) + 1] <- 0                             # too young to have taken a first bite
  rowSums(Cx * w)
}

# One simulation: draw the parameter sets, then run the age-specific model for each trial.
# `specs` is a named list of assumption specs for the assumptions in use (including growth_r and
# first_bite when the age structure is stable). Same code path for the app and the tests.
# Trials are run in blocks of `chunk` so memory stays small and progress can be reported.
run_model <- function(model, structure, specs, n, seed, pairs = NULL, uploaded = NULL,
                      progress = function(done, n) {}, chunk = 250L) {
  # Seeding makes the run reproducible, but leave the session's random stream as it was, so other
  # random choices (the shuffle-seed button) are not fixed by the seed of the last run
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv())
  on.exit(if (is.null(old)) suppressWarnings(rm(".Random.seed", envir = globalenv()))
          else assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  set.seed(seed)
  g <- generate_draws(specs, n, pairs, uploaded)
  d <- g$draws
  d$n_eip <- pmin(pmax(round(d$n_eip), 1), 150)
  ids <- names(specs)
  b_i <- if ("mort_b" %in% ids) d$mort_b else rep(0, n)
  s_i <- if ("mort_s" %in% ids) d$mort_s else rep(0, n)
  stable <- identical(structure, "stable")
  if (stable) {
    r_i   <- d$growth_r
    sig_i <- pmin(pmax(round(d$first_bite), 0), N_CLASS - 1)
  }
  ct <- numeric(n)
  for (from in seq(1L, n, by = chunk)) {
    i <- from:min(from + chunk - 1L, n)
    ct[i] <- ct_batch(model, d$mort_a[i], b_i[i], s_i[i], d$n_eip[i], d$m_dens[i] * d$a_bite[i]^2,
                      d$vec_comp[i], if (stable) r_i[i], if (stable) sig_i[i])
    progress(max(i), n)
  }
  list(ct = ct, draws = d, b = b_i, s = s_i, notes = g$notes, achieved = g$achieved)
}

# Partial rank correlation coefficients (PRCC): the rank correlation between each input and the output
# after removing the linear effects of all the other inputs (on ranks). 95% intervals use Fisher's z.
prcc_calc <- function(X, y) {
  R <- as.data.frame(lapply(X, rank)); ry <- rank(y); k <- ncol(R); n <- nrow(R)
  est <- vapply(seq_len(k), function(j) {
    others <- as.matrix(R[, -j, drop = FALSE])
    if (k == 1) return(cor(R[[j]], ry))
    ex <- lm.fit(cbind(1, others), R[[j]])$residuals
    ey <- lm.fit(cbind(1, others), ry)$residuals
    suppressWarnings(cor(ex, ey))
  }, 0)
  names(est) <- names(X)
  z  <- atanh(pmin(pmax(est, -0.9999), 0.9999)); se <- 1 / sqrt(max(n - 3 - (k - 1), 1))
  data.frame(id = names(X), est = est, lo = tanh(z - 1.96 * se), hi = tanh(z + 1.96 * se), row.names = NULL)
}

# ---- Plots and summaries shared by the screen and the downloadable report ----
sens_contrib <- function(res) {
  d      <- res$draws
  varied <- names(d)[sapply(d, function(v) sd(v) > 0)]
  if (length(varied) == 0 || sd(res$ct) == 0) return(NULL)
  rho <- sapply(varied, function(v) cor(d[[v]], res$ct, method = "spearman"))
  list(rho = rho, contrib = sort(100 * sign(rho) * rho^2 / sum(rho^2)),
       prcc = prcc_calc(d[varied], res$ct))
}

draw_sens <- function(res, metric = "prcc", sc = sens_contrib(res)) {
  if (is.null(sc)) {
    plot.new(); text(0.5, 0.5, "No assumptions are varying, so there is nothing to rank")
    return(invisible())
  }
  par(mar = c(5, 17, 3, 2))
  if (identical(metric, "prcc")) {
    pr   <- sc$prcc[order(sc$prcc$est), ]
    vals <- setNames(pr$est, pr$id)
    xl   <- range(c(0, pr$lo, pr$hi)); xl <- xl + c(-1, 1) * 0.12 * diff(xl)
    mp <- barplot(vals, horiz = TRUE, las = 1, names.arg = labels[pr$id], xlim = xl,
                  col = ifelse(vals > 0, "steelblue", "#D55E00"),
                  xlab = "Partial rank correlation (PRCC) with Ct, with 95% interval",
                  main = "Sensitivity of Ct to each assumption")
    arrows(pr$lo, mp, pr$hi, mp, angle = 90, code = 3, length = 0.04, lwd = 1.2)
    text(ifelse(vals > 0, pr$hi, pr$lo), mp, sprintf("%.2f", vals), pos = ifelse(vals > 0, 4, 2), cex = 0.8, xpd = NA)
    abline(v = 0)
    return(invisible())
  }
  vals <- if (identical(metric, "rho")) sort(sc$rho) else sc$contrib
  xl <- range(c(0, vals)); xl <- xl + c(-1, 1) * 0.2 * diff(xl)
  mp <- barplot(vals, horiz = TRUE, las = 1, names.arg = labels[names(vals)], xlim = xl,
                col = ifelse(vals > 0, "steelblue", "#D55E00"),
                xlab = if (identical(metric, "rho")) "Rank correlation with Ct (Spearman rho)" else "Share of squared rank correlation (%)",
                main = "Sensitivity of Ct to each assumption")
  text(vals, mp, if (identical(metric, "rho")) sprintf("%.2f", vals) else sprintf("%.1f%%", vals),
       pos = ifelse(vals > 0, 4, 2), cex = 0.8, xpd = NA)
  abline(v = 0)
}

# One assumption's drawn values. The small version is a tile on the page; the large version adds
# the median and 95% range, and the distribution that was used.
draw_one <- function(x, label, big = FALSE, spec = NULL) {
  if (diff(range(x)) == 0) {                       # fixed: every trial used the same value
    par(mar = if (big) c(5, 5, 6, 2) else c(3, 3, 3, 1))
    w <- max(abs(x[1]) * 0.1, 0.01)
    plot(NA, xlim = x[1] + c(-w, w), ylim = c(0, 1), yaxt = "n", xlab = if (big) label else "", ylab = "",
         main = label, cex.main = if (big) 1.3 else 0.95)
    segments(x[1], 0, x[1], 0.8, lwd = 4, col = "steelblue")
    text(x[1], 0.9, sprintf("Fixed at %s", fmt3(x[1])), cex = if (big) 1 else 0.8)
    return(invisible())
  }
  if (!big) {
    par(mar = c(3, 3.2, 3, 1), mgp = c(1.8, 0.6, 0), cex.main = 0.95)
    hist(x, breaks = 30, col = "grey70", border = "white", main = label, xlab = "", ylab = "Frequency")
    return(invisible())
  }
  par(mar = c(5, 5, 6, 2))
  hist(x, breaks = 50, col = "grey70", border = "white", main = "", xlab = label, ylab = "Number of trials")
  title(main = label, line = 3.6, cex.main = 1.3)
  if (!is.null(spec)) mtext(spec, side = 3, line = 1.6, cex = 0.85, col = "grey30")
  q <- quantile(x, c(0.025, 0.5, 0.975))
  abline(v = q[2], lwd = 2, lty = 2); abline(v = q[c(1, 3)], lwd = 1.5, lty = 3, col = "#b30000")
  legend("topright", bty = "n", lty = c(2, 3), lwd = c(2, 1.5), col = c("black", "#b30000"),
         legend = c(sprintf("Median %s", fmt3(q[2])), sprintf("2.5th and 97.5th percentiles: %s and %s", fmt3(q[1]), fmt3(q[3]))))
}

draw_draws <- function(d) {
  par(mfrow = c(ceiling(ncol(d) / 3), 3))
  for (v in names(d))
    hist(d[[v]], breaks = 40, col = "grey70", border = "white", main = labels[[v]], xlab = "")
}

draw_surv <- function(res) {
  k   <- seq_len(min(100, nrow(res$draws)))
  x   <- 0:80
  lts <- lapply(k, function(i)
    life_table(hazard(res$model, AGES, res$draws$mort_a[i], res$b[i], res$s[i])))
  par(mfrow = c(1, 2))
  plot(NA, xlim = range(x), ylim = c(0, 1), xlab = "Age (days)", ylab = "Survivorship lx",
       main = "Survivorship, first 100 trials")
  for (lt in lts) lines(x, lt$lx[x + 1], col = rgb(0, 0, 0, 0.15))
  ymax <- max(sapply(lts, function(lt) max(lt$u[x + 1])))
  plot(NA, xlim = range(x), ylim = c(0, ymax), xlab = "Age (days)", ylab = "Hazard ux",
       main = "Hazard, first 100 trials")
  for (lt in lts) lines(x, lt$u[x + 1], col = rgb(0.7, 0.1, 0.1, 0.15))
}

stats_df <- function(ct) {
  q <- quantile(ct, c(0.025, 0.10, 0.25, 0.50, 0.75, 0.90, 0.975))
  data.frame(
    Statistic = c("Trials", "Mean", "SD", "Min", "Max",
                  "2.5th percentile", "10th percentile", "25th percentile", "Median",
                  "75th percentile", "90th percentile", "97.5th percentile"),
    Value = c(length(ct), mean(ct), sd(ct), min(ct), max(ct), q))
}

fmt3 <- function(x) format(signif(x, 3), nsmall = 0)

# Headline sentence and simulation-precision sentence for a set of Ct values
summary_text <- function(ct) {
  if (diff(range(ct)) == 0)
    return(list(fixed = TRUE,
                headline = sprintf("Every trial gives the same Ct of %s, because all assumptions are fixed.", fmt3(ct[1])),
                precision = NULL))
  q <- quantile(ct, c(0.025, 0.5, 0.975))
  n <- length(ct); srt <- sort(ct)
  lo <- max(qbinom(0.025, n, 0.5), 1); hi <- min(qbinom(0.975, n, 0.5) + 1, n)   # 95% interval for the median
  hw <- (srt[hi] - srt[lo]) / 2
  list(fixed = FALSE,
       median = fmt3(q[2]),
       rest = sprintf("95%% of trials fall between %s and %s (mean %s, %s trials).",
                      fmt3(q[1]), fmt3(q[3]), fmt3(mean(ct)), format(n, big.mark = ",")),
       precision = sprintf("Simulation precision: the median is accurate to about \u00b1%s (95%% confidence). More trials tighten this.",
                           signif(hw, 2)))
}

# One-line plain-language notes for each distribution choice
dist_help <- c(
  Fixed      = "A single value; every trial uses it.",
  Uniform    = "Every value between min and max is equally likely.",
  Triangular = "Peaks at the likeliest value and falls in straight lines to min and max.",
  PERT       = "Like triangular but smoother; the likeliest value carries more weight.",
  Beta       = "Bounded between min and max. The two shapes set where the peak sits (equal shapes give a symmetric hump).",
  Normal     = "Bell curve around the mean with the given SD, cut off at min and max.",
  Lognormal  = "Skewed to the right, positive values only, with the given mean and SD, cut off at min and max.")

# Soft warnings: values that will run but may not mean what the user intends
soft_warning <- function(id, s) {
  lo <- if (isTRUE(s$dist == "Fixed")) s$value else s$min
  hi <- if (isTRUE(s$dist == "Fixed")) s$value else s$max
  msg <- character(0)
  if (id == "vec_comp" && (isTRUE(lo < 0) || isTRUE(hi > 1)))
    msg <- c(msg, "Vector competence is a probability, so values outside 0 to 1 are not meaningful.")
  if (id == "first_bite" && (isTRUE(lo < 0) || isTRUE(hi > N_CLASS - 1)))
    msg <- c(msg, sprintf("Ages are rounded to whole days and kept between 0 and %d.", N_CLASS - 1))
  if (id == "n_eip") {
    if (isTRUE(lo < 1))   msg <- c(msg, "Incubation periods under 1 day are rounded up to 1.")
    if (isTRUE(hi > 150)) msg <- c(msg, "Incubation periods over 150 days are capped at 150.")
  }
  if (isTRUE(s$dist %in% c("Normal", "Lognormal")) &&
      (isTRUE(s$mean < s$min) || isTRUE(s$mean > s$max)))
    msg <- c(msg, "The mean lies outside min and max, so the truncated distribution is centred elsewhere.")
  paste(msg, collapse = " ")
}

# Base64 text for a raw vector (the report embeds figures as data URIs)
b64_encode <- function(raw) jsonlite::base64_enc(raw)

# Draw to a temporary PNG and return it as base64, or NULL if this R cannot make PNG files
png_b64 <- function(drawer, w, h) {
  tmp <- tempfile(fileext = ".png")
  opened <- tryCatch({ png(tmp, width = w, height = h, res = 110); TRUE },
                     error = function(e) FALSE, warning = function(w) FALSE)
  if (!opened) return(NULL)
  tryCatch(drawer(), error = function(e) NULL)
  dev.off()
  if (!file.exists(tmp) || file.size(tmp) == 0) return(NULL)
  raw <- readBin(tmp, "raw", file.size(tmp)); unlink(tmp)
  b64_encode(raw)
}

html_table <- function(df) {
  esc  <- htmltools::htmlEscape
  head <- paste0("<tr>", paste0("<th>", esc(names(df)), "</th>", collapse = ""), "</tr>")
  rows <- vapply(seq_len(nrow(df)), function(i)
    paste0("<tr>", paste0("<td>", esc(vapply(df[i, ], function(x) if (is.numeric(x)) fmt3(x) else as.character(x), "")),
                          "</td>", collapse = ""), "</tr>"), "")
  paste0("<table>", head, paste(rows, collapse = ""), "</table>")
}

# Self-contained HTML report for one run: summary, settings, statistics and figures
# Comparison for the paper: Styer et al. (2007) published values, the deterministic model at their parameter
# values, a probabilistic run in which every assumption in use is Uniform within +/- `spread` of that value,
# and a probabilistic run with the literature-based distributions (the app's default preset, where the
# growth rate and first-bite age have no published range and stay fixed).
paper_comparison <- function(n = 10000, seed = 2026, spread = 0.2, step = function(i, k) {}) {
  combos <- expand.grid(structure = c("synchronous", "stable"), model = names(styer_pars), stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(combos)), function(i) {
    m <- combos$model[i]; st <- combos$structure[i]
    ids  <- c(names(vc_specs), "mort_a", if (m != "exponential") "mort_b", if (m == "logistic") "mort_s",
              if (st == "stable") names(pop_specs))
    base <- c(vc_specs, mort_specs[[m]], pop_specs)[ids]
    fixed <- lapply(base, function(x) { x$dist <- "Fixed"; x })
    prob  <- lapply(base, function(x) { x$dist <- "Uniform"; x$min <- x$value * (1 - spread); x$max <- x$value * (1 + spread); x })
    lit   <- Map(function(x, id) { x$dist <- lit_dists[[id]]; x }, base, names(base))
    det <- run_model(m, st, fixed, 2, seed)$ct[1]
    ct  <- run_model(m, st, prob, n, seed)$ct
    ctl <- run_model(m, st, lit, n, seed)$ct
    uni <- sprintf("Uniform +/-%g%%", 100 * spread)
    pub <- validation[[if (st == "stable") "Ct stable (published)" else "Ct synchronous (published)"]][validation$Model == m]
    step(i, nrow(combos))
    out <- data.frame(`Mortality model` = m, `Age structure` = st, `Styer et al. (published)` = pub,
                      Deterministic = det, `Difference from published (%)` = 100 * (det - pub) / pub,
                      check.names = FALSE)
    for (run in list(list(uni, ct), list("Literature-based", ctl)))
      out[paste(run[[1]], c("median", "mean", "2.5th percentile", "97.5th percentile"))] <-
        list(median(run[[2]]), mean(run[[2]]), unname(quantile(run[[2]], 0.025)), unname(quantile(run[[2]], 0.975)))
    out
  })
  do.call(rbind, rows)
}

# Figure for the comparison table: for each mortality model, the published value (diamond), the deterministic
# value (dot) and the median with 95% range of the two probabilistic runs. One panel per age structure.
draw_paper <- function(tbl) {
  uni <- sub(" median$", "", grep("^Uniform .* median$", names(tbl), value = TRUE)[1])
  runs <- list(list(label = uni,                nudge =  0.2, col = adjustcolor("#E69F00", 0.75)),
               list(label = "Literature-based", nudge = -0.2, col = adjustcolor("steelblue", 0.75)))
  mods <- names(styer_pars)
  nice <- c(exponential = "Exponential", gompertz = "Gompertz", logistic = "Logistic")
  layout(matrix(c(1, 3, 2, 3), 2, 2), heights = c(1, 0.16))      # two panels above a strip for the legend
  for (st in c("synchronous", "stable")) {
    d <- tbl[tbl[["Age structure"]] == st, ]; d <- d[match(mods, d[["Mortality model"]]), ]
    y <- rev(seq_along(mods))
    xmax <- max(d[["Deterministic"]], unlist(lapply(runs, function(r) d[[paste(r$label, "97.5th percentile")]])))
    par(mar = c(4.2, 6, 2.6, 1), mgp = c(2.4, 0.7, 0))
    plot(NA, xlim = c(0, 1.04 * xmax), ylim = c(0.5, length(mods) + 0.5), yaxt = "n",
         xlab = "Total vectorial capacity (Ct)", ylab = "",
         main = if (st == "synchronous") "Synchronous emergence" else "Stable age distribution")
    axis(2, at = y, labels = nice[d[["Mortality model"]]], las = 1, tick = FALSE)
    abline(h = y, col = "grey92", lwd = 6)
    for (r in runs) {
      segments(d[[paste(r$label, "2.5th percentile")]], y + r$nudge, d[[paste(r$label, "97.5th percentile")]], y + r$nudge,
               lwd = 9, col = r$col, lend = 1)
      md <- d[[paste(r$label, "median")]]
      segments(md, y + r$nudge - 0.1, md, y + r$nudge + 0.1, lwd = 2.5, col = "white")
    }
    points(d[["Styer et al. (published)"]], y, pch = 5, cex = 1.7, lwd = 1.6)
    points(d[["Deterministic"]], y, pch = 16, cex = 0.8)
  }
  par(mar = c(0, 0, 0, 0)); plot.new()
  legend("center", horiz = TRUE, bty = "n", cex = 0.9, x.intersp = 0.6,
         legend = c("Published (Styer et al.)", "Deterministic",
                    paste0(uni, ": median, 95% range"), "Literature-based: median, 95% range"),
         pch = c(5, 16, 15, 15), col = c("black", "black", runs[[1]]$col, runs[[2]]$col), pt.cex = c(1.5, 1, 1.8, 1.8))
}

# The comparison never changes (fixed trials, seed and spread), so a copy computed ahead of time ships in
# www/ and loads instantly. deploy.R rebuilds it, and the tests fail if it no longer matches the model.
# If the copy is missing or unreadable, the table is computed instead.
PAPER_CACHE <- "www/pvec_comparison.csv"
paper_table <- function(step = function(i, k) {}, cache = PAPER_CACHE) {
  r <- if (file.exists(cache)) tryCatch(read.csv(cache, check.names = FALSE, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.data.frame(r) && nrow(r) == 6 && ncol(r) == 13) return(r)
  paper_comparison(step = step)
}

build_report <- function(res, bounds, prev = NULL, run_labels = NULL, thresh = NA) {
  esc <- htmltools::htmlEscape
  st  <- summary_text(res$ct)
  head_html <- if (st$fixed) esc(st$headline) else sprintf("<b>Median Ct %s</b>. %s", st$median, esc(st$rest))
  setting_lines <- res$settings[-1]
  kv <- do.call(rbind, lapply(setting_lines, function(l) {
    i <- regexpr(": ", l, fixed = TRUE)
    data.frame(Setting = substr(l, 1, i - 1), Value = substring(l, i + 2))
  }))
  figs <- list(
    list("Forecast of total vectorial capacity", function() draw_forecast(res$ct, bounds, prev$ct, run_labels, thresh), 1000, 600,
         "Histogram of total vectorial capacity (Ct) across all trials."),
    list("Sensitivity", function() draw_sens(res), 1000, 600,
         "Bar chart of the partial rank correlation (PRCC) between each assumption and Ct, with 95% intervals."),
    list("Assumption draws", function() draw_draws(res$draws), 1000, 800,
         "Histograms of the values drawn for each assumption."),
    list("Survival curves", function() draw_surv(res), 1100, 500,
         "Survivorship and daily mortality hazard for the first 100 trials."))
  fig_html <- vapply(figs, function(f) {
    b64 <- png_b64(f[[2]], f[[3]], f[[4]])
    paste0("<h2>", esc(f[[1]]), "</h2>",
           if (is.null(b64)) "<p><i>Figure not available in this environment. Use the Download plot buttons in the app.</i></p>"
           else sprintf("<img alt=\"%s\" src=\"data:image/png;base64,%s\">", esc(f[[5]]), b64))
  }, "")
  paste0(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'><title>Vectorial capacity report</title><style>",
    "body{font-family:Helvetica,Arial,sans-serif;max-width:900px;margin:30px auto;padding:0 16px;color:#222;line-height:1.45}",
    "h1{font-size:24px;margin-bottom:4px}h2{font-size:18px;margin-top:30px;border-bottom:1px solid #ddd;padding-bottom:4px}",
    "table{border-collapse:collapse;font-size:14px;margin:8px 0}td,th{border:1px solid #ddd;padding:4px 10px;text-align:left}",
    "th{background:#f3f5f7}img{max-width:100%;height:auto}.muted{color:#666;font-size:13px}</style></head><body>",
    "<h1>PVEC report</h1>",
    "<p class='muted'>", esc(res$settings[1]), ". Run ", res$run, ", created ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ".</p>",
    "<h2>Summary</h2><p>", head_html, "</p>",
    if (!is.null(st$precision)) paste0("<p class='muted'>", esc(st$precision), "</p>") else "",
    "<h2>Settings used</h2>", html_table(kv),
    "<h2>Statistics</h2>", html_table(stats_df(res$ct)),
    paste(fig_html, collapse = ""),
    "<h2>Sources</h2><ul>",
    "<li>Macdonald G (1957) The Epidemiology and Control of Malaria. Oxford University Press.</li>",
    "<li>Garrett-Jones C (1964) Nature 204:1173-1175. https://doi.org/10.1038/2041173a0</li>",
    "<li>Styer LM, Carey JR, Wang J-L, Scott TW (2007) Am J Trop Med Hyg 76:111-117. https://doi.org/10.4269/ajtmh.2007.76.111</li>",
    "</ul></body></html>")
}

# Which fields matter for each distribution, and whether an assumption differs from its preset value
relevant_fields <- list(
  Fixed = "value", Uniform = c("min", "max"), Triangular = c("min", "mode", "max"),
  PERT = c("min", "mode", "max"), Beta = c("min", "max", "shape1", "shape2"),
  Normal = c("mean", "sd", "min", "max"), Lognormal = c("mean", "sd", "min", "max"))

same_spec <- function(a, b) {
  if (!identical(a$dist, b$dist) || is.null(relevant_fields[[a$dist]])) return(FALSE)
  all(vapply(relevant_fields[[a$dist]], function(f) {
    x <- a[[f]]; y <- b[[f]]
    length(x) == 1 && length(y) == 1 && isTRUE(all.equal(as.numeric(x), as.numeric(y)))
  }, logical(1)))
}

# Copy-to-clipboard button for a table output (handled in the page script)
copy_btn <- function(target)
  tags$button(type = "button", class = "btn btn-default btn-sm fade-btn copy-table", `data-target` = target,
              span(class = "fb-a", icon("copy"), " Copy table"),
              span(class = "fb-b", `aria-live` = "polite"))

# One numbered step in the Getting started box
howto_step <- function(n, title, ...)
  div(class = "howto-step",
      span(class = "howto-num", n),
      div(div(class = "howto-step-title", title), div(class = "howto-step-text", ...)))

# Small button that saves a plot's current image as a PNG (handled in the page script)
dl_png <- function(target, file)
  tags$button(type = "button", class = "btn btn-default btn-sm dl-img",
              `data-target` = target, `data-file` = file, icon("download"), " Download plot (PNG)")

# Tiny preview of one assumption's distribution, drawn inside its card
preview_plot <- function(s) {
  par(mar = c(1.6, 0.4, 0.2, 0.4), mgp = c(1, 0.25, 0), tcl = -0.2, cex.axis = 0.75)
  if (s$dist == "Fixed") {
    w <- max(abs(s$value) * 0.15, 0.01)
    plot(NA, xlim = s$value + c(-w, w), ylim = c(0, 1), yaxt = "n", xlab = "", ylab = "", bty = "n")
    segments(s$value, 0, s$value, 1, lwd = 3, col = "steelblue")
  } else if (s$dist == "Uniform") {
    plot(NA, xlim = c(s$min, s$max), ylim = c(0, 1.25), yaxt = "n", xlab = "", ylab = "", bty = "n")
    polygon(c(s$min, s$min, s$max, s$max), c(0, 1, 1, 0),
            col = adjustcolor("steelblue", 0.5), border = "steelblue")
  } else {
    x  <- qdraw(ppoints(1000), s)     # evenly spaced quantiles: a stable shape, and no random numbers used
    dn <- density(x, from = min(x), to = max(x), adjust = 1.3)
    plot(dn$x, dn$y, type = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
    polygon(c(dn$x[1], dn$x, tail(dn$x, 1)), c(0, dn$y, 0),
            col = adjustcolor("steelblue", 0.5), border = "steelblue")
  }
}

# ---- Assumption box UI ----
shows <- function(id, dists)
  sprintf("['%s'].indexOf(input.%s_dist) > -1", paste(dists, collapse = "','"), id)

# Size of one arrow-key or spinner step, matched to each assumption's scale
steps <- c(a_bite = 0.01, n_eip = 1, m_dens = 0.01, vec_comp = 0.01,
           mort_a = 0.0001, mort_b = 0.001, mort_s = 0.01, growth_r = 0.01, first_bite = 1)

num <- function(id, f, label, s)
  numericInput(paste0(id, "_", f), label, s[[f]], width = "100%",
               step = if (f %in% c("shape1", "shape2")) 0.1 else steps[[id]])

# Open on the literature-based distributions rather than fixed point values
with_default_dist <- function(id, s) { s$dist <- lit_dists[[id]]; s }

assumption_ui <- function(id, s) {
  s <- with_default_dist(id, s)
  wellPanel(class = "assump-card", id = paste0("card_", id),
    div(class = "assump-head",
      tags$button(type = "button", class = "assump-toggle", `aria-expanded` = "false",
                  `aria-controls` = paste0("body_", id),
                  span(class = "chev", icon("chevron-right")), strong(labels[[id]])),
      div(class = "edited-tools", span(class = "edited-badge", "not run yet"),
          actionLink(paste0(id, "_reset"), "reset", title = "Reset to the preset's values"))),
    div(class = "assump-summary", id = paste0(id, "_summary")),
    plotOutput(paste0(id, "_prev"), height = "48px"),
    div(class = "assump-upload", id = paste0(id, "_up")),
    div(class = "assump-warn", id = paste0(id, "_warn")),
    div(class = "assump-err", id = paste0(id, "_err")),
    div(class = "assump-body", id = paste0("body_", id),
     div(class = "assump-body-clip", div(class = "assump-body-pad",
      div(class = "assump-desc", descs[[id]]),
      if (id %in% names(card_notes)) div(class = "assump-note", card_notes[[id]]),
      selectInput(paste0(id, "_dist"), "Distribution", dist_choices, s$dist),
      div(class = "dist-help", lapply(dist_choices, function(d)
        conditionalPanel(shows(id, d), dist_help[[d]]))),
      conditionalPanel(shows(id, "Fixed"), num(id, "value", "Value", s)),
      conditionalPanel(shows(id, setdiff(dist_choices, "Fixed")),
        fluidRow(column(6, num(id, "min", "Min", s)), column(6, num(id, "max", "Max", s)))),
      conditionalPanel(shows(id, c("Triangular", "PERT")), num(id, "mode", "Likeliest", s)),
      conditionalPanel(shows(id, "Beta"),
        fluidRow(column(6, num(id, "shape1", "Shape 1", s)), column(6, num(id, "shape2", "Shape 2", s)))),
      conditionalPanel(shows(id, c("Normal", "Lognormal")),
        fluidRow(column(6, num(id, "mean", "Mean", s)), column(6, num(id, "sd", "SD", s)))),
      conditionalPanel(shows(id, c("Uniform", "Normal", "Lognormal", "Beta")),
        tags$details(class = "fit-help",
          tags$summary("Fit from a reported range"),
          div(class = "fit-note", "Enter a published 95% interval, and the mean if reported, then fit. This fills in the fields above."),
          fluidRow(column(6, numericInput(paste0(id, "_fit_lo"), "Lower 95%", NA, width = "100%", step = steps[[id]])),
                   column(6, numericInput(paste0(id, "_fit_hi"), "Upper 95%", NA, width = "100%", step = steps[[id]]))),
          numericInput(paste0(id, "_fit_est"), "Mean (optional)", NA, width = "100%", step = steps[[id]]),
          actionButton(paste0(id, "_fit"), "Fit", class = "btn-sm btn-default"),
          uiOutput(paste0(id, "_fitmsg")))),
      textInput(paste0(id, "_source"), "Source", "", width = "100%", placeholder = "e.g. Author (year), table 2")
      ))))
}

# Small helpers for writing equations as HTML strings (no external math library needed)
h       <- function(...) paste0(unlist(list(...)), collapse = "")
v       <- function(x) paste0("<i>", x, "</i>")
sup_    <- function(...) paste0("<sup>", h(...), "</sup>")
sub_    <- function(...) paste0("<sub>", h(...), "</sub>")
b_      <- function(...) paste0("<b>", h(...), "</b>")
eq_frac <- function(num, den) paste0("<span class='fr'><span>", h(num), "</span><span>", h(den), "</span></span>")
eq_sum  <- function(lo, hi) paste0("<span class='sm'><span>", h(hi), "</span>Σ<span>", h(lo), "</span></span>")
eq      <- function(...) HTML(paste0("<div class='eq'>", h(...), "</div>"))
P       <- function(..., class = NULL)
  HTML(paste0("<p", if (!is.null(class)) paste0(" class='", class, "'"), ">", h(...), "</p>"))

# A chip in the jump bar at the top of Define assumptions
jump_chip <- function(target, text)
  tags$button(type = "button", class = "jump-chip", `data-target` = target, text, span(class = "jump-count"))


# ---- UI ----
ui <- fluidPage(
  titlePanel(div(class = "app-brand",
                 tags$img(src = "pvec_logo.svg", alt = "PVEC", class = "app-logo"),
                 span(class = "app-brand-text", "Probabilistic Vectorial Capacity Simulator")),
             windowTitle = "PVEC: Probabilistic Vectorial Capacity Simulator"),
  tags$head(tags$link(rel = "icon", type = "image/svg+xml", href = "pvec_icon.svg")),
  tags$button(id = "expand_sidebar", type = "button", class = "sidebar-arrow-open",
              title = "Show settings", `aria-label` = "Show settings", `aria-expanded` = "false",
              icon("chevron-right")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "pvec.css")),
  div(class = "print-only", textOutput("run_status_print")),
  sidebarLayout(
    sidebarPanel(width = 3,
      div(class = "settings-head",
        h4("Simulation settings", style = "margin: 0;"),
        tags$button(id = "collapse_sidebar", type = "button", class = "sidebar-arrow",
                    title = "Hide settings", `aria-label` = "Hide settings", `aria-expanded` = "true",
                    icon("chevron-left"))),
      selectInput("mort_model", "Mortality model",
                  c("Logistic" = "logistic", "Gompertz" = "gompertz", "Exponential" = "exponential")),
      selectInput("structure", "Population age structure",
                  c("Stable age distribution" = "stable", "Synchronous emergence" = "synchronous")),
      fluidRow(
        column(6, selectInput("n_iter", "Trials",
                              c("500" = 500, "1,000" = 1000, "5,000" = 5000, "10,000 (slow)" = 10000), 1000)),
        column(6, numericInput("seed", HTML(paste0("Seed ", as.character(actionLink("rand_seed", icon("shuffle"), title = "Pick a random seed")))), 1))),
      radioButtons("preset", "Load a preset",
        choices = c("Literature-based distributions" = "lit",
                    "Fixed point estimates (deterministic)" = "fixed"),
        selected = "lit"),
      conditionalPanel("input.preset == 'lit'",
        p(class = "preset-desc",
          HTML(paste0(
            "Each assumption is drawn from a probability distribution whose range comes from ",
            "published estimates (see ", as.character(actionLink("goto_about", "About")), "). ",
            "Biting rate and vector competence use Beta, incubation period and mosquito density ",
            "use Uniform, and the mortality parameters use truncated Normal. Results vary from ",
            "trial to trial, so the forecast shows a distribution of Ct.")))),
      conditionalPanel("input.preset == 'fixed'",
        p(class = "preset-desc",
          "Every assumption is set to a single value, so every trial gives the same Ct. Use",
          "this as a deterministic baseline to see how much the uncertainty changes the result.",
          "You can still change any distribution by hand.")),
      div(class = "preset-reset", actionLink("reset_preset", "Reset all values to this preset")),
      actionButton("run", "Run simulation", class = "btn-primary", width = "100%",
                   title = "Shortcut: Cmd or Ctrl + Enter"),
      uiOutput("stale_note"),
      div(class = "run-status", textOutput("run_status")),
      div(class = "start-over", actionLink("start_over", "Start over (defaults, clear run history)"))
    ),
    mainPanel(width = 9,
      tabsetPanel(id = "tabs",
        tabPanel("Define assumptions",
          div(class = "jump-bar", id = "jump_bar", role = "navigation", `aria-label` = "Jump to a section",
            jump_chip("sec_transmission", "Transmission"),
            jump_chip("sec_mortality", "Mortality"),
            jump_chip("sec_population", "Population"),
            jump_chip("sec_linking", "Linking"),
            div(class = "jump-actions",
              tags$button(type = "button", id = "expand_all", class = "link-btn", "Expand all"),
              span(class = "jump-sep", "|"),
              tags$button(type = "button", id = "collapse_all", class = "link-btn", "Collapse all"))),
          div(class = "howto", id = "howto",
            div(class = "howto-title", "Getting started"),
            div(class = "howto-steps",
              howto_step(1, "Choose assumptions", "Pick a preset in Simulation settings, or open the cards below to edit them."),
              howto_step(2, "Set up the run", "Choose the mortality model, number of trials and a seed."),
              howto_step(3, "Run and read", "Click ", strong("Run simulation"), " (or press ",
                         tags$kbd("Cmd/Ctrl + Enter"), "), then open the Forecast tab.")),
            div(class = "howto-note", span(class = "legend-dot"),
                "A purple dot on a tab means it has new results you have not looked at yet."),
            tags$button(type = "button", class = "howto-close", `aria-label` = "Dismiss", HTML("&times;"))),
          tags$button(type = "button", id = "howto_open", class = "link-btn howto-open",
                      icon("circle-info"), " Getting started"),
          div(class = "settings-io",
            downloadButton("dl_settings", "Save settings", class = "btn-sm"),
            tags$button(id = "copy_link", type = "button", class = "btn btn-default btn-sm fade-btn",
                        title = "Copies a link that restores these settings",
                        span(class = "fb-a", icon("share-nodes"), " Share settings"),
                        span(class = "fb-b", `aria-live` = "polite",
                             span(class = "fb-icon", icon("link")), span(class = "fb-msg"))),
            fileInput("load_settings", NULL, buttonLabel = tagList(icon("upload"), " Upload settings"), placeholder = "",
                      accept = ".csv", width = "auto")),
          div(class = "assump-section", id = "sec_transmission",
            h4("Transmission"),
            div(class = "cards-grid", lapply(names(vc_specs), function(id) assumption_ui(id, vc_specs[[id]])))),
          div(class = "assump-section", id = "sec_mortality",
            h4("Mortality schedule"),
            div(class = "cards-grid",
              assumption_ui("mort_a", mort_specs$logistic$mort_a),
              conditionalPanel("input.mort_model != 'exponential'", assumption_ui("mort_b", mort_specs$logistic$mort_b)),
              conditionalPanel("input.mort_model == 'logistic'", assumption_ui("mort_s", mort_specs$logistic$mort_s)))),
          div(class = "assump-section", id = "sec_population",
            h4("Population age structure"),
            div(class = "cards-grid",
              assumption_ui("growth_r", pop_specs$growth_r),
              assumption_ui("first_bite", pop_specs$first_bite))),
          div(class = "assump-section", id = "sec_linking",
            tags$button(id = "link_toggle", type = "button", class = "adv-toggle", `aria-expanded` = "false",
                        `aria-controls` = "link_body",
                        span(class = "chev", icon("chevron-right")), strong("Linking assumptions"),
                        span(class = "adv-tag", "advanced"), span(id = "link_status", class = "adv-status")),
            div(class = "adv-body", id = "link_body", div(class = "adv-clip", div(class = "link-box",
              p(class = "assump-desc",
                "By default every assumption is drawn on its own. If assumptions move together, for example",
                "mortality a and b estimated from the same data, linking them changes how wide the forecast is.",
                "Set a rank correlation between a pair, or upload joint parameter draws (such as posterior",
                "samples from a fitted model) that already carry the correlations."),
              fluidRow(
                column(3, selectInput("corr_a", "Assumption A", setNames(all_setting_ids, labels[all_setting_ids]))),
                column(3, selectInput("corr_b", "Assumption B", setNames(all_setting_ids, labels[all_setting_ids]),
                                      selected = all_setting_ids[2])),
                column(3, numericInput("corr_rho", "Rank correlation (-0.95 to 0.95)", 0, min = -0.95, max = 0.95, step = 0.05)),
                column(3, div(class = "corr-add", actionButton("corr_add", "Add correlation", class = "btn-default btn-sm")))),
              uiOutput("corr_list"),
              tags$hr(),
              div(class = "settings-io",
                fileInput("up_file", NULL, buttonLabel = "Upload parameter draws (CSV)", placeholder = "",
                          accept = ".csv", width = "auto"),
                downloadButton("dl_template", "Download template", class = "btn-sm")),
              uiOutput("up_status")))))),
        tabPanel("Forecast",
          uiOutput("forecast_summary"),
          div(class = "no-print",
            selectInput("compare_run", "Compare with an earlier run",
                        c("None (current run only)" = "none"), width = "420px")),
          uiOutput("overlay_note"),
          uiOutput("compare_table"),
          div(class = "dl-row",
            dl_png("forecast_plot", "forecast_Ct.png"),
            downloadButton("dl_csv", "Download results (CSV)", class = "btn-sm"),
            downloadButton("dl_report", "Download report (HTML)", class = "btn-sm")),
          fluidRow(
            column(3, numericInput("cert_lo", "Certainty range, lower", NA, step = 0.1)),
            column(3, numericInput("cert_hi", "Certainty range, upper", NA, step = 0.1)),
            column(3, numericInput("thresh", "Chance Ct exceeds", NA, step = 0.1))),
          uiOutput("cert_text"),
          plotOutput("forecast_plot", height = 400),
          div(class = "table-tools", copy_btn("stats")),
          tableOutput("stats")),
        tabPanel("Sensitivity",
          helpText("Which assumptions move Ct the most? The default, the partial rank correlation coefficient (PRCC),",
                   "is the rank correlation between one assumption and Ct after removing the effect of all the",
                   "others, from -1 to 1, with a 95% interval. Blue bars raise Ct and orange bars lower it.",
                   "Assumptions that are fixed do not vary and are left out. If you have linked assumptions",
                   "or uploaded draws, read the values with care: the inputs are no longer independent.",
                   "The model itself has no random noise, so PRCC values are often large; compare their order",
                   "and sign more than their size."),
          radioButtons("sens_metric", NULL, inline = TRUE, selected = "prcc",
                       c("Partial rank correlation (PRCC)" = "prcc", "Rank correlation (rho)" = "rho",
                         "Share of squared rank correlation (rough guide)" = "contrib")),
          div(class = "dl-row", dl_png("sens_plot", "sensitivity.png")),
          plotOutput("sens_plot", height = 400)),
        tabPanel("Assumption draws",
          helpText("Histograms of the values drawn for each assumption across all trials. An assumption",
                   "that is fixed shows as a single bar. Click any plot to enlarge it."),
          div(class = "dl-row",
              tags$button(type = "button", class = "btn btn-default btn-sm dl-grid", `data-target` = "draws_grid",
                          `data-file` = "assumption_draws.png", icon("download"), " Download all (PNG)")),
          uiOutput("draws_grid")),
        tabPanel("Survival curves",
          helpText("Survivorship (the fraction of mosquitoes still alive at each age) and the daily",
                   "mortality hazard for the first 100 trials. Each line is one trial."),
          div(class = "dl-row", dl_png("surv_plot", "survival_curves.png")),
          plotOutput("surv_plot", height = 450)),
        tabPanel("Model check",
          p("As a check on the implementation, the deterministic model is run with the fixed",
            "parameter values reported by Styer et al. (2007) and compared with their published",
            "vectorial capacity estimates. Published values are never used as inputs, except that",
            "r is solved from the exponential stable-age case."),
          div(class = "table-tools", copy_btn("validation")),
          div(style = "overflow-x: auto;", tableOutput("validation")),
          p(class = "check-result",
            sprintf("Largest difference from a published value: %.1f%%. Published values are rounded to one decimal place, so small differences are expected.", max_dev)),
          h4("Comparison for the paper"),
          p("Four results side by side for each mortality model and age structure: the value published by",
            "Styer et al. (2007); this model run deterministically at their parameter values; a probabilistic run",
            "in which every assumption in use is drawn uniformly within plus or minus 20% of that same value; and a",
            "probabilistic run with the literature-based distributions (the default preset). The uniform run is",
            "centred on the deterministic one, so its difference from the deterministic value is the effect of",
            "parameter uncertainty alone. 10,000 trials, random seed 2026. In the uniform run the growth rate r and",
            "first-bite age are varied too in the stable age distribution (the first-bite age is rounded to a whole",
            "day when used). The table is precomputed with exactly these settings, and the tests check it against",
            "the model, so Run comparison loads it at once."),
          div(class = "dl-row",
              actionButton("run_paper", "Run comparison", class = "btn-primary btn-sm"),
              downloadButton("dl_paper", "Download CSV", class = "btn-sm")),
          div(class = "table-tools", copy_btn("paper_tbl")),
          div(style = "overflow-x: auto;", tableOutput("paper_tbl")),
          p(class = "eq-note", style = "margin-top: 10px;",
            "Read the two probabilistic runs differently. The uniform run is centred on the deterministic value, so it",
            "isolates the effect of uncertainty. The literature-based run is a different scenario: its distributions are",
            "centred on literature values for biting rate, mosquito density and vector competence that are lower than the",
            "values Styer et al. used (for example, a mean biting rate of about 0.45 per day against their 0.75), so its",
            "Ct is far lower. Do not read that gap as an effect of uncertainty. The growth rate r and first-bite age have",
            "no published range, so they stay fixed in that run. The default r is solved so that the exponential,",
            "stable-age case reproduces the published value, so that one row matches by construction; the other five",
            "rows are independent checks."),
          div(class = "dl-row", dl_png("paper_plot", "pvec_comparison.png")),
          plotOutput("paper_plot", height = 430)),
        tabPanel("About",
          h4("What this tool does"),
          p("PVEC (Probabilistic VECtorial capacity) propagates uncertainty in transmission and mosquito mortality parameters",
            "through an age-specific vectorial capacity model. Each assumption can be fixed or",
            "given a probability distribution; the simulation draws parameter sets at random and",
            "reports the resulting distribution of vectorial capacity (Ct), along with a",
            "sensitivity ranking of the inputs."),
          p(strong("Ct"), "is total vectorial capacity: age-specific vectorial capacity combined",
            "across the age structure of the mosquito population (a stable age distribution or",
            "synchronous emergence)."),
          h4("Model structure"),
          p("Vectorial capacity follows the classical formulation of Macdonald (1957) and",
            "Garrett-Jones (1964), extended to age-dependent mortality and extrinsic incubation",
            "following Styer et al. (2007). Mortality can follow exponential, Gompertz, or",
            "logistic hazards. Vector competence enters as a multiplicative term. Population age",
            "structure can be a stable age distribution or synchronous emergence."),
          div(class = "fold", id = "eq_fold",
            tags$button(id = "eq_toggle", type = "button", class = "adv-toggle", `aria-expanded` = "false",
                        `aria-controls` = "eq_body",
                        span(class = "chev", icon("chevron-right")), strong("Equations"),
                        span(class = "adv-tag", "how Ct is calculated")),
            div(class = "adv-body", id = "eq_body", div(class = "adv-clip", div(class = "eq-pad",
          P("Notation: ", v("x"), " is mosquito age in days; ", v("m"), " is mosquito density per person; ", v("a"),
            " is the biting rate (bites on humans per mosquito per day); ", v("c"), " is vector competence; ", v("n"),
            " is the extrinsic incubation period in days; ", v("r"), " is the population growth rate; and ", v("σ"),
            " is the age at first bite."),
          P(b_("1. Classical vectorial capacity"), " (Macdonald 1957; Garrett-Jones 1964), with constant daily survival ", v("p"), ":"),
          eq(v("C"), " = ", v("m"), " ", v("a"), sup_("2"), " ", v("c"), " ", eq_frac(h(v("p"), sup_(v("n"))), h("−ln ", v("p")))),
          P(b_("2. Age-specific mortality"), " (Styer et al. 2007). The daily hazard ", v("μ"), "(", v("x"), ") takes one of three forms. ",
            v("a"), " is the initial hazard, ", v("b"), " the rate of ageing and ", v("s"), " the deceleration of the logistic model:"),
          eq("Exponential: ", v("μ"), "(", v("x"), ") = ", v("a"), "<br>",
             "Gompertz: ", v("μ"), "(", v("x"), ") = ", v("a"), " ", v("e"), sup_(h(v("b"), v("x"))), "<br>",
             "Logistic: ", v("μ"), "(", v("x"), ") = ", eq_frac(h(v("a"), " ", v("e"), sup_(h(v("b"), v("x")))),
               h("1 + (", v("a"), v("s"), "/", v("b"), ")(", v("e"), sup_(h(v("b"), v("x"))), " − 1)"))),
          P(b_("3. Survivorship and remaining life expectancy"), ", from the hazard (the fraction of mosquitoes alive at age ", v("x"),
            ", and the expected days of life left for a mosquito that has reached it):"),
          eq(v("l"), "(", v("x"), ") = exp(−", eq_sum("k = 0", h(v("x"), " − 1")), " ", v("μ"), "(", v("k"), "))", "<br>",
             v("e"), "(", v("x"), ") = ", eq_sum("k ≥ x", "∞"), " ", eq_frac(h(v("l"), "(", v("k"), ")"), h(v("l"), "(", v("x"), ")")), " − ½"),
          P(b_("4. Age-specific vectorial capacity."), " A mosquito of age ", v("x"), " must survive the ", v("n"),
            "-day incubation period and then lives, on average, ", v("e"), "(", v("x"), " + ", v("n"), ") more days to bite:"),
          eq(v("C"), "(", v("x"), ") = ", v("m"), " ", v("a"), sup_("2"), " ", v("c"), " ", eq_frac(h(v("l"), "(", v("x"), " + ", v("n"), ")"), h(v("l"), "(", v("x"), ")")),
             " ", v("e"), "(", v("x"), " + ", v("n"), ")"),
          P(class = "eq-note", "With constant mortality the survival term becomes ", v("p"), sup_(v("n")), " and ", v("e"),
            " approaches 1/(−ln ", v("p"), "), so this reduces to equation 1. The tests check this."),
          P(b_("5. Total vectorial capacity"), ", Ct, depends on the population age structure."),
          eq("Synchronous emergence: ", v("C"), sub_("t"), " = ", eq_frac("1", "4"), " ", eq_sum("x = 3", "6"), " ", v("C"), "(", v("x"), ")", "<br>",
             "Stable age distribution: ", v("w"), "(", v("x"), ") = ", eq_frac(h(v("l"), "(", v("x"), ") ", v("e"), sup_(h("−", v("r"), v("x")))),
               h(eq_sum("k = 0", "199"), " ", v("l"), "(", v("k"), ") ", v("e"), sup_(h("−", v("r"), v("k"))))), "<br>",
             h(v("C"), sub_("t"), " = ", eq_sum(h(v("x"), " ≥ ", v("σ")), "199"), " ", v("w"), "(", v("x"), ") ", v("C"), "(", v("x"), ")")),
          P(class = "eq-note", "Synchronous emergence averages ", v("C"), "(", v("x"), ") over ages 3 to 6 days. Under the stable age distribution, ",
            v("w"), "(", v("x"), ") is the share of the population at age ", v("x"), " (ages 0 to 199 days) and only mosquitoes old enough to have taken a first bite, ",
            v("x"), " ≥ ", v("σ"), " (rounded to a whole day), contribute."),
          P(b_("6. Probabilistic version."), " Each of ", v("N"), " trials draws a parameter set ", v("θ"), sub_("i"), " = (", v("a"), ", ", v("m"), ", ", v("c"), ", ", v("n"),
            ", mortality parameters, ", v("r"), ", ", v("σ"), ") from the distributions chosen on the Define assumptions tab, with optional rank correlations, and computes"),
          eq(v("C"), sub_(h("t", ",", v("i"))), " = ", v("f"), "(", v("θ"), sub_("i"), "),   ", v("i"), " = 1, …, ", v("N")),
          P(class = "eq-note", "The forecast is the distribution of ", v("C"), sub_("t,i"), ". The incubation period ", v("n"),
            " is rounded to a whole day between 1 and 150. Setting every assumption to Fixed gives the deterministic model that the Model check tab compares with Styer et al. (2007). ",
            "Sensitivity is the partial rank correlation of each input with ", v("C"), sub_("t"), "."))))),
          h4("Methods notes"),
          tags$ul(
            tags$li("Each trial draws one value for every assumption, runs the age-specific model, and records Ct."),
            tags$li("Assumptions are drawn independently unless you link them. A rank correlation between two",
                    "assumptions is imposed with a Gaussian copula, which leaves each assumption's own",
                    "distribution unchanged. If several requested correlations cannot all hold together they are",
                    "reduced by the same fraction and the run reports it."),
            tags$li("Uploaded draws are used as whole rows, so any correlation in the file is kept. Rows are sampled",
                    "without replacement when the file has at least as many rows as trials, otherwise with replacement."),
            tags$li("The growth rate r and the age at first bite apply to the stable age distribution and can be given",
                    "distributions like any other assumption. The default r is solved to reproduce a published",
                    "Ct, so treat it as a calibration, not a field estimate."),
            tags$li("Sensitivity uses partial rank correlation coefficients (PRCC) with 95% intervals. The older",
                    "share-of-squared-correlation view is a rough guide, not a variance decomposition.")),
          h4("Sources"),
          tags$ul(
            tags$li("Macdonald G (1957) The Epidemiology and Control of Malaria. Oxford University Press. ",
                    tags$a(href = "https://archive.org/details/in.ernet.dli.2015.549644/page/n11/mode/2up", target = "_blank", "Internet Archive")),
            tags$li("Garrett-Jones C (1964) Prognosis for interruption of malaria transmission through assessment of the mosquito's vectorial capacity. Nature 204:1173-1175. ",
                    tags$a(href = "https://doi.org/10.1038/2041173a0", target = "_blank", "https://doi.org/10.1038/2041173a0")),
            tags$li("Styer LM, Carey JR, Wang J-L, Scott TW (2007) Mosquitoes do senesce: departure from the paradigm of constant mortality. Am J Trop Med Hyg 76:111-117. ",
                    tags$a(href = "https://doi.org/10.4269/ajtmh.2007.76.111", target = "_blank", "https://doi.org/10.4269/ajtmh.2007.76.111"))),
          p("Parameter ranges and distributions are from the literature as described in the",
            "accompanying paper."),
          p(class = "app-meta", paste0("Version ", APP_VERSION, ", last updated ", LAST_UPDATED)),
          tags$hr(style = "margin: 30px 0 20px;"),
          div(class = "author-card",
            img(src = "headshot.jpg", alt = "Jackson Strand", class = "author-photo"),
            div(class = "author-text",
              div(class = "author-label", "About the author"),
              h4("Jackson R. Strand"),
              p("PhD student, Montana State University"),
              p(class = "author-note",
                "Jackson is an entomologist who studies insect ecology and biological control, with a background in chemical ecology and plant-insect interactions. He frequently works with Bayesian statistics and simulation in R, and he built PVEC to make probabilistic vectorial capacity forecasts accessible to anyone who wants to explore how parameter uncertainty shapes transmission risk."),
              p(class = "author-link",
                tags$a(href = "https://www.jackson-strand.com", target = "_blank",
                       rel = "noopener", "www.jackson-strand.com")))))
      )
    )
  ),
  # The copyright year follows the last-updated date, so deploy.R keeps both current
  div(class = "app-footer",
      HTML(paste0("&copy; ", sub(".*, ", "", LAST_UPDATED), " Jackson R. Strand &nbsp;&middot;&nbsp; Last updated: ", LAST_UPDATED))),
  # Page behaviour (notification panel, cards, jump bar, share link, downloads) is in www/pvec.js,
  # and the styling is in www/pvec.css, so the browser can cache both
  tags$script(src = "pvec.js")
)

# ---- Server ----
server <- function(input, output, session) {

  get_spec <- function(id) {
    s <- lapply(fields, function(f) input[[paste0(id, "_", f)]])
    names(s) <- fields
    c(list(dist = input[[paste0(id, "_dist")]]), s)
  }

  set_spec <- function(id, s) {
    updateSelectInput(session, paste0(id, "_dist"), selected = s$dist)
    for (f in fields) updateNumericInput(session, paste0(id, "_", f), value = s[[f]])
  }

  # Assumptions in use. Growth rate and first-bite age only matter for the stable age distribution.
  active_ids <- function(model, structure = input$structure)
    c(names(vc_specs), "mort_a",
      if (model != "exponential") "mort_b",
      if (model == "logistic") "mort_s",
      if (identical(structure, "stable")) names(pop_specs))

  setting_ids <- all_setting_ids
  loaded <- reactiveVal(NULL)

  # ---- Correlations between assumptions ----
  empty_pairs <- function() data.frame(a = character(), b = character(), rho = numeric(), stringsAsFactors = FALSE)
  corr_pairs <- reactiveVal(empty_pairs())

  observe({   # offer only the assumptions in use for the current model and age structure
    act <- active_ids(input$mort_model, input$structure)
    ch  <- setNames(act, labels[act])
    updateSelectInput(session, "corr_a", choices = ch, selected = isolate(if (input$corr_a %in% act) input$corr_a else act[1]))
    updateSelectInput(session, "corr_b", choices = ch, selected = isolate(if (input$corr_b %in% act) input$corr_b else act[2]))
  })

  observeEvent(input$corr_add, {
    a <- input$corr_a; b <- input$corr_b; rho <- input$corr_rho
    bad <- if (is.null(a) || is.null(b) || identical(a, b)) "Pick two different assumptions."
           else if (is.na(rho) || abs(rho) > 0.95) "Enter a rank correlation between -0.95 and 0.95."
           else if (identical(get_spec(a)$dist, "Fixed")) sprintf("%s is fixed, so it cannot be correlated. Give it a distribution first.", labels[[a]])
           else if (identical(get_spec(b)$dist, "Fixed")) sprintf("%s is fixed, so it cannot be correlated. Give it a distribution first.", labels[[b]])
           else if (nrow(corr_pairs()) >= 15) "You can add at most 15 correlations."
    if (!is.null(bad)) { showNotification(bad, type = "error", duration = 6); return() }
    cp <- corr_pairs()
    cp <- cp[!((cp$a == a & cp$b == b) | (cp$a == b & cp$b == a)), , drop = FALSE]   # a new value replaces an old one
    corr_pairs(rbind(cp, data.frame(a = a, b = b, rho = rho, stringsAsFactors = FALSE)))
  })
  lapply(1:15, function(i) observeEvent(input[[paste0("corr_rm_", i)]], {
    cp <- corr_pairs(); if (i <= nrow(cp)) corr_pairs(cp[-i, , drop = FALSE])
  }, ignoreInit = TRUE))

  output$corr_list <- renderUI({
    cp <- corr_pairs()
    if (!nrow(cp)) return(helpText("No correlations set, so every assumption is drawn independently."))
    act <- active_ids(input$mort_model, input$structure)
    tagList(lapply(seq_len(nrow(cp)), function(i) {
      inuse <- cp$a[i] %in% act && cp$b[i] %in% act
      div(class = paste("corr-row", if (!inuse) "inactive"),
          sprintf("%s and %s: %+.2f", labels[[cp$a[i]]], labels[[cp$b[i]]], cp$rho[i]),
          if (!inuse) " (not used with the current model or age structure)",
          actionLink(paste0("corr_rm_", i), "remove"))
    }))
  })

  # The share link needs the correlation list on the page
  observe(session$sendCustomMessage("corrState",
            as.list(sprintf("%s;%s;%s", corr_pairs()$a, corr_pairs()$b, corr_pairs()$rho))))

  # ---- Uploaded joint parameter draws ----
  uploaded <- reactiveVal(NULL)
  min_ok <- c(a_bite = 0, n_eip = 0, m_dens = 0, vec_comp = 0, mort_a = 0, mort_b = 0, mort_s = 0, growth_r = -Inf, first_bite = 0)

  observeEvent(input$up_file, {
    df <- tryCatch(read.csv(input$up_file$datapath, comment.char = "#", stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || !nrow(df)) { showNotification("That file could not be read as a CSV with a header row.", type = "error", duration = 8); return() }
    use <- intersect(names(df), setting_ids)
    if (!length(use)) {
      showNotification(sprintf("No column names matched. Use these names: %s. The template has them.", paste(setting_ids, collapse = ", ")),
                       type = "error", duration = 10); return()
    }
    df[use] <- lapply(df[use], function(x) suppressWarnings(as.numeric(x)))
    probs <- unlist(lapply(use, function(id) {
      x <- df[[id]]
      if (anyNA(x) || any(!is.finite(x))) sprintf("%s has blank, non-numeric or infinite values", id)
      else if (any(x < min_ok[[id]])) sprintf("%s has values below %s", id, min_ok[[id]])
    }))
    if (length(probs)) { showNotification(paste("The file was not used:", paste(probs, collapse = "; ")), type = "error", duration = 10); return() }
    if (nrow(df) < 20) { showNotification("The file needs at least 20 rows.", type = "error", duration = 8); return() }
    uploaded(list(df = df[use], name = input$up_file$name, ignored = setdiff(names(df), use),
                  sig = list(input$up_file$name, nrow(df), sum(unlist(df[use])))))
  })
  observeEvent(input$up_clear, uploaded(NULL))

  output$up_status <- renderUI({
    up <- uploaded()
    if (is.null(up)) return(helpText("No file loaded. Upload a CSV with one column per assumption, named as in the template.",
                                     "Whole rows are used together, so correlations in the file are kept."))
    act <- active_ids(input$mort_model, input$structure)
    used <- intersect(names(up$df), act)
    tagList(
      div(sprintf("Using %s rows from %s for: %s.", format(nrow(up$df), big.mark = ","), up$name,
                  if (length(used)) paste(labels[used], collapse = ", ") else "nothing (none of its columns apply to the current settings)"),
          actionLink("up_clear", "clear")),
      if (length(up$ignored)) helpText("Columns not used because they do not match an assumption name:", paste(up$ignored, collapse = ", ")),
      helpText("Uploaded draws are not included in saved settings or shared links."))
  })

  observe({   # mark the cards that take their values from the file
    up <- uploaded(); act <- active_ids(input$mort_model, input$structure)
    m <- setNames(vector("list", length(setting_ids)), setting_ids)
    for (id in setting_ids) m[[id]] <- if (!is.null(up) && id %in% names(up$df) && id %in% act)
      sprintf("Values come from the uploaded draws (%s rows). The settings below are ignored.", format(nrow(up$df), big.mark = ",")) else ""
    session$sendCustomMessage("uploadedCards", m)
  })

  output$dl_template <- downloadHandler(
    filename = function() "parameter_draws_template.csv",
    content  = function(file) {
      act <- active_ids(input$mort_model, input$structure)
      ex  <- vapply(act, function(id) as.numeric(get_spec(id)$value), 0)
      ex  <- signif(ifelse(is.na(ex), 0, ex), 6)
      writeLines(paste("# One row per draw, one column per assumption. Delete the columns you do not need.",
                       "At least 20 rows. Replace the example values with your own draws."), file)
      write.table(as.data.frame(rbind(ex, ex, ex), row.names = FALSE), file, append = TRUE, sep = ",", row.names = FALSE)
    })

  apply_cfg <- function(v) {
    get1 <- function(k) if (k %in% names(v)) v[[k]] else NA_character_
    num  <- function(k) suppressWarnings(as.numeric(get1(k)))
    if (get1("structure") %in% c("stable", "synchronous"))
      updateSelectInput(session, "structure", selected = get1("structure"))
    if (!is.na(num("seed")))  updateNumericInput(session, "seed", value = num("seed"))
    if (get1("trials") %in% c("500", "1000", "5000", "10000"))
      updateSelectInput(session, "n_iter", selected = get1("trials"))
    for (id in setting_ids) {
      src <- get1(paste0(id, ".source"))
      if (!is.na(src)) updateTextInput(session, paste0(id, "_source"), value = src)
      d <- get1(paste0(id, ".dist"))
      if (is.na(d) || !d %in% dist_choices) next
      sp <- list(dist = d)
      for (f in fields) sp[[f]] <- num(paste0(id, ".", f))
      set_spec(id, sp)
    }
    # Correlations: a loaded file or link replaces whatever was set
    pairs <- empty_pairs()
    for (x in v[grepl("^corr\\.", names(v))]) {
      parts <- strsplit(x, ";", fixed = TRUE)[[1]]
      rho <- if (length(parts) == 3) suppressWarnings(as.numeric(parts[3])) else NA
      if (length(parts) == 3 && all(parts[1:2] %in% setting_ids) && parts[1] != parts[2] && is.finite(rho) && abs(rho) <= 0.95)
        pairs <- rbind(pairs, data.frame(a = parts[1], b = parts[2], rho = rho, stringsAsFactors = FALSE))
    }
    corr_pairs(pairs)
    # Older settings files and links stored r and the first-bite age as plain numbers
    for (old in list(c("r", "growth_r"), c("sigma", "first_bite")))
      if (!is.na(num(old[1])) && is.na(get1(paste0(old[2], ".dist")))) {
        base <- pop_specs[[old[2]]]; base$dist <- "Fixed"; base$value <- num(old[1]); set_spec(old[2], base)
      }
  }

  output$dl_settings <- downloadHandler(
    filename = function() "vectorial_capacity_settings.csv",
    content  = function(file) {
      rows <- list(model = input$mort_model, structure = input$structure,
                   trials = input$n_iter, seed = input$seed)
      cp <- corr_pairs()
      for (i in seq_len(nrow(cp))) rows[[paste0("corr.", i)]] <- sprintf("%s;%s;%s", cp$a[i], cp$b[i], cp$rho[i])
      for (id in setting_ids) {
        sp <- get_spec(id)
        rows[[paste0(id, ".dist")]] <- sp$dist
        rows[[paste0(id, ".source")]] <- input[[paste0(id, "_source")]]
        for (f in fields) rows[[paste0(id, ".", f)]] <- sp[[f]]
      }
      writeLines("# Settings saved from PVEC, the Probabilistic Vectorial Capacity Simulator. Use Upload settings to restore them.", file)
      write.table(data.frame(setting = names(rows),
                             value = vapply(rows, function(x) if (is.null(x) || is.na(x)) "" else as.character(x), "")),
                  file, append = TRUE, sep = ",", row.names = FALSE, qmethod = "double")
    })

  # Apply a named character vector of settings (from a file or from a link)
  apply_loaded <- function(v, what) {
    ok <- !is.null(v) && all(c("model", "structure") %in% names(v)) &&
          v[["model"]] %in% c("logistic", "gompertz", "exponential")
    if (!ok) {
      showNotification(sprintf("That %s is not a settings %s saved from this app.", what, what),
                       type = "error", duration = 8)
      return(invisible())
    }
    if (!identical(v[["model"]], input$mort_model)) {
      loaded(v)
      updateSelectInput(session, "mort_model", selected = v[["model"]])
    } else apply_cfg(v)
    showNotification("Settings loaded. Click Run to use them.", type = "message", duration = 6)
  }

  observeEvent(input$load_settings, {
    v <- tryCatch({
      df <- read.csv(input$load_settings$datapath, comment.char = "#", stringsAsFactors = FALSE,
                     colClasses = "character", na.strings = character(0))
      setNames(df$value, df$setting)
    }, error = function(e) NULL)
    apply_loaded(v, "file")
  })

  # Settings carried in the page address (the "Copy link" button)
  observeEvent(input$url_settings, {
    v <- tryCatch(unlist(input$url_settings), error = function(e) NULL)
    apply_loaded(v, "link")
  })

  # Start over: confirm, then restart the app from its defaults (clears runs, history and any link settings)
  observeEvent(input$start_over, {
    showModal(modalDialog(
      title = "Start over?", size = "s", easyClose = TRUE,
      "This resets every setting to its default and clears your run history. Settings you have not saved or shared will be lost.",
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_start_over", "Start over", class = "btn-primary"))))
  })
  observeEvent(input$confirm_start_over, {
    removeModal()
    session$sendCustomMessage("startOver", list())
  })

  observeEvent(input$rand_seed, updateNumericInput(session, "seed", value = sample.int(99999, 1)))

  # Distribution previews inside each assumption card
  lapply(setting_ids, function(id) {
    output[[paste0(id, "_prev")]] <- renderPlot({
      sp <- get_spec(id)
      req(is.null(check_spec(sp, labels[[id]])))
      preview_plot(sp)
    }, bg = "transparent",
    alt = reactive(sprintf("Preview of the %s distribution for %s.", input[[paste0(id, "_dist")]], labels[[id]])))
  })

  # Swap in fitted values when the mortality model changes, keep chosen distributions
  observeEvent(input$mort_model, {
    for (id in names(mort_specs[[input$mort_model]])) {
      s <- mort_specs[[input$mort_model]][[id]]
      s$dist <- input[[paste0(id, "_dist")]]
      set_spec(id, s)
    }
    # A settings file that changed the mortality model is applied after the fitted values
    if (!is.null(loaded())) { apply_cfg(loaded()); loaded(NULL) }
  }, ignoreInit = TRUE)

  observeEvent(input$goto_about, updateTabsetPanel(session, "tabs", selected = "About"))

  apply_preset <- function(preset) {
    all <- c(vc_specs, mort_specs[[input$mort_model]], pop_specs)
    for (id in names(all)) {
      s <- all[[id]]
      s$dist <- if (preset == "lit") lit_dists[[id]] else "Fixed"
      set_spec(id, s)
    }
  }
  observeEvent(input$preset, apply_preset(input$preset), ignoreInit = TRUE)
  observeEvent(input$reset_preset, apply_preset(input$preset))

  # The preset's own spec for one assumption (what "unedited" means)
  preset_spec <- function(id, model = input$mort_model, preset = input$preset) {
    sp <- c(vc_specs, mort_specs[[model]], pop_specs)[[id]]
    sp$dist <- if (identical(preset, "lit")) lit_dists[[id]] else "Fixed"
    sp
  }

  # Mark assumption cards whose values differ from the preset, and let each be reset on its own
  run_specs  <- reactiveVal(NULL)     # the assumption values used in the most recent run
  last_cards <- reactiveVal(NULL)
  # Wait for a pause in typing (300 ms) before comparing every card with its preset and the last run
  card_inputs <- debounce(reactive(list(
    model = input$mort_model, structure = input$structure, preset = input$preset,
    specs = lapply(setNames(setting_ids, setting_ids), get_spec))), 300)
  observe({
    rs  <- run_specs()
    ci  <- card_inputs()
    act <- active_ids(ci$model, ci$structure)
    st  <- lapply(setNames(setting_ids, setting_ids), function(id) {
      cur <- ci$specs[[id]]
      list(differs = !same_spec(cur, preset_spec(id, ci$model, ci$preset)),
           changed = !is.null(rs) && id %in% act && (!(id %in% names(rs)) || !same_spec(cur, rs[[id]])))
    })
    if (!identical(st, isolate(last_cards()))) {
      last_cards(st)
      session$sendCustomMessage("cardStates", st)
    }
  })
  lapply(setting_ids, function(id)
    observeEvent(input[[paste0(id, "_reset")]], set_spec(id, preset_spec(id)), ignoreInit = TRUE))

  # Fit a distribution to a reported 95% interval (and mean), then fill in the card
  lapply(setting_ids, function(id) {
    msg <- reactiveVal(NULL)
    output[[paste0(id, "_fitmsg")]] <- renderUI({
      m <- msg(); if (is.null(m)) return(NULL)
      div(class = paste("fit-msg", if (!m$ok) "bad"), m$note)
    })
    observeEvent(input[[paste0(id, "_fit")]], {
      s <- get_spec(id)
      r <- fit_range(s$dist, input[[paste0(id, "_fit_lo")]], input[[paste0(id, "_fit_hi")]],
                     input[[paste0(id, "_fit_est")]], s)
      msg(r)
      if (r$ok) for (f in names(r$fields))
        updateNumericInput(session, paste0(id, "_", f), value = signif(r$fields[[f]], 6))
    }, ignoreInit = TRUE)
  })

  # Snapshot of everything that feeds a run, to tell when results are out of date
  cur_sig_now <- reactive({
    ids <- active_ids(input$mort_model)
    list(model = input$mort_model, structure = input$structure,
         n = input$n_iter, seed = input$seed, corr = corr_pairs(), upload = uploaded()$sig,
         specs = lapply(setNames(ids, ids), get_spec))
  })
  # The warning waits for a pause in typing. A run records the exact current snapshot, not the delayed one.
  cur_sig <- debounce(cur_sig_now, 300)
  run_sig <- reactiveVal(NULL)

  output$stale_note <- renderUI({
    req(run_sig())
    if (!identical(cur_sig(), run_sig()))
      div(class = "stale-note", "\u26a0 Settings changed since the last run. Click Run to update the results.")
  })

  results_val <- reactiveVal(NULL)
  history     <- reactiveVal(list())   # up to 10 recent runs, kept for comparison
  run_count   <- reactiveVal(0)
  run_info    <- reactiveVal("No runs yet")

  # Everything downstream waits until at least one run exists
  results <- reactive(req(results_val()))

  output$run_status <- renderText(run_info())
  output$run_status_print <- renderText(run_info())
  outputOptions(output, "run_status_print", suspendWhenHidden = FALSE)   # hidden on screen, needed when printing

  observeEvent(input$run, {
    model <- input$mort_model
    ids   <- active_ids(model)
    specs <- lapply(ids, get_spec)
    names(specs) <- ids
    problems <- unlist(Map(check_spec, specs, labels[ids]))
    if (!is.null(problems)) {
      showNotification(paste(problems, collapse = ". "), type = "error", duration = 8)
      return()
    }

    t0 <- Sys.time()
    n  <- as.integer(input$n_iter)
    run <- withProgress(message = "Running trials", value = 0,
      run_model(model, input$structure, specs, n, input$seed, pairs = corr_pairs(), uploaded = uploaded(),
                progress = function(i, n) setProgress(i / n, detail = sprintf("%d of %d", i, n))))
    d <- run$draws; ct <- run$ct; b_i <- run$b; s_i <- run$s

    settings <- c(
      sprintf("PVEC (probabilistic vectorial capacity simulator), version %s", APP_VERSION),
      sprintf("Run time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      sprintf("Mortality model: %s", model),
      sprintf("Population age structure: %s", input$structure),
      sprintf("Trials: %d", n),
      sprintf("Random seed: %s", input$seed),
      unlist(Map(function(s, l, id) {
        src <- trimws(paste(input[[paste0(id, "_source")]], collapse = ""))
        sprintf("%s: %s, %s%s", l, s$dist, describe_spec(s), if (nzchar(src)) sprintf(" [Source: %s]", src) else "")
      }, specs, labels[ids], ids)),
      if (!is.null(run$achieved))
        sprintf("Rank correlation: %s and %s, requested %+.2f, achieved %+.2f",
                labels[run$achieved$a], labels[run$achieved$b], run$achieved$rho, run$achieved$achieved),
      run$notes)

    new_run <- run_count() + 1
    entry <- list(run = new_run, ct = ct,
                  label = sprintf("Run %d: %s, %s, %s trials, median Ct %s", new_run, model,
                                  input$structure, format(n, big.mark = ","), signif(median(ct), 3)))
    history(c(tail(history(), 9), list(entry)))
    results_val(list(ct = ct, draws = d, model = model, b = b_i, s = s_i,
                     settings = settings, run = new_run))
    run_sig(cur_sig_now())
    run_specs(specs)
    run_count(run_count() + 1)

    secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    msg  <- sprintf("Run %d finished at %s, %s trials in %.1f seconds",
                    run_count(), format(Sys.time(), "%I:%M:%S %p"),
                    format(n, big.mark = ","), secs)
    run_info(msg)
    session$sendCustomMessage("newResults", list())

    # Skip the pop up for the automatic run when the app first opens
    if (input$run > 0) {
      showNotification(msg, type = "message", duration = 5)
    }
  }, ignoreNULL = FALSE)

  cert_bounds <- reactive(c(
    if (is.na(input$cert_lo)) -Inf else input$cert_lo,
    if (is.na(input$cert_hi))  Inf else input$cert_hi))

  output$cert_text <- renderUI({
    ct <- results()$ct
    b  <- cert_bounds()
    th <- input$thresh
    parts <- list()
    if (!all(is.infinite(b)))
      parts <- c(parts, list(h4(sprintf("Certainty is %.1f%% that Ct falls in this range",
                                        100 * mean(ct >= b[1] & ct <= b[2])))))
    if (!is.na(th))
      parts <- c(parts, list(h4(sprintf("Chance that Ct exceeds %s is %.1f%%",
                                        format(th), 100 * mean(ct > th)))))
    if (!length(parts))
      return(helpText("Enter a lower and/or upper limit, or a threshold, to see the probability."))
    tagList(parts)
  })

  output$forecast_summary <- renderUI({
    st <- summary_text(results()$ct)
    if (st$fixed) return(p(class = "forecast-summary", st$headline))
    tagList(
      p(class = "forecast-summary", HTML(sprintf("<b>Median Ct %s</b>. %s", st$median, htmltools::htmlEscape(st$rest)))),
      p(class = "forecast-note", st$precision))
  })

  # Offer the earlier runs in the "Compare" menu, keeping the current choice if it still exists
  observeEvent(history(), {
    h      <- history()
    others <- rev(h[-length(h)])
    choices <- c("None (current run only)" = "none",
                 setNames(vapply(others, function(e) as.character(e$run), ""),
                          vapply(others, function(e) e$label, "")))
    sel <- isolate(input$compare_run)
    if (is.null(sel) || !sel %in% choices) sel <- "none"
    updateSelectInput(session, "compare_run", choices = choices, selected = sel)
  })

  overlay_data <- reactive({
    id <- input$compare_run
    if (is.null(id) || id == "none") return(NULL)
    for (e in history()) if (as.character(e$run) == id) return(e)
    NULL
  })

  output$overlay_note <- renderUI({
    if (length(history()) < 2)
      helpText("Run again with different settings to compare runs here.")
    else if (!is.null(overlay_data()))
      helpText("Bars show each run's share of trials, so runs with different numbers of trials",
               "can be compared. The certainty range shading is hidden while comparing.")
  })

  output$compare_table <- renderUI({
    prev <- overlay_data(); req(prev); res <- results()
    q   <- function(x, p) unname(quantile(x, p))
    row <- function(label, ct) c(label, format(length(ct), big.mark = ","), fmt3(median(ct)), fmt3(mean(ct)),
                                 fmt3(q(ct, 0.025)), fmt3(q(ct, 0.975)))
    d_med <- median(res$ct) - median(prev$ct); d_mean <- mean(res$ct) - mean(prev$ct)
    sgn <- function(x) paste0(if (x > 0) "+" else "", fmt3(x))
    df <- as.data.frame(rbind(
      row(sprintf("Run %d (current)", res$run), res$ct),
      row(sprintf("Run %d", prev$run), prev$ct),
      c("Difference (current minus other)", "", sgn(d_med), sgn(d_mean), "", "")), stringsAsFactors = FALSE)
    names(df) <- c("Run", "Trials", "Median", "Mean", "2.5th percentile", "97.5th percentile")
    tagList(div(class = "compare-table", HTML(html_table(df))),
            div(class = "table-tools", copy_btn("compare_table")))
  })

  output$forecast_plot <- renderPlot({
    res <- results(); prev <- overlay_data()
    draw_forecast(res$ct, cert_bounds(), prev$ct,
                  c(sprintf("Run %d (current)", res$run), sprintf("Run %d", prev$run)), input$thresh)
  }, alt = reactive({
    ct <- results()$ct; st <- summary_text(ct); th <- input$thresh
    base <- if (st$fixed) st$headline
            else sprintf("Histogram of total vectorial capacity (Ct). Median %s. %s", st$median, st$rest)
    if (has_thresh(th)) sprintf("%s A dotted line marks Ct = %s; %.1f%% of trials are above it.", base, format(th), 100 * mean(ct > th))
    else base
  }))

  # Flag assumption boxes with invalid values as soon as they are entered
  last_errs  <- reactiveVal(NULL)
  last_warns <- reactiveVal(NULL)
  observe({
    active <- active_ids(input$mort_model)
    errs <- vapply(setting_ids, function(id) {
      if (!id %in% active) return("")
      msg <- check_spec(get_spec(id), labels[[id]])
      if (is.null(msg)) "" else substring(msg, nchar(labels[[id]]) + 2)
    }, character(1))
    if (!identical(errs, isolate(last_errs()))) {
      last_errs(errs)
      session$sendCustomMessage("assumpErr", as.list(errs))
    }
    warns <- vapply(setting_ids, function(id) {
      if (!id %in% active || nzchar(errs[[id]])) return("")
      soft_warning(id, get_spec(id))
    }, character(1))
    if (!identical(warns, isolate(last_warns()))) {
      last_warns(warns)
      session$sendCustomMessage("assumpWarn", as.list(warns))
    }
  })

  output$dl_csv <- downloadHandler(
    filename = function() "vectorial_capacity_results.csv",
    content  = function(file) {
      res <- results()
      writeLines(paste("#", res$settings), file)
      write.table(data.frame(trial = seq_along(res$ct), Ct = res$ct, res$draws),
                  file, append = TRUE, sep = ",", row.names = FALSE)
    })

  output$stats <- renderTable(stats_df(results()$ct), digits = 3)

  # The ranking is computed once per run; switching the metric only redraws it
  sens_res <- reactive(sens_contrib(results()))
  output$sens_plot <- renderPlot(draw_sens(results(), input$sens_metric, sens_res()), alt = reactive({
    sc <- sens_res(); m <- input$sens_metric
    if (is.null(sc)) "No assumptions vary, so there is nothing to rank."
    else if (identical(m, "prcc")) {
      k <- which.max(abs(sc$prcc$est))
      sprintf("Bar chart of the partial rank correlation between each assumption and Ct, with 95%% intervals. Strongest: %s, PRCC %.2f.",
              labels[[sc$prcc$id[k]]], sc$prcc$est[k])
    } else if (identical(m, "rho"))
      sprintf("Bar chart of the rank correlation between each assumption and Ct. Strongest: %s, rho %.2f.",
              labels[[names(sc$rho)[which.max(abs(sc$rho))]]], sc$rho[[which.max(abs(sc$rho))]])
    else sprintf("Bar chart of each assumption's share of the squared rank correlation with Ct. Largest: %s, %.0f%%.",
                 labels[[names(sc$contrib)[which.max(abs(sc$contrib))]]], max(abs(sc$contrib)))
  }))

  # One plot output per assumption, shown as tiles that open a larger version when clicked
  lapply(all_setting_ids, function(id) {
    output[[paste0("draw_", id)]] <- renderPlot({
      d <- results()$draws; req(id %in% names(d))
      draw_one(d[[id]], labels[[id]])
    }, alt = reactive({
      d <- results()$draws; req(id %in% names(d)); x <- d[[id]]
      if (diff(range(x)) == 0) sprintf("%s is fixed at %s.", labels[[id]], fmt3(x[1]))
      else sprintf("Histogram of the drawn values of %s. Median %s; 95%% of draws between %s and %s. Click to enlarge.",
                   labels[[id]], fmt3(median(x)), fmt3(quantile(x, 0.025)), fmt3(quantile(x, 0.975)))
    }))
  })

  output$draws_grid <- renderUI({
    d <- results()$draws
    div(class = "draws-grid", id = "draws_grid",
        lapply(names(d), function(id)
          div(class = "draw-tile", tabindex = 0, role = "button", `data-id` = id,
              `aria-label` = paste("Enlarge the plot for", labels[[id]]),
              plotOutput(paste0("draw_", id), height = "210px"))))
  })

  expand_id <- reactiveVal(NULL)
  observeEvent(input$expand_draw, {
    id <- input$expand_draw
    req(id %in% names(results()$draws))
    expand_id(id)
    showModal(modalDialog(title = labels[[id]], size = "l", easyClose = TRUE, footer = modalButton("Close"),
      div(class = "dl-row", dl_png("draw_big", "assumption_draw.png")),
      plotOutput("draw_big", height = "520px")))
  })
  output$draw_big <- renderPlot({
    id <- expand_id(); req(id)
    res <- results(); req(id %in% names(res$draws))
    pre  <- paste0(labels[[id]], ": ")
    line <- res$settings[startsWith(res$settings, pre)]
    draw_one(res$draws[[id]], labels[[id]], big = TRUE, spec = if (length(line)) substring(line[1], nchar(pre) + 1))
  }, alt = reactive({
    id <- expand_id(); req(id); x <- results()$draws[[id]]
    sprintf("Enlarged histogram of the drawn values of %s, with the median and the 2.5th and 97.5th percentiles marked.", labels[[id]])
  }))

  output$surv_plot <- renderPlot(draw_surv(results()), alt = "Survivorship and daily mortality hazard curves for the first 100 trials, one line per trial.")

  output$dl_report <- downloadHandler(
    filename = function() "vectorial_capacity_report.html",
    content  = function(file) {
      res <- results(); prev <- overlay_data()
      writeLines(build_report(res, cert_bounds(), prev,
                              c(sprintf("Run %d (current)", res$run), sprintf("Run %d", prev$run)), input$thresh), file)
    })

  output$validation <- renderTable(validation, digits = 2)

  paper_res <- reactiveVal(NULL)
  observeEvent(input$run_paper, {
    withProgress(message = "Running the comparison", value = 0,
      paper_res(paper_table(step = function(i, k) incProgress(1 / k, detail = sprintf("%d of %d", i, k)))))
  })
  output$paper_tbl <- renderTable({
    r <- paper_res()
    if (is.null(r)) return(data.frame(` ` = "Click Run comparison to compute the table.", check.names = FALSE))
    r
  }, digits = 2)
  output$paper_plot <- renderPlot({
    req(paper_res())
    draw_paper(paper_res())
  }, alt = "For each mortality model and age structure, the published value, the deterministic value, and the median with 95% range of the uniform plus or minus 20% run and of the literature-based run.")
  output$dl_paper <- downloadHandler(
    filename = function() "pvec_comparison.csv",
    content  = function(file) {
      r <- paper_res()
      if (is.null(r)) r <- paper_table()
      write.csv(r, file, row.names = FALSE)
    })
}

shinyApp(ui, server)
