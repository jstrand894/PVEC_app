library(shiny)
 
# Shown in the page footer; update when you publish a new version
LAST_UPDATED <- "October 3, 2026"
CITE_URL     <- "https://jstrand894.github.io/PVEC_app/"
CITE_YEAR    <- sub(".*, ", "", LAST_UPDATED)
APP_VERSION  <- "1.2"

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
  col <- .pv$red
  abline(v = thresh, col = col, lwd = 2.5, lty = 3)
  usr <- par("usr")
  lab <- sprintf("P(Ct > %s) = %.1f%%", format(thresh), 100 * mean(ct > thresh))
  cex <- 0.95
  w <- strwidth(lab, cex = cex); h <- strheight(lab, cex = cex)
  gap <- 0.012 * (usr[2] - usr[1]); padx <- 0.5 * strwidth("m", cex = cex); pady <- 0.6 * h   # the two axes have different units
  left <- thresh > mean(usr[1:2])                       # keep the label on the roomier side of the line
  x1 <- if (left) thresh - gap - w - 2 * padx else thresh + gap
  yc <- usr[3] + 0.88 * (usr[4] - usr[3])
  rect(x1, yc - h / 2 - pady, x1 + w + 2 * padx, yc + h / 2 + pady, col = .pv$bg, border = col, lwd = 1.5)
  text(x1 + padx, yc, lab, adj = c(0, 0.5), col = col, cex = cex, font = 2)
}

# Address of a file in www/ with its last-changed time added, so a browser holding an older copy fetches the new one
asset_url <- function(f) {
  t <- file.mtime(file.path("www", f))
  if (is.na(t)) f else paste0(f, "?v=", format(t, "%Y%m%d%H%M%S"))
}

# Main plot colour, matched to the accent colour in www/pvec.css
ACCENT <- "#2b6cb0"

# Plot text styled like the page: sans-serif, dark bold titles, softer axis and label colours
# Plot colours follow the page theme. Inside a plot output, Shiny reports the background and text colour of the
# element the plot sits in, so a dark card gives a dark plot. Elsewhere (the HTML report, tests) the plots stay light.
.pv <- new.env()
pvec_par <- function() {
  bg <- "#ffffff"; dark <- FALSE
  # The page reports its theme as input$theme. That is steadier than measuring the plot's background, which
  # briefly read as white while a plot was redrawing and made dark plots flash.
  theme <- tryCatch(shiny::getDefaultReactiveDomain()$input$theme, error = function(e) NULL)
  info <- tryCatch(shiny::getCurrentOutputInfo(), error = function(e) NULL)
  if (identical(theme, "dark") || identical(theme, "light")) {
    dark <- theme == "dark"; if (dark) bg <- "#171d24"
  } else if (!is.null(info) && is.function(info$bg)) {
    cols <- tryCatch(htmltools::parseCssColors(info$bg()), error = function(e) NULL)
    if (length(cols) == 1 && !is.na(cols)) { bg <- cols; dark <- mean(grDevices::col2rgb(bg)) < 128 }
  }
  .pv$dark  <- dark
  .pv$bg    <- bg
  .pv$fg    <- if (dark) "#e4e8ec" else "#1f2933"
  .pv$muted <- if (dark) "#a4afba" else "#4b535a"
  .pv$grid  <- if (dark) "#2b343e" else "#e3e8ee"
  .pv$bar   <- if (dark) "#3a4551" else "#d9dde2"      # bars outside the certainty range
  .pv$red   <- if (dark) "#ff7a6b" else "#b30000"
  par(family = "sans", bg = bg, fg = .pv$fg, col = .pv$fg, col.main = .pv$fg, col.lab = .pv$muted, col.axis = .pv$muted,
      font.main = 2, cex.main = 1.1)
}

draw_forecast <- function(ct, b, prev = NULL, run_labels = NULL, thresh = NA, bins = 50) {
  pvec_par()
  if (!is.null(prev)) {
    runs <- list(ct, prev)
    cols <- c(ACCENT, "#E69F00")
    all  <- unlist(runs)
    pad  <- max(diff(range(all)) * 0.05, 0.05 * abs(mean(all)), 1e-6)
    drng <- range(all) + c(-pad, pad)
    br   <- seq(drng[1], drng[2], length.out = bins + 1)
    xlim <- if (has_thresh(thresh)) range(drng, thresh) else drng     # keep the threshold on the plot
    # A run whose trials all give the same Ct (all assumptions fixed) is drawn as a line, not bars
    hs   <- lapply(runs, function(x) if (diff(range(x)) > 0) hist(x, breaks = br, plot = FALSE))
    ytop <- max(c(1e-9, unlist(lapply(hs, function(h) if (!is.null(h)) h$density)))) * 1.1
    plot(NA, xlim = xlim, ylim = c(0, ytop), xlab = "Ct", ylab = "Density",
         main = "")
    fig_title("Forecast of total vectorial capacity", line = 2)
    for (k in 1:2) {
      if (!is.null(hs[[k]])) {
        plot(hs[[k]], freq = FALSE, add = TRUE, col = adjustcolor(cols[k], 0.55), border = .pv$bg)
        abline(v = median(runs[[k]]), lwd = 2, lty = 2, col = cols[k])
      } else segments(runs[[k]][1], 0, runs[[k]][1], ytop * 0.9, lwd = 4, col = cols[k])
    }
    legend("topright", legend = run_labels, fill = adjustcolor(cols, 0.7), border = NA, bty = "n")
    if (has_thresh(thresh)) draw_threshold(ct, thresh)
    return(invisible())
  }
  h  <- hist(ct, breaks = bins, plot = FALSE)
  xl <- range(h$breaks); if (has_thresh(thresh)) xl <- range(xl, thresh)
  cols <- ifelse(h$mids >= b[1] & h$mids <= b[2], ACCENT, .pv$bar)
  plot(h, xlim = xl, col = cols,
       border = .pv$bg, main = "", xlab = "Ct")
  fig_title("Forecast of total vectorial capacity", line = 2)
  abline(v = median(ct), lwd = 2, lty = 2, col = .pv$fg)
  usr <- par("usr"); md <- median(ct)
  text(md, usr[4], paste("Median", fmt3(md)), pos = 3, offset = 0.3, cex = 0.9, font = 2, col = .pv$fg, xpd = NA)   # a flag above the line, clear of the bars
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
                      progress = function(done, n) {}, chunk = 250L, adjust = NULL) {
  # Seeding makes the run reproducible, but leave the session's random stream as it was, so other
  # random choices (the shuffle-seed button) are not fixed by the seed of the last run
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv())
  on.exit(if (is.null(old)) suppressWarnings(rm(".Random.seed", envir = globalenv()))
          else assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  set.seed(seed)
  g <- generate_draws(specs, n, pairs, uploaded)
  d <- g$draws
  # A temperature what-if scales the incubation period, the initial mortality hazard and the biting rate of every trial
  if (!is.null(adjust)) {
    d$n_eip  <- d$n_eip  * adjust[["n_eip"]]
    d$mort_a <- d$mort_a * adjust[["mort_a"]]
    d$a_bite <- d$a_bite * adjust[["a_bite"]]
  }
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

# Temperature what-if: each trial's incubation period, initial mortality hazard and biting rate are multiplied by
# (1 + pct / 100)^dT, where pct is the percent change per degree and dT the temperature change in degrees C.
# These per-degree values are placeholders to be replaced with values for a particular vector and pathogen.
temp_multipliers <- function(dT, pct_eip, pct_mort, pct_bite)
  c(n_eip = (1 + pct_eip / 100)^dT, mort_a = (1 + pct_mort / 100)^dT, a_bite = (1 + pct_bite / 100)^dT)

# Published temperature responses for the vector-pathogen pairs in the literature behind this tool. Each curve gives the
# incubation period (n, days), the adult mortality rate (mu, per day) and, where one is built in, the biting rate (a, per day) as
# functions of temperature in degrees C; values are held at the ends of the range each equation was fitted over.
#  - Aedes aegypti and dengue virus: Liu-Helmersson J, et al. (2014) PLoS ONE 9:e89783, equations 2, 5 and 6 (biting rate after
#    Scott et al. 2000, mortality after Yang et al. 2009, incubation period after Watts et al. 1987 and McLean et al. 1974).
#  - Anopheles and Plasmodium falciparum: mortality after Martens (1997), as given in Parasites & Vectors 6:20 (2013), and the
#    degree-day rule of 111 degree-days above 16 C for parasite development (Detinova; Macdonald 1957). No biting-rate curve is
#    built in, so the biting rate is left unchanged.
TEMP_CURVES <- list(
  aedes_dengue = list(
    label = "Aedes aegypti and dengue virus", ref = 27,
    a  = function(T) 0.0043 * T + 0.0943,
    n  = function(T) 4 + exp(5.15 - 0.123 * T),
    mu = function(T) 0.8692 - 0.1590 * T + 0.01116 * T^2 - 3.408e-4 * T^3 + 3.809e-6 * T^4,
    range = list(a = c(21, 32), n = c(12, 36), mu = c(10.54, 33.41))),
  anopheles_pf = list(
    label = "Anopheles and Plasmodium falciparum", ref = 25,
    a  = NULL,
    n  = function(T) 111 / (T - 16),
    mu = function(T) 1 / (-4.4 + 1.31 * T - 0.03 * T^2),
    range = list(n = c(17, 40), mu = c(5, 39))))

# The factors by which a trait changes when temperature moves from `ref` to `ref + dT` along a published curve. `outside` lists
# the traits whose new temperature lies beyond the range the equation was fitted over.
curve_multipliers <- function(curve, ref, dT) {
  cv <- TEMP_CURVES[[curve]]
  at <- function(f, rg, T) f(min(max(T, rg[1]), rg[2]))
  ratio <- function(f, rg) if (is.null(f)) 1 else at(f, rg, ref + dT) / at(f, rg, ref)
  out <- c(n = ref + dT < cv$range$n[1] || ref + dT > cv$range$n[2],
           mu = ref + dT < cv$range$mu[1] || ref + dT > cv$range$mu[2],
           a = !is.null(cv$a) && (ref + dT < cv$range$a[1] || ref + dT > cv$range$a[2]))
  list(mult = c(n_eip = ratio(cv$n, cv$range$n), mort_a = ratio(cv$mu, cv$range$mu), a_bite = ratio(cv$a, cv$range$a)),
       outside = names(out)[out])
}

# The multipliers for a run: a published curve, or the generic percent-per-degree placeholders
temperature_adjust <- function(curve, ref, dT, pe, pm, pa) {
  if (is.null(dT) || is.na(dT) || dT == 0) return(NULL)
  if (!is.null(curve) && curve %in% names(TEMP_CURVES) && !is.null(ref) && !is.na(ref)) return(curve_multipliers(curve, ref, dT))
  list(mult = temp_multipliers(dT, pe, pm, pa), outside = character(0))
}

# Which assumptions a run uses, for a given mortality model and age structure
active_ids_for <- function(model, structure)
  c(names(vc_specs), "mort_a", if (model != "exponential") "mort_b", if (model == "logistic") "mort_s",
    if (identical(structure, "stable")) names(pop_specs))

# Rebuild one assumption's spec from a saved settings vector (the same keys as the settings file)
spec_from_snap <- function(snap, id) {
  get <- function(k) if (k %in% names(snap)) snap[[k]] else NA_character_
  d <- get(paste0(id, ".dist"))
  if (is.na(d) || !nzchar(d)) return(NULL)
  sp <- list(dist = d)
  for (f in fields) sp[[f]] <- suppressWarnings(as.numeric(get(paste0(id, ".", f))))
  sp
}

# What differs between two saved runs: the model choices, the temperature change, linked assumptions and any assumption whose
# distribution or values differ. Returns a data frame with one row per difference, or an empty one.
snap_diff <- function(a, b) {
  rows <- list()
  add <- function(what, x, y) rows[[length(rows) + 1]] <<- data.frame(Setting = what, A = x, B = y, stringsAsFactors = FALSE)
  g <- function(v, k) { x <- if (k %in% names(v)) v[[k]] else NA; if (is.na(x)) "" else as.character(x) }
  nice <- c(logistic = "Logistic", gompertz = "Gompertz", exponential = "Exponential", stable = "Stable", synchronous = "Synchronous")
  for (k in c("model", "structure")) if (!identical(g(a, k), g(b, k)))
    add(c(model = "Mortality model", structure = "Age structure")[[k]], nice[[g(a, k)]], nice[[g(b, k)]])
  tr <- function(v) format(suppressWarnings(as.numeric(gsub(",", "", g(v, "trials")))), big.mark = ",")
  if (!identical(tr(a), tr(b))) add("Trials", tr(a), tr(b))
  if (!identical(g(a, "seed"), g(b, "seed"))) add("Seed", g(a, "seed"), g(b, "seed"))
  tmp <- function(v) { d <- suppressWarnings(as.numeric(g(v, "temp"))); if (is.na(d) || d == 0 || toupper(g(v, "temp.on")) == "FALSE") "No change" else sprintf("%+g \u00b0C", d) }
  if (!identical(tmp(a), tmp(b))) add("Temperature change", tmp(a), tmp(b))
  cvn <- function(v) { k <- g(v, "temp.curve"); if (k %in% names(TEMP_CURVES)) sprintf("%s (from %s \u00b0C)", TEMP_CURVES[[k]]$label, g(v, "temp.ref")) else "Generic per-degree changes" }
  if (tmp(a) != "No change" || tmp(b) != "No change") if (!identical(cvn(a), cvn(b))) add("Temperature response", cvn(a), cvn(b))
  cr <- function(v) { k <- v[grepl("^corr\\.", names(v))]; k <- k[nzchar(k)]; if (!length(k)) "None" else paste(sort(unname(k)), collapse = "; ") }
  if (!identical(cr(a), cr(b))) add("Linked assumptions", cr(a), cr(b))
  ids <- union(active_ids_for(g(a, "model"), g(a, "structure")), active_ids_for(g(b, "model"), g(b, "structure")))
  for (id in ids) {
    sa <- spec_from_snap(a, id); sb <- spec_from_snap(b, id)
    txt <- function(sp) if (is.null(sp)) "Not used" else paste0(sp$dist, ": ", describe_spec(sp))
    if (is.null(sa) != is.null(sb) || (!is.null(sa) && !same_spec(sa, sb))) add(labels[[id]], txt(sa), txt(sb))
  }
  if (!length(rows)) return(data.frame(Setting = character(), A = character(), B = character()))
  do.call(rbind, rows)
}

# The share of the variance in y that one input explains on its own (its first-order, or main-effect, index): the variance of
# the average y within slices of x, divided by the total variance. It is estimated from the run itself, so it needs no extra
# model runs and works when inputs are linked. The bias that comes from using slices is removed. Interactions between
# inputs are not credited to any one input, so the shares usually add up to less than 100%.
first_order_index <- function(x, y, bins = 20L) {
  n <- length(y); sst <- sum((y - mean(y))^2)
  if (n < 30 || sst == 0 || sd(x) == 0) return(0)
  B  <- max(5L, min(bins, n %/% 50L))
  br <- unique(quantile(x, seq(0, 1, length.out = B + 1), names = FALSE))
  if (length(br) < 3) return(0)
  g  <- cut(x, br, include.lowest = TRUE)
  m  <- tapply(y, g, mean); k <- tapply(y, g, length)
  ssb <- sum(k * (m - mean(y))^2, na.rm = TRUE)
  bias <- (length(br) - 2) / (n - 1)                  # what slicing alone would explain by chance
  max(0, min(1, (ssb / sst - bias) / (1 - bias)))
}

# ---- Plots and summaries shared by the screen and the downloadable report ----
sens_contrib <- function(res) {
  d      <- res$draws
  varied <- names(d)[sapply(d, function(v) sd(v) > 0)]
  if (length(varied) == 0 || sd(res$ct) == 0) return(NULL)
  rho <- sapply(varied, function(v) cor(d[[v]], res$ct, method = "spearman"))
  vari <- sort(100 * vapply(varied, function(v) first_order_index(d[[v]], res$ct), 0))
  list(rho = rho, contrib = sort(100 * sign(rho) * rho^2 / sum(rho^2)), vari = vari,
       prcc = prcc_calc(d[varied], res$ct))
}

# Where the heaviest part of the forecast comes from. Looks at the highest 1% of trials (at least 10) and asks, for each
# varying assumption, where its draws in those trials sit within all of its draws (its percentile). An input whose draws in
# the tail sit near the bottom or top is driving the tail. `heavy` is TRUE when the 99th percentile of Ct is at least five
# times the median.
tail_info <- function(res) {
  ct <- res$ct; d <- res$draws; n <- length(ct)
  varied <- names(d)[sapply(d, function(v) sd(v) > 0)]
  if (n < 100 || !length(varied) || sd(ct) == 0) return(NULL)
  q <- quantile(ct, c(0.5, 0.99), names = FALSE)
  pct <- as.data.frame(lapply(d[varied], function(v) rank(v, ties.method = "average") / n))
  top <- order(ct, decreasing = TRUE)[seq_len(max(10L, ceiling(0.01 * n)))]
  med <- vapply(pct, function(p) median(p[top]), 0)
  drivers <- data.frame(id = varied, med = med, ext = abs(med - 0.5) * 2, row.names = NULL)
  list(heavy = q[1] > 0 && q[2] / q[1] >= 5, ratio = if (q[1] > 0) q[2] / q[1] else Inf, varied = varied, pct = pct,
       top = top, top5 = order(ct, decreasing = TRUE)[1:5], drivers = drivers[order(-drivers$ext), ])
}

ordinal <- function(p) {
  k <- min(max(round(100 * p), 1), 99)
  sfx <- if (k %% 100 %in% 11:13) "th" else switch(as.character(k %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
  paste0(k, sfx)
}

# The same draws run through the other population age structure. Going to the stable age distribution needs a growth rate
# and a first-bite age, which a synchronous run does not have, so the app's default values are used.
ct_other_structure <- function(res, from, progress = function(done, n) {}, chunk = 250L) {
  d <- res$draws; n <- nrow(d); to_stable <- identical(from, "synchronous")
  r_i   <- if (to_stable) rep(pop_specs$growth_r$value, n)
  sig_i <- if (to_stable) rep(pmin(pmax(round(pop_specs$first_bite$value), 0), N_CLASS - 1), n)
  ct <- numeric(n)
  for (from_i in seq(1L, n, by = chunk)) {
    i <- from_i:min(from_i + chunk - 1L, n)
    ct[i] <- ct_batch(res$model, d$mort_a[i], res$b[i], res$s[i], d$n_eip[i], d$m_dens[i] * d$a_bite[i]^2,
                      d$vec_comp[i], if (to_stable) r_i[i], if (to_stable) sig_i[i])
    progress(max(i), n)
  }
  ct
}

# A bold title at the left edge of the whole figure instead of centred over the plot area
fig_title <- function(txt, line = 1)
  mtext(txt, side = 3, line = line, at = grconvertX(0.01, "ndc", "user"), adj = 0, font = 2, cex = 1.1, col = .pv$fg, xpd = NA)

draw_sens <- function(res, metric = "prcc", sc = sens_contrib(res)) {
  pvec_par()
  if (is.null(sc)) {
    plot.new(); text(0.5, 0.5, "No assumptions are varying, so there is nothing to rank")
    return(invisible())
  }
  par(mar = c(5, 17, 3, 2))
  if (identical(metric, "prcc")) {
    pr   <- sc$prcc[order(sc$prcc$est), ]
    vals <- setNames(pr$est, pr$id)
    xl   <- range(c(0, pr$lo, pr$hi)); xl <- xl + c(-1, 1) * 0.12 * diff(xl)
    cols <- ifelse(vals > 0, ACCENT, "#D55E00")
    mp <- barplot(vals, horiz = TRUE, las = 1, names.arg = labels[pr$id], xlim = xl, col = NA, border = NA,
                  xlab = "Partial rank correlation (PRCC) with Ct, with 95% interval",
                  main = "Sensitivity of Ct to each assumption")
    grid(nx = NULL, ny = NA, col = .pv$grid, lty = 1)
    barplot(vals, horiz = TRUE, add = TRUE, axes = FALSE, names.arg = NA, col = cols, border = NA)
    arrows(pr$lo, mp, pr$hi, mp, angle = 90, code = 3, length = 0.04, lwd = 1.2)
    text(ifelse(vals > 0, pr$hi, pr$lo), mp, sprintf("%.2f", vals), pos = ifelse(vals > 0, 4, 2), cex = 0.8, xpd = NA)
    abline(v = 0)
    return(invisible())
  }
  vals <- switch(metric, rho = sort(sc$rho), var = sc$vari, sc$contrib)
  xl <- range(c(0, vals)); xl <- xl + c(-1, 1) * 0.2 * diff(xl)
  cols <- ifelse(vals > 0, ACCENT, "#D55E00")
  mp <- barplot(vals, horiz = TRUE, las = 1, names.arg = labels[names(vals)], xlim = xl, col = NA, border = NA,
                xlab = switch(metric, rho = "Rank correlation with Ct (Spearman rho)",
                                      var = "Share of the variance in Ct explained by the assumption alone (%)",
                                      "Share of squared rank correlation (%)"),
                main = "Sensitivity of Ct to each assumption")
  if (identical(metric, "var"))
    mtext(sprintf("The bars add up to %.0f%%; the rest comes from assumptions acting together.", sum(vals)), side = 1, line = 4.2, cex = 0.8, col = .pv$muted)
  grid(nx = NULL, ny = NA, col = .pv$grid, lty = 1)
  barplot(vals, horiz = TRUE, add = TRUE, axes = FALSE, names.arg = NA, col = cols, border = NA)
  text(vals, mp, if (identical(metric, "rho")) sprintf("%.2f", vals) else sprintf("%.1f%%", vals),
       pos = ifelse(vals > 0, 4, 2), cex = 0.8, xpd = NA)
  abline(v = 0)
}

# Ct against one assumption's drawn values, with a smooth trend and the rank correlation
draw_scatter <- function(x, ct, label) {
  pvec_par()
  par(mar = c(5, 5, 4, 2))
  k <- if (length(x) > 3000) sample.int(length(x), 3000) else seq_along(x)
  plot(x[k], ct[k], pch = 16, cex = 0.6, col = adjustcolor(ACCENT, 0.35), xlab = label, ylab = "Ct",
       main = sprintf("Ct against %s", label))
  if (diff(range(x)) > 0) {
    lines(lowess(x, ct), lwd = 3, col = "#D55E00")
    legend("topright", bty = "n", lty = 1, lwd = 3, col = "#D55E00",
           legend = sprintf("Trend (rank correlation %.2f)", suppressWarnings(cor(x, ct, method = "spearman"))))
  }
}

# Median lifespan of each trial's mosquitoes (the age when half have died), for up to the first 2,000 trials
lifespans <- function(res) {
  n <- min(2000, nrow(res$draws))
  vapply(seq_len(n), function(i) {
    lt <- life_table(hazard(res$model, AGES, res$draws$mort_a[i], res$b[i], res$s[i]))
    k <- which(lt$lx < 0.5)[1]
    if (is.na(k)) NA_real_ else k - 1
  }, 0)
}

draw_lifespans <- function(ls) {
  pvec_par()
  ls <- ls[!is.na(ls)]
  if (!length(ls)) { plot.new(); text(0.5, 0.5, "Mosquitoes live too long here for a median lifespan to be found"); return(invisible()) }
  par(mar = c(5, 5, 4, 2))
  hist(ls, breaks = max(10, min(40, length(unique(ls)))), col = ACCENT, border = .pv$bg, main = sprintf("Median lifespan across %s trials", format(length(ls), big.mark = ",")),
       xlab = "Median lifespan (days)", ylab = "Number of trials")
  q <- quantile(ls, c(0.025, 0.5, 0.975))
  abline(v = q[2], lwd = 2, lty = 2); abline(v = q[c(1, 3)], lwd = 1.5, lty = 3, col = .pv$red)
  legend("topright", bty = "n", lty = c(2, 3), lwd = c(2, 1.5), col = c(.pv$fg, .pv$red),
         legend = c(sprintf("Median %s days", fmt3(q[2])), sprintf("95%% of trials: %s to %s days", fmt3(q[1]), fmt3(q[3]))))
}

# One assumption's drawn values. The small version is a tile on the page; the large version adds
# the median and 95% range, and the distribution that was used.
draw_one <- function(x, label, big = FALSE, spec = NULL) {
  pvec_par()
  if (diff(range(x)) == 0) {                       # fixed: every trial used the same value
    par(mar = if (big) c(5, 5, 6, 2) else c(3, 3, 3, 1))
    w <- max(abs(x[1]) * 0.1, 0.01)
    plot(NA, xlim = x[1] + c(-w, w), ylim = c(0, 1), yaxt = "n", xlab = if (big) label else "", ylab = "",
         main = label, cex.main = if (big) 1.3 else 0.95)
    segments(x[1], 0, x[1], 0.8, lwd = 4, col = ACCENT)
    text(x[1], 0.9, sprintf("Fixed at %s", fmt3(x[1])), cex = if (big) 1 else 0.8)
    return(invisible())
  }
  if (!big) {
    par(mar = c(3, 3.2, 3, 1), mgp = c(1.8, 0.6, 0), cex.main = 0.95)
    hist(x, breaks = 30, col = ACCENT, border = .pv$bg, main = label, xlab = "", ylab = "Frequency")
    abline(v = median(x), lwd = 1.6, lty = 2, col = .pv$fg)
    return(invisible())
  }
  par(mar = c(5, 5, 6, 2))
  hist(x, breaks = 50, col = ACCENT, border = .pv$bg, main = "", xlab = label, ylab = "Number of trials")
  title(main = label, line = 3.6, cex.main = 1.3)
  if (!is.null(spec)) mtext(spec, side = 3, line = 1.6, cex = 0.85, col = .pv$muted)
  q <- quantile(x, c(0.025, 0.5, 0.975))
  abline(v = q[2], lwd = 2, lty = 2); abline(v = q[c(1, 3)], lwd = 1.5, lty = 3, col = .pv$red)
  legend("topright", bty = "n", lty = c(2, 3), lwd = c(2, 1.5), col = c(.pv$fg, .pv$red),
         legend = c(sprintf("Median %s", fmt3(q[2])), sprintf("2.5th and 97.5th percentiles: %s and %s", fmt3(q[1]), fmt3(q[3]))))
}

draw_draws <- function(d) {
  pvec_par()
  par(mfrow = c(ceiling(ncol(d) / 3), 3))
  for (v in names(d))
    hist(d[[v]], breaks = 40, col = ACCENT, border = .pv$bg, main = labels[[v]], xlab = "")
}

# Life tables for the first 100 trials (the lines drawn on the Survival curves tab)
surv_tables <- function(res) {
  lapply(seq_len(min(100, nrow(res$draws))), function(i)
    life_table(hazard(res$model, AGES, res$draws$mort_a[i], res$b[i], res$s[i])))
}

draw_surv <- function(res, view = "both", lts = surv_tables(res), k = 100, xmax = 80, hl = NULL) {
  pvec_par()
  lts  <- lts[seq_len(min(k, length(lts)))]
  x    <- 0:xmax
  both <- identical(view, "both")
  if (both) par(mfrow = c(1, 2))
  if (view %in% c("both", "surv")) {
    plot(NA, xlim = range(x), ylim = c(0, 1), xlab = "Age (days)", ylab = "Survivorship (fraction alive)",
         main = sprintf("Survivorship, first %d trials", length(lts)), las = 1)
    grid(col = .pv$grid, lty = 1)
    for (lt in lts) lines(x, lt$lx[x + 1], col = adjustcolor(ACCENT, 0.18))
    lines(x, apply(sapply(lts, function(lt) lt$lx[x + 1]), 1, median), lwd = 3, col = .pv$fg)
    ne <- median(res$draws$n_eip[seq_along(lts)]); abline(v = ne, lty = 3, lwd = 2, col = "#D55E00")
    if (!is.null(hl) && hl <= length(lts)) lines(x, lts[[hl]]$lx[x + 1], lwd = 3, col = "#7b2cbf")
    legend("topright", bty = "n", lty = c(1, 3), lwd = c(3, 2), col = c(.pv$fg, "#D55E00"), cex = 0.9,
           legend = c("Median of the trials", sprintf("Median incubation period, %s days", fmt3(ne))))
  }
  if (view %in% c("both", "hazard")) {
    ymax <- max(sapply(lts, function(lt) max(lt$u[x + 1])))
    plot(NA, xlim = range(x), ylim = c(0, ymax), xlab = "Age (days)", ylab = "Daily mortality hazard",
         main = sprintf("Daily mortality hazard, first %d trials", length(lts)), las = 1)
    grid(col = .pv$grid, lty = 1)
    for (lt in lts) lines(x, lt$u[x + 1], col = adjustcolor("#D55E00", 0.18))
    lines(x, apply(sapply(lts, function(lt) lt$u[x + 1]), 1, median), lwd = 3, col = .pv$fg)
    if (!is.null(hl) && hl <= length(lts)) lines(x, lts[[hl]]$u[x + 1], lwd = 3, col = "#7b2cbf")
    legend("topleft", bty = "n", lty = 1, lwd = 3, col = .pv$fg, legend = "Median of the trials", cex = 0.9)
  }
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
  pvec_par()
  uni <- sub(" median$", "", grep("^Uniform .* median$", names(tbl), value = TRUE)[1])
  runs <- list(list(label = uni,                nudge =  0.2, col = adjustcolor("#E69F00", 0.75)),
               list(label = "Literature-based", nudge = -0.2, col = adjustcolor(ACCENT, 0.75)))
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
    abline(h = y, col = .pv$grid, lwd = 6)
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
         pch = c(5, 16, 15, 15), col = c(.pv$fg, .pv$fg, runs[[1]]$col, runs[[2]]$col), pt.cex = c(1.5, 1, 1.8, 1.8))
}

# The comparison never changes (fixed trials, seed and spread), so a copy computed ahead of time ships in
# www/ and loads instantly. deploy.R rebuilds it, and the tests fail if it no longer matches the model.
# If the copy is missing or unreadable, the table is computed instead.
PAPER_CACHE <- "www/pvec_comparison.csv"
# The precomputed table, or NULL when the copy is missing, unreadable or the wrong shape
paper_cached <- function(cache = PAPER_CACHE) {
  r <- if (file.exists(cache)) tryCatch(read.csv(cache, check.names = FALSE, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.data.frame(r) && nrow(r) == 6 && ncol(r) == 13) r else NULL
}
paper_table <- function(step = function(i, k) {}, cache = PAPER_CACHE) {
  r <- paper_cached(cache)
  if (!is.null(r)) return(r)
  paper_comparison(step = step)
}

build_report <- function(res, bounds, prev = NULL, run_labels = NULL, thresh = NA, summaries = NULL) {
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
         "Histogram of total vectorial capacity (Ct) across all trials.", "forecast"),
    list("Sensitivity", function() draw_sens(res), 1000, 600,
         "Bar chart of the partial rank correlation (PRCC) between each assumption and Ct, with 95% intervals.", "sens"),
    list("Sensitivity: share of variance", function() draw_sens(res, "var"), 1000, 600,
         "Bar chart of the share of the variance in Ct that each assumption explains on its own.", NA),
    list("Assumption draws", function() draw_draws(res$draws), 1000, 800,
         "Histograms of the values drawn for each assumption.", "draws"),
    list("Survival curves", function() draw_surv(res), 1100, 500,
         "Survivorship and daily mortality hazard for the first 100 trials.", "surv"))
  fig_html <- vapply(figs, function(f) {
    b64 <- png_b64(f[[2]], f[[3]], f[[4]])
    sm <- if (!is.na(f[[6]]) && !is.null(summaries[[f[[6]]]]) && nzchar(summaries[[f[[6]]]]))
      paste0("<div class='summary'><b>Quick summary.</b> ", summaries[[f[[6]]]],
             "<div class='muted'>This summary is generated automatically, so please excuse any mistakes. The data below is always the better summary.</div></div>") else ""
    paste0("<h2>", esc(f[[1]]), "</h2>", sm,
           if (is.null(b64)) "<p><i>Figure not available in this environment. Use the Download plot buttons in the app.</i></p>"
           else sprintf("<img alt=\"%s\" src=\"data:image/png;base64,%s\">", esc(f[[5]]), b64))
  }, "")
  paste0(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'><title>Vectorial capacity report</title><style>",
    "body{font-family:Helvetica,Arial,sans-serif;max-width:900px;margin:30px auto;padding:0 16px;color:#222;line-height:1.45}",
    "h1{font-size:24px;margin-bottom:4px}h2{font-size:18px;margin-top:30px;border-bottom:1px solid #ddd;padding-bottom:4px}",
    "table{border-collapse:collapse;font-size:14px;margin:8px 0}td,th{border:1px solid #ddd;padding:4px 10px;text-align:left}",
    "th{background:#f3f5f7}img{max-width:100%;height:auto}.muted{color:#666;font-size:13px}",
    ".summary{background:#eaf2fb;border-left:4px solid #2b6cb0;padding:8px 14px;margin:10px 0}.summary .muted{margin-top:6px;font-style:italic}</style></head><body>",
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

# The "Dig deeper" section under the forecast: a row of buttons, and one panel open at a time
tool_tab <- function(id, ic, label)
  tags$button(type = "button", class = "tool-tab", `data-panel` = id, `aria-expanded` = "false", `aria-controls` = paste0("tool_", id), icon(ic), span(label))
tool_panel <- function(id, title, ...)
  div(class = "tool-panel", id = paste0("tool_", id),
      div(class = "tool-panel-in", div(class = "tool-card", h4(class = "tool-card-title", title), ...)))

# One "Download" menu per tab: a button that drops up a list of the downloads for that tab
dl_menu <- function(...)
  div(class = "btn-group dropup dl-menu no-print",
      tags$button(type = "button", class = "btn btn-default btn-sm dropdown-toggle", `data-toggle` = "dropdown",
                  `aria-haspopup` = "true", `aria-expanded` = "false", icon("download"), " Download ", span(class = "caret")),
      tags$ul(class = "dropdown-menu", role = "menu", ...))
dl_img_item <- function(target, file, label = "Plot image (PNG)", cls = "dl-img")
  tags$li(tags$a(href = "#", class = cls, `data-target` = target, `data-file` = file, label))

# "How to read" help: a button with a ? at the start that opens a short explanation
how_to <- function(title, ...) tags$details(class = "how-to-read", tags$summary(title), ...)

# The two presets side by side, built from the specs the app itself uses (mortality shown for the logistic model)
preset_table <- function() {
  ids <- c(names(vc_specs), names(mort_specs$logistic), names(pop_specs))
  base <- c(vc_specs, mort_specs$logistic, pop_specs)
  data.frame(Assumption = unname(labels[ids]),
             `Literature preset` = vapply(ids, function(id) { sp <- base[[id]]; sp$dist <- lit_dists[[id]]; paste0(sp$dist, ": ", describe_spec(sp)) }, ""),
             `Fixed preset` = vapply(ids, function(id) { sp <- base[[id]]; sp$dist <- "Fixed"; paste0("Fixed: ", describe_spec(sp)) }, ""),
             check.names = FALSE, row.names = NULL, stringsAsFactors = FALSE)
}

# Tiny preview of one assumption's distribution, drawn inside its card
preview_plot <- function(s) {
  pvec_par()
  par(mar = c(1.6, 0.4, 0.2, 0.4), mgp = c(1, 0.25, 0), tcl = -0.2, cex.axis = 0.75)
  if (s$dist == "Fixed") {
    w <- max(abs(s$value) * 0.15, 0.01)
    plot(NA, xlim = s$value + c(-w, w), ylim = c(0, 1), yaxt = "n", xlab = "", ylab = "", bty = "n")
    segments(s$value, 0, s$value, 1, lwd = 3, col = ACCENT)
  } else if (s$dist == "Uniform") {
    plot(NA, xlim = c(s$min, s$max), ylim = c(0, 1.25), yaxt = "n", xlab = "", ylab = "", bty = "n")
    polygon(c(s$min, s$min, s$max, s$max), c(0, 1, 1, 0),
            col = adjustcolor(ACCENT, 0.5), border = ACCENT)
  } else {
    x  <- qdraw(ppoints(1000), s)     # evenly spaced quantiles: a stable shape, and no random numbers used
    dn <- density(x, from = min(x), to = max(x), adjust = 1.3)
    plot(dn$x, dn$y, type = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
    polygon(c(dn$x[1], dn$x, tail(dn$x, 1)), c(0, dn$y, 0),
            col = adjustcolor(ACCENT, 0.5), border = ACCENT)
  }
}

# ---- Assumption box UI ----
shows <- function(id, dists)
  sprintf("['%s'].indexOf(input.%s_dist) > -1", paste(dists, collapse = "','"), id)

# Size of one arrow-key or spinner step, matched to each assumption's scale
steps <- c(a_bite = 0.01, n_eip = 1, m_dens = 0.01, vec_comp = 0.01,
           mort_a = 0.0001, mort_b = 0.001, mort_s = 0.01, growth_r = 0.01, first_bite = 1)

# autocomplete = "off" stops the browser refilling these boxes from an earlier visit, which can scramble Min and Max
num <- function(id, f, label, s)
  htmltools::tagQuery(numericInput(paste0(id, "_", f), label, s[[f]], width = "100%",
               step = if (f %in% c("shape1", "shape2")) 0.1 else steps[[id]]))$
    find("input")$addAttrs(autocomplete = "off")$allTags()

# Open on the literature-based distributions rather than fixed point values
with_default_dist <- function(id, s) { s$dist <- lit_dists[[id]]; s }

# A lone variable letter in a label (the a in "Mortality a") is set like it is in the equations: serif italic
math_label <- function(txt) {
  parts <- regmatches(txt, regexec("^(.*[ ])([abrs])($| \\(.*$)", txt))[[1]]
  if (length(parts) == 0) return(txt)
  tagList(parts[2], span(class = "mvar", parts[3]), parts[4])
}

assumption_ui <- function(id, s) {
  s <- with_default_dist(id, s)
  wellPanel(class = "assump-card", id = paste0("card_", id),
    div(class = "assump-head",
      tags$button(type = "button", class = "assump-toggle", `aria-expanded` = "false",
                  `aria-controls` = paste0("body_", id),
                  span(class = "chev", icon("chevron-right")), strong(math_label(labels[[id]]))),
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
      div(title = "Optional note on where these values come from, such as a paper and table. It is kept with your results for reference and does not change the simulation.",
        textInput(paste0(id, "_source"), "Source", "", width = "100%", placeholder = "e.g. Author (year), table 2"))
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
CITE_TEXT <- sprintf("Strand, J. R. (%s). PVEC: Probabilistic Vectorial Capacity Simulator (Version %s) [Computer software]. %s",
                     CITE_YEAR, APP_VERSION, CITE_URL)
CITE_BIBTEX <- sprintf("@software{strand_pvec,\n  author  = {Strand, Jackson R.},\n  title   = {{PVEC}: Probabilistic Vectorial Capacity Simulator},\n  year    = {%s},\n  version = {%s},\n  url     = {%s}\n}",
                       CITE_YEAR, APP_VERSION, CITE_URL)

# Shown on the results tabs until the first run; hidden by CSS once results exist
# "Used for" chips under a source: each jumps to the assumption card (or tab) the source supports
src_uses <- function(cards = character(), tabs = character())
  div(class = "src-uses", span("Used for: "),
      lapply(cards, function(id) tags$button(type = "button", class = "src-link", `data-card` = id, labels[[id]])),
      lapply(names(tabs), function(t) tags$button(type = "button", class = "src-link", `data-tab` = tabs[[t]], t)))

MIN_TRIALS <- 10
MAX_TRIALS <- 50000

empty_state <- function(msg)
  div(class = "empty-state",
      div(class = "empty-title", "No results yet"),
      div(class = "empty-text", msg),
      tags$button(type = "button", class = "btn btn-primary empty-run", icon("play"), " Run simulation"))

ui <- fluidPage(
  titlePanel(div(class = "app-brand",
                 tags$img(src = "pvec_logo.svg", alt = "PVEC", class = "app-logo logo-light"),
                 tags$img(src = "pvec_logo_dark.svg", alt = "PVEC", class = "app-logo logo-dark"),
                 tags$img(src = "pvec_mosquito.svg", alt = "", class = "app-mosquito"),
                 span(class = "app-brand-text", "Probabilistic Vectorial Capacity Simulator"),
                 tags$button(id = "theme_toggle", type = "button", class = "theme-toggle", `aria-label` = "Switch between light and dark theme",
                             title = "Switch between light and dark theme", icon("moon"), icon("sun"))),
             windowTitle = "PVEC: Probabilistic Vectorial Capacity Simulator"),
  tags$head(tags$link(rel = "icon", type = "image/svg+xml", href = "pvec_icon.svg")),
  tags$head(tags$script(HTML("(function(){var t=null;try{t=localStorage.getItem('vc_theme');}catch(e){}if(!t){t=(window.matchMedia&&window.matchMedia('(prefers-color-scheme: dark)').matches)?'dark':'light';}document.documentElement.setAttribute('data-theme',t);})();"))),
  tags$button(id = "expand_sidebar", type = "button", class = "sidebar-arrow-open",
              title = "Show settings", `aria-label` = "Show settings", `aria-expanded` = "false",
              icon("chevron-right")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = asset_url("pvec.css"))),
  div(class = "print-only", textOutput("run_status_print")),
  sidebarLayout(
    div(class = "col-sm-3",
     tags$form(class = "well", role = "complementary",
      div(class = "settings-head",
        h4("Simulation settings", style = "margin: 0;"),
        tags$button(id = "collapse_sidebar", type = "button", class = "sidebar-arrow",
                    title = "Hide settings", `aria-label` = "Hide settings", `aria-expanded` = "true",
                    icon("chevron-left"))),
      radioButtons("mort_model",
        span("Mortality model ",
          span(class = "help-tip", tabindex = "0", role = "note", `aria-label` = "About the mortality model", "?",
            span(class = "help-tip-text",
              "How a mosquito's daily chance of dying changes as it ages.",
              br(), br(),
              strong("Logistic: "), "risk rises with age, then levels off at old ages (uses a, b and s).",
              br(), br(),
              strong("Gompertz: "), "risk keeps rising steadily with age, with no levelling off (uses a and b).",
              br(), br(),
              strong("Exponential: "), "the same risk every day, whatever the age (uses a only)."))),
        c("Logistic" = "logistic", "Gompertz" = "gompertz", "Exponential" = "exponential")),
      radioButtons("structure",
        span("Population age structure ",
          span(class = "help-tip", tabindex = "0", role = "note", `aria-label` = "About population age structure", "?",
            span(class = "help-tip-text",
              strong("Stable: "), "the population has settled into a steady mix of ages (0 to 199 days). Only mosquitoes old enough to have taken a first bite contribute to Ct.",
              br(), br(),
              strong("Synchronous: "), "all mosquitoes hatch together as one cohort, and Ct averages the daily capacity over ages 3 to 6 days."))),
        c("Stable" = "stable", "Synchronous" = "synchronous")),
      fluidRow(
        column(6, div(class = "trials-box", textInput("n_iter", "Trials", "1,000"))),
        column(6, numericInput("seed", HTML(paste0("Seed ", as.character(actionLink("rand_seed", icon("shuffle"), title = "Pick a random seed")))), 1))),
      tags$details(class = "temp-section",
        tags$summary(span(class = "temp-name", "Temperature"), span(class = "temp-optional", "optional"), span(class = "temp-badge-wrap", `aria-live` = "polite", span(class = "temp-badge", id = "temp_badge")),
          # On/off switch: the checkbox holds the state (so the server can set it), the button is what people click
          tags$input(type = "checkbox", id = "temp_on", class = "temp-sw-input", tabindex = "-1", `aria-hidden` = "true"),
          tags$button(type = "button", class = "temp-switch", role = "switch", `aria-checked` = "false", `aria-label` = "Apply the temperature change",
                      title = "Turn the temperature change on or off without losing its settings",
                      span(class = "temp-sw-knob"))),
        div(class = "temp-body",
          radioButtons("temp_curve", "Vector and pathogen",
                       choiceNames = list("Generic (editable)", tagList(tags$i("Aedes aegypti"), " \u00b7 dengue virus"),
                                          tagList(tags$i("Anopheles"), " \u00b7 ", tags$i("P. falciparum"))),
                       choiceValues = c("generic", "aedes_dengue", "anopheles_pf"), selected = "generic"),
          div(class = "temp-slider is-zero",
            div(class = "temp-head", span(class = "temp-head-label", "Change in temperature"),
                span(class = "temp-value",
                     tags$input(type = "text", id = "temp_value", class = "temp-input", value = "0", inputmode = "decimal", autocomplete = "off",
                                maxlength = "6", `aria-label` = "Change in temperature in degrees Celsius. Type a value from minus 8 to 8.",
                                title = "Type a value from \u22128 to 8"),
                     span(class = "temp-unit", "\u00b0C"))),
            # The value lives in a hidden number box (so Shiny, saving and presets treat it as a normal input); pvec.js draws the slider
            tags$input(type = "number", id = "temp_delta", class = "temp-delta-input", value = 0, min = -8, max = 8, step = 0.1,
                       tabindex = "-1", `aria-hidden` = "true"),
            div(class = "tslider", id = "temp_slider",
                div(class = "ts-track", div(class = "ts-fill")),
                div(class = "ts-zero"),
                tags$button(type = "button", class = "ts-handle", role = "slider", `aria-label` = "Change in temperature, degrees Celsius",
                            `aria-valuemin` = "-8", `aria-valuemax` = "8", `aria-valuenow` = "0", `aria-valuetext` = "No change")),
            div(class = "ts-scale", `aria-hidden` = "true", span("\u22128"), span("0"), span("8")),
            div(class = "temp-chips", role = "group", `aria-label` = "Common temperature changes",
                lapply(c(-4, -2, 0, 2, 4), function(v)
                  tags$button(type = "button", class = "temp-chip", `data-v` = v,
                              if (v == 0) "None" else paste0(if (v > 0) "+" else "\u2212", abs(v)))))),
          uiOutput("temp_effect", class = "smooth-h"),
          tags$details(class = "fit-help temp-sens",
            tags$summary("Baseline and settings"),
            div(class = "smooth-h",
            conditionalPanel("input.temp_curve != 'generic'",
              numericInput("temp_ref", "Baseline temperature (\u00b0C)", 27, step = 0.5, width = "100%"),
              p(class = "temp-note", "The temperature your assumptions describe. The slider moves away from it.")),
            conditionalPanel("input.temp_curve == 'generic'",
              numericInput("temp_pe", "Incubation period (% per \u00b0C)", -10, step = 1, width = "100%"),
              numericInput("temp_pm", "Initial mortality a (% per \u00b0C)", 5, step = 1, width = "100%"),
              numericInput("temp_pa", "Biting rate (% per \u00b0C)", 3, step = 1, width = "100%"),
              p(class = "temp-note", "Rough placeholders, not fitted curves. Pick a vector and pathogen above to use published curves.")))))),
      radioButtons("preset",
        span("Assumption presets ",
          span(class = "help-tip", tabindex = "0", role = "note", `aria-label` = "About the assumption presets", "?",
            span(class = "help-tip-text",
              strong("Literature: "), "each assumption is drawn from a probability distribution whose range comes from published estimates. Biting rate and vector competence use Beta, incubation period and mosquito density use Uniform, and the mortality parameters use truncated Normal. Results vary from trial to trial, so the forecast shows a distribution of Ct.",
              br(), br(),
              strong("Fixed: "), "every assumption is a single value, so every trial gives the same Ct. Use it as a deterministic baseline to see how much the uncertainty changes the result. You can still change any distribution by hand."))),
        choices = c("Literature" = "lit",
                    "Fixed" = "fixed"),
        selected = "lit"),
      # Both short descriptions sit in one box whose height follows the chosen one (see pvec.js), so switching
      # presets fades the text and eases the controls below up or down instead of snapping
      div(class = "preset-stack", id = "preset_stack",
        div(class = "preset-pane on", `data-preset` = "lit",
          p(class = "preset-desc",
          HTML(paste0("Ranges from published estimates (", as.character(actionLink("goto_about", "see Presets")), ").")))),
        div(class = "preset-pane", `data-preset` = "fixed",
          p(class = "preset-desc", "One value per assumption: a baseline with no spread."))),
      div(class = "preset-vary", role = "status", `aria-live` = "polite", id = "preset_vary"),
      actionButton("reset_open", tagList(icon("rotate-left"), " Reset..."), class = "btn-default btn-sm reset-open", width = "100%")
     ),
     div(class = "sidebar-dock",
     div(class = "run-dock",
        actionButton("run", "Run simulation", class = "btn-primary btn-lg", width = "100%",
                     title = "Shortcut: Cmd or Ctrl + Enter"),
        div(class = "run-issues smooth-h", id = "run_issues", role = "status", `aria-live` = "polite"),
        uiOutput("stale_note"),
        div(class = "run-status", textOutput("run_status"))),
     div(class = "history-box",
        div(class = "history-header",
            div(class = "history-text",
                div(class = "history-title-row", icon("clock-rotate-left"), span(class = "history-title", " Past runs")),
                div(class = "history-sub", "Click a run to reload its settings.")),
            span(class = "hist-actions",
                 tags$button(type = "button", class = "hist-icon scen-dlall", `aria-label` = "Download all past runs",
                             `data-tip` = "Download all past runs as one table (CSV)", icon("file-arrow-down")),
                 tags$button(type = "button", class = "hist-icon scen-clear", `aria-label` = "Clear all past runs",
                             `data-tip` = "Remove all past runs from the list (you can undo)", icon("trash-can")))),
        uiOutput("scenario_list", class = "smooth-h")))
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
            div(class = "howto-head",
              div(class = "howto-title", "Getting started"),
              tags$button(type = "button", id = "tour_start", class = "btn btn-default btn-sm tour-btn", icon("compass"), " Take a 1-minute tour")),
            div(class = "howto-steps",
              howto_step(1, "Choose assumptions", "Pick a preset in Simulation settings, or open the cards below to edit them."),
              howto_step(2, "Set up the run", "Choose the mortality model, number of trials and a seed."),
              howto_step(3, "Run and read", "Click ", strong("Run simulation"), " (or press ",
                         tags$kbd("Cmd/Ctrl + Enter"), "), then open the Forecast tab.")),
            tags$button(type = "button", class = "howto-close", `aria-label` = "Hide getting started", title = "Hide getting started",
                        span(class = "x", HTML("&times;")), span(class = "lbl", "Hide"))),
          tags$button(type = "button", id = "howto_open", class = "link-btn howto-open",
                      icon("circle-info"), " Getting started"),
          div(class = "settings-io",
            downloadButton("dl_settings", "Save inputs", class = "btn-sm"),
            tags$button(id = "copy_link", type = "button", class = "btn btn-default btn-sm fade-btn",
                        title = "Copies a link that restores these settings",
                        span(class = "fb-a", icon("share-nodes"), " Share settings"),
                        span(class = "fb-b", `aria-live` = "polite",
                             span(class = "fb-icon", icon("link")), span(class = "fb-msg"))),
            fileInput("load_settings", NULL, buttonLabel = tagList(icon("upload"), " Upload inputs"), placeholder = "",
                      accept = ".csv", width = "auto")),
          div(class = "bulk-note", role = "status", div(class = "bulk-note-in",
            "Most inputs changed since the last run (for example by switching the preset or model). Click Run to update the results.")),
          div(class = "card-legend", div(class = "card-legend-in",
            span(class = "lg-edit", "Edited from the preset"), span(class = "lg-run", "Changed since the last run"),
            span(class = "lg-up", "Uses uploaded draws"))),
          div(class = "assump-section", id = "sec_transmission",
            h4("Transmission", span(class = "sec-count")),
            p(class = "sec-desc", "The inputs that set how readily a mosquito population can spread infection."),
            div(class = "cards-grid", lapply(names(vc_specs), function(id) assumption_ui(id, vc_specs[[id]])))),
          div(class = "assump-section", id = "sec_mortality",
            h4("Mortality schedule", span(class = "sec-count")),
            p(class = "sec-desc", "How quickly mosquitoes die as they age. The cards shown depend on the mortality model."),
            uiOutput("ab_hint", class = "smooth-h"),
            div(class = "cards-grid",
              assumption_ui("mort_a", mort_specs$logistic$mort_a),
              conditionalPanel("input.mort_model != 'exponential'", assumption_ui("mort_b", mort_specs$logistic$mort_b)),
              conditionalPanel("input.mort_model == 'logistic'", assumption_ui("mort_s", mort_specs$logistic$mort_s)))),
          div(class = "assump-section", id = "sec_population",
            h4("Population age structure", span(class = "sec-count")),
            p(class = "sec-desc", "The age mix of the mosquito population. Used only with the stable age distribution."),
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
              uiOutput("corr_list", class = "smooth-h"),
              tags$hr(),
              div(class = "settings-io",
                fileInput("up_file", NULL, buttonLabel = "Upload parameter draws (CSV)", placeholder = "",
                          accept = ".csv", width = "auto"),
                downloadButton("dl_template", "Download template", class = "btn-sm")),
              uiOutput("up_status", class = "smooth-h")))))),
        tabPanel("Forecast",
          empty_state("Run a simulation to see the forecast of Ct, the chance it exceeds a threshold, and the summary statistics."),
          div(class = "results-body",
          uiOutput("ctx_forecast"),
          uiOutput("insight_forecast"),
          uiOutput("forecast_summary", class = "smooth-h"),
          how_to("How to read this chart",
            p("Each bar counts how many trials gave a Ct in that range. The dashed line is the median. Dark bars fall inside",
              "the certainty range you set (all bars when none is set). Drag across the chart to set the range, or type it.",
              "Type a threshold, or click the chart to place one, to see the chance that Ct exceeds it.",
              "More bins show finer detail but a noisier shape.")),
          div(class = "plot-card",
            div(class = "plot-controls no-print",
              numericInput("cert_lo", "Range from", NA, step = 0.1, width = "150px"),
              numericInput("cert_hi", "Range to", NA, step = 0.1, width = "150px"),
              numericInput("thresh", "Threshold", NA, step = 0.1, width = "150px"),
              sliderInput("bins", "Bins", 10, 100, 50, step = 5, width = "170px", ticks = FALSE)),
            uiOutput("cert_text", class = "smooth-h"),
            div(class = "chart-hint no-print", `data-hint` = "forecast", icon("hand-pointer"), span("Tip: drag across the chart to set a range, or click it to place a threshold."),
                tags$button(type = "button", class = "chart-hint-x", `aria-label` = "Dismiss this tip", HTML("&times;"))),
            div(class = "tip-cell",
              plotOutput("forecast_plot", height = 400, click = "forecast_click",
                         brush = brushOpts("forecast_brush", direction = "x", resetOnNew = FALSE, delay = 300, delayType = "debounce",
                                           fill = "#2b6cb0", opacity = 0.15, stroke = "#2b6cb0")),
              uiOutput("forecast_reset", class = "reset-float")),
            div(class = "dl-row",
              dl_menu(dl_img_item("forecast_plot", "forecast_Ct.png"),
                      tags$li(downloadLink("dl_csv", "Results (CSV)")),
                      tags$li(downloadLink("dl_report", "Report (HTML)"))))),
          div(class = "tools no-print",
            div(class = "tools-head", span(class = "tools-title", "Dig deeper"), span(class = "tools-sub", "More views of this forecast. Pick one to open it.")),
            div(class = "tool-tabs", role = "group", `aria-label` = "More views of this forecast",
              tool_tab("extreme", "arrow-up-wide-short", "Most extreme trials"),
              tool_tab("r0", "calculator", "Convert to R\u2080"),
              tool_tab("struct", "layer-group", "Other age structure"),
              tool_tab("compare", "code-compare", "Compare to another run")),
            div(class = "tool-panels",
              tool_panel("extreme", "Most extreme trials",
                p(class = "tab-lead", "The five trials with the highest Ct, with every input's value and where it sits among that assumption's draws."),
                uiOutput("extreme_ui", class = "smooth-h")),
              tool_panel("r0", "Convert Ct to R\u2080 (optional)",
                p(class = "tab-lead", "R\u2080 here is the number of new human infections one infectious person causes through mosquitoes: Ct \u00d7 the chance an infectious bite infects a person \u00d7 the days a person is infectious. These two numbers are extra assumptions, not part of the model."),
                div(class = "plot-controls",
                  numericInput("r0_b", "Chance an infectious bite infects a person (0 to 1)", NA, min = 0, max = 1, step = 0.05, width = "230px"),
                  numericInput("r0_dur", "Days a person is infectious", NA, min = 0, step = 1, width = "190px")),
                uiOutput("r0_ui", class = "smooth-h")),
              tool_panel("struct", "Run the same draws on the other age structure",
                p(class = "tab-lead", "Runs the same parameter draws through the other population age structure, so the difference you see is the structure alone."),
                actionButton("struct_go", "Run the other age structure", class = "btn-default btn-sm"),
                uiOutput("struct_out", class = "smooth-h")),
              tool_panel("compare", "Compare to another run",
                p(class = "tab-lead", "Pick any two of your runs, not only the latest. The first is drawn in blue and the second in orange."),
                div(class = "plot-controls",
                  selectInput("compare_a", "Run", choices = NULL, width = "360px"),
                  selectInput("compare_run", "Compare it with", c("Choose a run" = "none"), width = "360px")),
                uiOutput("compare_out", class = "smooth-h")))),
          div(class = "table-tools", copy_btn("stats")),
          div(class = "stats-table", tableOutput("stats")))),
        tabPanel("Sensitivity",
          empty_state("Run a simulation to see which assumptions move Ct the most."),
          div(class = "results-body",
          uiOutput("ctx_sens"),
          uiOutput("insight_sens"),
          p(class = "tab-lead", "Which assumptions move Ct the most? Blue bars raise Ct and orange bars lower it. Click a driver, or a bar, to see Ct plotted against that assumption."),
          uiOutput("sens_top"),
          how_to("How to read this chart",
            p("The default, the partial rank correlation coefficient (PRCC), is the rank correlation between one",
              "assumption and Ct after removing the effect of all the others, from -1 to 1, with a 95% interval.",
              "Assumptions that are fixed do not vary and are left out. If you have linked assumptions or uploaded",
              "draws, read the values with care: the inputs are no longer independent. The model itself has no random",
              "noise, so PRCC values are often large; compare their order and sign more than their size.")),
          div(class = "plot-card",
            div(class = "no-print seg-wrap",
              radioButtons("sens_metric", NULL, inline = TRUE, selected = "prcc",
                           c("PRCC" = "prcc", "Rank correlation" = "rho", "Share of squared correlation" = "contrib", "Share of variance" = "var"))),
            uiOutput("sens_caption", class = "smooth-h"),
            div(class = "tip-cell",
              plotOutput("sens_plot", height = 400, click = "sens_click")),
            div(class = "dl-row", dl_menu(dl_img_item("sens_plot", "sensitivity.png")))))),
        tabPanel("Assumption draws",
          empty_state("Run a simulation to see the values drawn for each assumption."),
          div(class = "results-body",
          uiOutput("ctx_draws"),
          uiOutput("insight_draws"),
          p(class = "tab-lead", "The values each assumption took across all trials. Click any card to enlarge it."),
          div(class = "draws-toolbar no-print",
              div(class = "seg-js", role = "group", `aria-label` = "Show assumptions",
                  tags$button(type = "button", class = "on", `data-filter` = "all", "All"),
                  tags$button(type = "button", `data-filter` = "vary", "Varying"),
                  tags$button(type = "button", `data-filter` = "fixed", "Fixed")),
              div(class = "seg-sort", role = "group", `aria-label` = "Order",
                  tags$button(type = "button", class = "on", `data-sort` = "default", "Default order"),
                  tags$button(type = "button", `data-sort` = "effect", "Strongest effect first")),
              span(class = "draws-count", role = "status", `aria-live` = "polite"),
              dl_menu(dl_img_item("draws_grid", "assumption_draws.png", "Shown plots (PNG)", cls = "dl-grid"))),
          uiOutput("draws_grid"))),
        tabPanel("Survival curves",
          empty_state("Run a simulation to see survivorship and mortality curves for the first 100 trials."),
          div(class = "results-body",
          uiOutput("ctx_surv"),
          uiOutput("insight_surv"),
          p(class = "tab-lead", "How long mosquitoes live under each trial's mortality assumptions. Each faint line is one trial. Hover a line to see that trial's values."),
          uiOutput("surv_tiles"),
          how_to("How to read these charts",
            p("Survivorship is the fraction of mosquitoes still alive at each age. The daily mortality hazard is the chance",
              "that a mosquito of that age dies that day. The dark line is the median across the first 100 trials, and the",
              "dotted line marks the median extrinsic incubation period: a mosquito must live past it to transmit.")),
          div(class = "plot-card",
            div(class = "no-print seg-wrap",
              radioButtons("surv_view", NULL, inline = TRUE, selected = "both",
                           c("Both" = "both", "Survivorship" = "surv", "Hazard" = "hazard"))),
            div(class = "plot-controls no-print",
              selectInput("surv_n", "Lines shown", c("10" = 10, "25" = 25, "50" = 50, "100 (all)" = 100), 100, width = "140px"),
              sliderInput("surv_age", "Ages shown (days)", 10, 150, 80, step = 10, width = "220px", ticks = FALSE)),
            div(class = "chart-hint no-print", `data-hint` = "survival", icon("hand-pointer"), span("Tip: drag across a chart to zoom the age axis."),
                tags$button(type = "button", class = "chart-hint-x", `aria-label` = "Dismiss this tip", HTML("&times;"))),
            div(class = "surv-wrap", id = "surv_wrap",
              uiOutput("surv_reset", class = "reset-float"),
              conditionalPanel("input.surv_view != 'hazard'", class = "combine-img surv-cell",
                plotOutput("surv_plot_s", height = 450, hover = hoverOpts("surv_hover_s", delay = 120, delayType = "debounce"),
                           brush = brushOpts("surv_brush_s", direction = "x", resetOnNew = TRUE, delay = 400, delayType = "debounce",
                                             fill = "#2b6cb0", opacity = 0.15, stroke = "#2b6cb0")),
                uiOutput("surv_tip_s")),
              conditionalPanel("input.surv_view != 'surv'", class = "combine-img surv-cell",
                plotOutput("surv_plot_h", height = 450, hover = hoverOpts("surv_hover_h", delay = 120, delayType = "debounce"),
                           brush = brushOpts("surv_brush_h", direction = "x", resetOnNew = TRUE, delay = 400, delayType = "debounce",
                                             fill = "#2b6cb0", opacity = 0.15, stroke = "#2b6cb0")),
                uiOutput("surv_tip_h"))),
            div(class = "dl-row", dl_menu(dl_img_item("surv_wrap", "survival_curves.png", cls = "dl-grid")))),
          div(class = "plot-card",
            h4(class = "mc-head", "How much does lifespan vary between trials?"),
            p(class = "tab-lead", "Each trial has its own median lifespan, the age when half its mosquitoes have died. The spread shows how much the mortality assumptions matter."),
            plotOutput("surv_life_plot", height = 340),
            div(class = "dl-row", dl_menu(dl_img_item("surv_life_plot", "median_lifespan.png")))))),
        tabPanel("Model check",
          h4(class = "mc-head", "Check against published values"),
          p(class = "tab-lead", "The deterministic model is run at the parameter values Styer et al. (2007) report and compared with their published vectorial capacity."),
          div(class = "stat-tiles",
            div(class = "stat-tile is-lead", div(class = "tile-label", "Largest difference"),
                div(class = "tile-value", sprintf("%.1f%%", max_dev)), div(class = "tile-sub", "from any published value")),
            div(class = "stat-tile", div(class = "tile-label", "Within 1%"),
                div(class = "tile-value", sprintf("%d of %d", sum(abs(unlist(validation[, c("Sync. diff (%)", "Stable diff (%)")])) < 1),
                                                   2 * nrow(validation))),
                div(class = "tile-sub", "published values matched")),
            div(class = "stat-tile", div(class = "tile-label", "Average difference"),
                div(class = "tile-value", sprintf("%.1f%%", mean(abs(unlist(validation[, c("Sync. diff (%)", "Stable diff (%)")]))))),
                div(class = "tile-sub", "across all six"))),
          tags$details(class = "how-to-read",
            tags$summary("How this check works"),
            p("Published values are never used as inputs, except that the growth rate r is solved from the exponential",
              "stable-age case, so that row matches by construction and the other five are independent checks. Published",
              "values are rounded to one decimal place, so small differences are expected.")),
          div(class = "table-tools", copy_btn("validation")),
          div(class = "clean-table", style = "overflow-x: auto;", tableOutput("validation")),
          div(class = "mc-legend", span(class = "mc-pass", "\u2713 within 1%"), span(class = "mc-warn", "~ within 5%"), span(class = "mc-fail", "\u2717 5% or more")),
          h4(class = "mc-head", "Comparison for the paper"),
          p(class = "tab-lead", "Published, deterministic and two probabilistic runs side by side, for each mortality model and age structure."),
          tags$details(class = "how-to-read",
            tags$summary("What each column is"),
            p("The value published by Styer et al. (2007); this model run deterministically at their parameter values; a",
              "probabilistic run in which every assumption in use is drawn uniformly within plus or minus 20% of that same",
              "value; and a probabilistic run with the literature-based distributions (the default preset). The uniform run",
              "is centred on the deterministic one, so its difference from the deterministic value is the effect of parameter",
              "uncertainty alone. 10,000 trials, random seed 2026. In the uniform run the growth rate r and first-bite age are",
              "varied too in the stable age distribution (the first-bite age is rounded to a whole day when used). The table is",
              "precomputed with exactly these settings, and the tests check it against the model, so Run comparison loads it at once.")),
          div(class = "dl-row mc-actions",
              actionButton("run_paper", "Run comparison", class = "btn-primary btn-sm"),
              downloadButton("dl_paper", "Download CSV", class = "btn-sm"),
              copy_btn("paper_tbl")),
          div(class = "clean-table two-text", style = "overflow-x: auto;", tableOutput("paper_tbl")),
          tags$details(class = "how-to-read",
            tags$summary("How to read the two probabilistic runs"),
            p("The uniform run is centred on the deterministic value, so it isolates the effect of uncertainty. The",
              "literature-based run is a different scenario: its distributions are centred on literature values for biting",
              "rate, mosquito density and vector competence that are lower than the values Styer et al. used (for example, a",
              "mean biting rate of about 0.45 per day against their 0.75), so its Ct is far lower. Do not read that gap as an",
              "effect of uncertainty. The growth rate r and first-bite age have no published range, so they stay fixed in that",
              "run. The default r is solved so that the exponential, stable-age case reproduces the published value, so that",
              "one row matches by construction; the other five rows are independent checks.")),
          div(class = "plot-card", uiOutput("paper_plot_card"))),
        tabPanel("About",
          div(class = "about",
          div(class = "jump-bar", role = "navigation", `aria-label` = "Jump to a section",
            jump_chip("about_quick", "In one minute"), jump_chip("about_cite", "Cite"),
            jump_chip("about_what", "Overview"), jump_chip("about_model", "Model"), jump_chip("about_presets", "Presets"),
            jump_chip("about_methods", "Methods"), jump_chip("about_sources", "Sources"),
            jump_chip("about_author", "Author")),
          div(class = "about-card about-section about-callout", id = "about_quick",
            h4("In one minute"),
            tags$ul(class = "quick-list",
              tags$li(strong("What it is. "), "A simulator that shows how uncertain mosquito and transmission inputs spread into uncertainty in vectorial capacity (Ct)."),
              tags$li(HTML("<strong>How to use it.</strong> Pick a preset or edit the assumptions, press <strong>Run simulation</strong>, then read the Forecast tab.")),
              tags$li(strong("What you get. "), "A distribution of Ct, which inputs drive it, the values drawn, and how long the mosquitoes live."),
              tags$li(strong("What to trust. "), "The Model check tab reproduces Styer et al. (2007) within about 0.5%. The default distributions come from the literature, not from field data for one place."))),
          div(class = "about-card about-section about-callout cite-card", id = "about_cite",
            h4("How to cite"),
            p(class = "cite-text", CITE_TEXT),
            div(class = "cite-actions",
              tags$button(type = "button", class = "btn btn-default btn-sm fade-btn cite-btn", `data-cite` = CITE_TEXT,
                          span(class = "fb-a", icon("quote-right"), " Copy citation"), span(class = "fb-b", `aria-live` = "polite")),
              tags$button(type = "button", class = "btn btn-default btn-sm fade-btn cite-btn", `data-cite` = CITE_BIBTEX,
                          span(class = "fb-a", icon("file-lines"), " Copy BibTeX"), span(class = "fb-b", `aria-live` = "polite")))),
          div(class = "about-card about-section", id = "about_what",
          h4("What this tool does"),
          p(class = "about-lead", "PVEC (Probabilistic VECtorial capacity) propagates uncertainty in transmission and mosquito mortality parameters",
            "through an age-specific vectorial capacity model. Each assumption can be fixed or",
            "given a probability distribution; the simulation draws parameter sets at random and",
            "reports the resulting distribution of vectorial capacity (Ct), along with a",
            "sensitivity ranking of the inputs."),
          p(strong("Ct"), "is total vectorial capacity: age-specific vectorial capacity combined",
            "across the age structure of the mosquito population (a stable age distribution or",
            "synchronous emergence).")),
          div(class = "about-card about-section", id = "about_model",
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
            "Sensitivity is the partial rank correlation of each input with ", v("C"), sub_("t"), ".")))))),
          div(class = "about-card about-section", id = "about_presets",
            h4("Presets"),
            p("A preset sets every assumption at once. Pick one in the side panel, then change any assumption by hand."),
            tags$ul(
              tags$li(strong("Literature. "), "Each assumption is drawn from a probability distribution whose range comes from published estimates, as described in the accompanying paper. ",
                      "Biting rate and vector competence use Beta distributions, incubation period and mosquito density use Uniform, and the mortality parameters use truncated Normal distributions. ",
                      "The growth rate and the age at first bite have no published range, so they stay fixed. Results vary from trial to trial, so the forecast shows a distribution of Ct."),
              tags$li(strong("Fixed. "), "Every assumption is set to a single value, so every trial gives the same Ct. This is the deterministic model that the Model check tab compares with Styer et al. (2007), ",
                      "and a baseline for seeing how much the uncertainty changes the result. The mortality values are the ones fitted by Styer et al. for the chosen mortality model, and the growth rate is solved so the exponential, stable-age case matches their published Ct.")),
            how_to("Show every assumption's values",
              p(class = "preset-table-lead", "The values below are for the logistic mortality model; the exponential and Gompertz models use their own fitted mortality values."),
              div(class = "clean-table preset-table", style = "overflow-x: auto;", HTML(html_table(preset_table()))))),
          div(class = "about-card about-section", id = "about_methods",
          h4("Methods notes"),
          tags$ul(
            tags$li(strong("How a trial works. "), "Each trial draws one value for every assumption, runs the age-specific model, and records Ct."),
            tags$li(strong("Linking assumptions. "), "Assumptions are drawn independently unless you link them. A rank correlation between two",
                    "assumptions is imposed with a Gaussian copula, which leaves each assumption's own",
                    "distribution unchanged. If several requested correlations cannot all hold together they are",
                    "reduced by the same fraction and the run reports it."),
            tags$li(strong("Uploaded draws. "), "Uploaded draws are used as whole rows, so any correlation in the file is kept. Rows are sampled",
                    "without replacement when the file has at least as many rows as trials, otherwise with replacement."),
            tags$li(strong("Growth rate and first bite. "), "The growth rate r and the age at first bite apply to the stable age distribution and can be given",
                    "distributions like any other assumption. The default r is solved to reproduce a published",
                    "Ct, so treat it as a calibration, not a field estimate."),
            tags$li(strong("Sensitivity. "), "Sensitivity uses partial rank correlation coefficients (PRCC) with 95% intervals. The older",
                    "share-of-squared-correlation view is a rough guide, not a variance decomposition."))),
          div(class = "about-card about-section", id = "about_sources",
          h4("Sources"),
          tags$ol(class = "about-refs",
            tags$li("Macdonald G (1957) The Epidemiology and Control of Malaria. Oxford University Press. ",
                    tags$a(href = "https://archive.org/details/in.ernet.dli.2015.549644/page/n11/mode/2up", target = "_blank", "Internet Archive"),
                    src_uses(c("a_bite", "m_dens", "vec_comp", "n_eip"))),
            tags$li("Garrett-Jones C (1964) Prognosis for interruption of malaria transmission through assessment of the mosquito's vectorial capacity. Nature 204:1173-1175. ",
                    tags$a(href = "https://doi.org/10.1038/2041173a0", target = "_blank", "https://doi.org/10.1038/2041173a0"),
                    src_uses(c("a_bite", "m_dens", "vec_comp", "n_eip"))),
            tags$li("Styer LM, Carey JR, Wang J-L, Scott TW (2007) Mosquitoes do senesce: departure from the paradigm of constant mortality. Am J Trop Med Hyg 76:111-117. ",
                    tags$a(href = "https://doi.org/10.4269/ajtmh.2007.76.111", target = "_blank", "https://doi.org/10.4269/ajtmh.2007.76.111"),
                    src_uses(c("mort_a", "mort_b", "mort_s", "growth_r"), c("Model check" = "Model check")))),
          p("Parameter ranges and distributions are from the literature as described in the",
            "accompanying paper.")),
          div(class = "cite-row",
            span(class = "app-meta", paste0("Version ", APP_VERSION, ", last updated ", LAST_UPDATED))),
          div(class = "author-card about-section", id = "about_author",
            img(src = "headshot.jpg", alt = "Jackson Strand", class = "author-photo"),
            div(class = "author-text",
              div(class = "author-label", "About the author"),
              h4("Jackson R. Strand"),
              p("PhD student, Montana State University"),
              p(class = "author-note",
                "Jackson is an entomologist who studies insect ecology and biological control, with a background in chemical ecology and plant-insect interactions. He frequently works with Bayesian statistics and simulation in R, and he built PVEC to make probabilistic vectorial capacity forecasts accessible to anyone who wants to explore how parameter uncertainty shapes transmission risk."),
              p(class = "author-link",
                tags$a(href = "https://www.jackson-strand.com", target = "_blank",
                       rel = "noopener", "www.jackson-strand.com"))))))
      )
    )
  ),
  # The copyright year follows the last-updated date, so deploy.R keeps both current
  div(class = "app-footer",
      HTML(paste0("&copy; ", sub(".*, ", "", LAST_UPDATED), " Jackson R. Strand &nbsp;&middot;&nbsp; Last updated: ", LAST_UPDATED))),
  # Page behaviour (notification panel, cards, jump bar, share link, downloads) is in www/pvec.js,
  # and the styling is in www/pvec.css, so the browser can cache both
  tags$script(src = asset_url("pvec.js"))
)

# ---- Server ----
server <- function(input, output, session) {

  # Messages to the user appear as a toast fixed near the bottom of the window (pvec.js), never in the sidebar where they would push
  # things around. A toast can carry one action (Undo); kind is "info", "warn" or "error".
  toast <- function(text, action = NULL, input = NULL, ms = 8000, kind = "info")
    session$sendCustomMessage("toast", list(text = text, action = action, input = input, ms = ms, kind = kind))
  notify <- function(text, type = "message", duration = 6)
    toast(text, kind = switch(type, error = "error", warning = "warn", "info"), ms = max(duration, if (type == "error") 8 else 4) * 1000)

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
    if (!is.null(bad)) { notify(bad, type = "error", duration = 6); return() }
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
    if (is.null(df) || !nrow(df)) { notify("That file could not be read as a CSV with a header row.", type = "error", duration = 8); return() }
    use <- intersect(names(df), setting_ids)
    if (!length(use)) {
      notify(sprintf("No column names matched. Use these names: %s. The template has them.", paste(setting_ids, collapse = ", ")),
                       type = "error", duration = 10); return()
    }
    df[use] <- lapply(df[use], function(x) suppressWarnings(as.numeric(x)))
    probs <- unlist(lapply(use, function(id) {
      x <- df[[id]]
      if (anyNA(x) || any(!is.finite(x))) sprintf("%s has blank, non-numeric or infinite values", id)
      else if (any(x < min_ok[[id]])) sprintf("%s has values below %s", id, min_ok[[id]])
    }))
    if (length(probs)) { notify(paste("The file was not used:", paste(probs, collapse = "; ")), type = "error", duration = 10); return() }
    if (nrow(df) < 20) { notify("The file needs at least 20 rows.", type = "error", duration = 8); return() }
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
      updateRadioButtons(session, "structure", selected = get1("structure"))
    if (!is.na(num("seed")))  updateNumericInput(session, "seed", value = num("seed"))
    tr <- suppressWarnings(as.numeric(gsub(",", "", get1("trials"))))
    if (!is.na(tr) && tr >= MIN_TRIALS && tr <= MAX_TRIALS)
      updateTextInput(session, "n_iter", value = format(round(tr), big.mark = ","))
    # Temperature what-if (absent in older files and links, which means no change)
    cv <- get1("temp.curve"); updateRadioButtons(session, "temp_curve", selected = if (!is.na(cv) && cv %in% c("generic", names(TEMP_CURVES))) cv else "generic")
    if (!is.na(num("temp.ref"))) updateNumericInput(session, "temp_ref", value = num("temp.ref"))
    td <- num("temp"); updateNumericInput(session, "temp_delta", value = if (is.na(td)) 0 else max(-8, min(8, td)))
    # Files and links from before the switch existed: on whenever there was a change
    on <- toupper(get1("temp.on")); updateCheckboxInput(session, "temp_on", value = if (on %in% c("TRUE", "FALSE")) on == "TRUE" else (!is.na(td) && td != 0))
    for (k in list(c("temp.pe", "temp_pe", -10), c("temp.pm", "temp_pm", 5), c("temp.pa", "temp_pa", 3)))
      updateNumericInput(session, k[2], value = if (is.na(num(k[1]))) as.numeric(k[3]) else num(k[1]))
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

  # Every setting as one named character vector (the same keys as the settings file)
  settings_rows <- function() {
    rows <- list(model = input$mort_model, structure = input$structure,
                 trials = n_trials(), seed = input$seed,
                 temp = input$temp_delta, temp.on = isTRUE(input$temp_on), temp.curve = input$temp_curve, temp.ref = input$temp_ref,
                 temp.pe = input$temp_pe, temp.pm = input$temp_pm, temp.pa = input$temp_pa)
    cp <- corr_pairs()
    for (i in seq_len(nrow(cp))) rows[[paste0("corr.", i)]] <- sprintf("%s;%s;%s", cp$a[i], cp$b[i], cp$rho[i])
    for (id in setting_ids) {
      sp <- get_spec(id)
      rows[[paste0(id, ".dist")]] <- sp$dist
      rows[[paste0(id, ".source")]] <- input[[paste0(id, "_source")]]
      for (f in fields) rows[[paste0(id, ".", f)]] <- sp[[f]]
    }
    vapply(rows, function(x) if (is.null(x) || length(x) != 1 || is.na(x)) "" else as.character(x), "")
  }

  output$dl_settings <- downloadHandler(
    filename = function() "vectorial_capacity_settings.csv",
    content  = function(file) {
      rows <- settings_rows()
      writeLines("# Settings saved from PVEC, the Probabilistic Vectorial Capacity Simulator. Use Upload inputs to restore them.", file)
      write.table(data.frame(setting = names(rows), value = unname(rows)),
                  file, append = TRUE, sep = ",", row.names = FALSE, qmethod = "double")
    })

  # Apply a named character vector of settings (from a file or from a link)
  apply_loaded <- function(v, what, quiet = FALSE) {
    ok <- !is.null(v) && all(c("model", "structure") %in% names(v)) &&
          v[["model"]] %in% c("logistic", "gompertz", "exponential")
    if (!ok) {
      notify(sprintf("That %s is not a settings %s saved from this app.", what, what),
                       type = "error", duration = 8)
      return(invisible())
    }
    if (!identical(v[["model"]], input$mort_model)) {
      loaded(v)
      updateRadioButtons(session, "mort_model", selected = v[["model"]])
    } else apply_cfg(v)
    if (!quiet) notify("Settings loaded. Click Run to use them.", type = "message", duration = 6)
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
  # Reset opens a pop-up with the two choices
  observeEvent(input$reset_open, {
    showModal(modalDialog(
      title = "Reset", size = "s", easyClose = TRUE, footer = modalButton("Cancel"),
      actionButton("reset_preset", tagList(div(class = "ro-title", "Reset values to this preset"),
                                           div(class = "ro-desc", "Sets every assumption and the number of trials back to the preset's values.")),
                   class = "reset-option"),
      actionButton("start_over", tagList(div(class = "ro-title", "Start over"),
                                         div(class = "ro-desc", "Restores all defaults and clears the run history.")),
                   class = "reset-option")))
  })

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

  observeEvent(input$goto_about, session$sendCustomMessage("gotoAbout", list(target = "about_presets")))

  apply_preset <- function(preset) {
    all <- c(vc_specs, mort_specs[[input$mort_model]], pop_specs)
    for (id in names(all)) {
      s <- all[[id]]
      s$dist <- if (preset == "lit") lit_dists[[id]] else "Fixed"
      set_spec(id, s)
    }
  }
  observeEvent(input$preset, apply_preset(input$preset), ignoreInit = TRUE)
  # Reset values to the preset applies at once; a toast offers Undo, which puts back everything as it was
  reset_snap <- reactiveVal(NULL)
  observeEvent(input$undo_reset, {
    v <- reset_snap(); req(v)
    apply_loaded(v, "settings", quiet = TRUE); reset_snap(NULL)
  })
  observeEvent(input$reset_preset, {
    removeModal()
    reset_snap(settings_rows())
    apply_preset(input$preset)
    updateTextInput(session, "n_iter", value = "1,000")
    updateNumericInput(session, "temp_delta", value = 0)
    updateCheckboxInput(session, "temp_on", value = FALSE)
    toast("Values reset to the preset", "Undo", "undo_reset")
  })

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

  # The number of trials as typed (NA when empty or outside the allowed range)
  n_trials <- reactive({
    n <- suppressWarnings(as.numeric(gsub("[, ]", "", input$n_iter)))
    if (length(n) != 1 || is.na(n) || n < MIN_TRIALS || n > MAX_TRIALS) NA_integer_ else as.integer(round(n))
  })

  # The part of the temperature section that counts as a setting: nothing while it is off, or on the generic curve at 0 (that is
  # the same as no temperature); otherwise the change plus the settings of the chosen curve (the baseline for a published curve,
  # the percent changes for the generic one). Choosing a published curve marks the results as out of date, going back to the
  # generic curve at 0 does not.
  temp_sig <- function() {
    d <- input$temp_delta; if (is.null(d) || is.na(d)) d <- 0
    if (!isTRUE(input$temp_on)) return("none")
    if (input$temp_curve %in% names(TEMP_CURVES)) list(d, input$temp_curve, input$temp_ref)   # choosing a published curve counts as a setting
    else if (d == 0) "none"                                                                  # generic at 0 is the same as no temperature
    else list(d, "generic", input$temp_pe, input$temp_pm, input$temp_pa)
  }

  # Snapshot of everything that feeds a run, to tell when results are out of date
  cur_sig_now <- reactive({
    ids <- active_ids(input$mort_model)
    list(model = input$mort_model, structure = input$structure,
         n = n_trials(), seed = input$seed, corr = corr_pairs(), upload = uploaded()$sig,
         temp = temp_sig(),
         specs = lapply(setNames(ids, ids), get_spec))
  })
  # The warning waits for a pause in typing. A run records the exact current snapshot, not the delayed one.
  cur_sig <- debounce(cur_sig_now, 300)
  run_sig <- reactiveVal(NULL)

  # Whether the results are out of date, as a plain TRUE or FALSE. A reactiveVal only tells its readers when the value actually
  # flips, so the "settings changed" note and chip are drawn once and stay put while more settings are edited, rather than being
  # redrawn (and flashing) on every change.
  stale_flag <- reactiveVal(FALSE)
  observe(stale_flag(!is.null(run_sig()) && !identical(cur_sig(), run_sig())))

  output$stale_note <- renderUI({
    req(run_sig())
    if (stale_flag())
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
    if (is.na(n_trials()))
      problems <- c(problems, sprintf("Enter a number of trials between %s and %s", MIN_TRIALS, format(MAX_TRIALS, big.mark = ",")))
    if (!is.null(problems)) {
      notify(paste(problems, collapse = ". "), type = "error", duration = 8)
      session$sendCustomMessage("scrollToError", list())
      return()
    }

    t0 <- Sys.time()
    snap <- settings_rows()
    n  <- n_trials()
    dT <- if (!isTRUE(input$temp_on) || is.null(input$temp_delta) || is.na(input$temp_delta)) 0 else input$temp_delta
    tadj <- temperature_adjust(input$temp_curve, input$temp_ref, dT, input$temp_pe, input$temp_pm, input$temp_pa)
    adj <- if (!is.null(tadj)) tadj$mult
    run <- withProgress(message = "Running trials", value = 0,
      run_model(model, input$structure, specs, n, input$seed, pairs = corr_pairs(), uploaded = uploaded(),
                progress = function(i, n) setProgress(i / n, detail = sprintf("%d of %d", i, n)), adjust = adj))
    d <- run$draws; ct <- run$ct; b_i <- run$b; s_i <- run$s

    settings <- c(
      sprintf("PVEC (probabilistic vectorial capacity simulator), version %s", APP_VERSION),
      sprintf("Run time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      sprintf("Mortality model: %s", model),
      sprintf("Population age structure: %s", input$structure),
      sprintf("Trials: %d", n),
      sprintf("Random seed: %s", input$seed),
      if (!is.null(adj)) sprintf("Temperature what-if: %+g degrees C%s (incubation period x%.2f, initial mortality a x%.2f, biting rate x%.2f)",
                                 dT, if (input$temp_curve %in% names(TEMP_CURVES)) sprintf(" from %s C, %s", format(input$temp_ref), TEMP_CURVES[[input$temp_curve]]$label) else "",
                                 adj[["n_eip"]], adj[["mort_a"]], adj[["a_bite"]]),
      unlist(Map(function(s, l, id) {
        src <- trimws(paste(input[[paste0(id, "_source")]], collapse = ""))
        sprintf("%s: %s, %s%s", l, s$dist, describe_spec(s), if (nzchar(src)) sprintf(" [Source: %s]", src) else "")
      }, specs, labels[ids], ids)),
      if (!is.null(run$achieved))
        sprintf("Rank correlation: %s and %s, requested %+.2f, achieved %+.2f",
                labels[run$achieved$a], labels[run$achieved$b], run$achieved$rho, run$achieved$achieved),
      run$notes)

    new_run <- run_count() + 1
    entry <- list(run = new_run, ct = ct, snap = snap,
                  label = sprintf("Run %d: %s, %s, %s trials, median Ct %s", new_run, model,
                                  input$structure, format(n, big.mark = ","), signif(median(ct), 3)))
    history(c(tail(history(), 39), list(entry)))
    results_val(list(ct = ct, draws = d, model = model, b = b_i, s = s_i,
                     settings = settings, run = new_run,
                     structure = input$structure, n = n, seed = input$seed, snapshot = snap, temp = dT))
    run_sig(cur_sig_now())
    run_specs(specs)
    run_count(run_count() + 1)

    secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    msg  <- sprintf("Run %d finished at %s, %s trials in %.1f seconds",
                    run_count(), format(Sys.time(), "%I:%M:%S %p"),
                    format(n, big.mark = ","), secs)
    run_info(msg)
    scen_id(scen_id() + 1)
    q3 <- quantile(ct, c(0.025, 0.5, 0.975), names = FALSE)
    scenarios(tail(c(scenarios(), list(list(id = scen_id(), run = new_run, name = sprintf("Run %d", new_run), time = format(Sys.time(), "%I:%M %p"),
                                            snap = snap, n = n, med = q3[2], lo = q3[1], hi = q3[3], model = model, structure = input$structure))), 40))
    session$sendCustomMessage("newResults", list())

  }, ignoreNULL = FALSE)

  # ---- Suggest linking mortality a and b, which are usually estimated together ----
  ab_dismissed <- reactiveVal(FALSE)
  ab_pair <- function(cp) any((cp$a == "mort_a" & cp$b == "mort_b") | (cp$a == "mort_b" & cp$b == "mort_a"))
  output$ab_hint <- renderUI({
    if (ab_dismissed() || identical(input$mort_model, "exponential")) return(NULL)
    va <- input$mort_a_dist; vb <- input$mort_b_dist
    if (is.null(va) || is.null(vb) || va == "Fixed" || vb == "Fixed" || ab_pair(corr_pairs())) return(NULL)
    div(class = "hint-card", role = "note",
        span(class = "hint-icon", `aria-hidden` = "true", icon("link")),
        div(class = "hint-body",
            p(strong("Mortality a and b are drawn independently."), " They are usually estimated from the same data, and in published fits they tend to move in opposite directions: ",
              "a higher starting hazard goes with a slower rate of ageing. Drawing them independently can make the forecast look wider or narrower than the data support."),
            div(class = "hint-actions",
                actionButton("ab_link", "Link them (\u03c1 = \u22120.5)", class = "btn-default btn-sm"),
                actionButton("ab_dismiss", "Not now", class = "btn-link btn-sm"),
                span(class = "hint-small", "\u22120.5 is a moderate starting point. You can edit it under Linking assumptions."))))
  })
  observeEvent(input$ab_link, {
    cp <- corr_pairs()
    corr_pairs(rbind(cp, data.frame(a = "mort_a", b = "mort_b", rho = -0.5, stringsAsFactors = FALSE)))
    notify("Linked mortality a and b (\u03c1 = \u22120.5). Click Run to use it.", type = "message", duration = 6)
  })
  observeEvent(input$ab_dismiss, ab_dismissed(TRUE))

  # ---- Temperature what-if: the badge on the section header and the size of the effect ----
  observeEvent(input$temp_curve, {
    if (input$temp_curve %in% names(TEMP_CURVES)) updateNumericInput(session, "temp_ref", value = TEMP_CURVES[[input$temp_curve]]$ref)
  }, ignoreInit = TRUE)
  temp_adj <- reactive(temperature_adjust(input$temp_curve, input$temp_ref, input$temp_delta, input$temp_pe, input$temp_pm, input$temp_pa))
  output$temp_effect <- renderUI({
    cv <- input$temp_curve; a <- temp_adj()
    cite <- if (identical(cv, "aedes_dengue")) p(class = "temp-note", HTML("Curves from Liu-Helmersson et al. (2014, <i>PLoS ONE</i> 9:e89783), fitted to laboratory data for <i>Aedes aegypti</i> and dengue virus."))
            else if (identical(cv, "anopheles_pf")) p(class = "temp-note", HTML("Mortality after Martens (1997) and parasite development as 111 degree-days above 16 \u00b0C (Detinova; Macdonald 1957), for <i>Anopheles</i> and <i>Plasmodium falciparum</i>. The biting rate is left unchanged."))
            else p(class = "temp-note", "Shifts the incubation period, the initial mortality hazard (a) and the biting rate of every trial together.")
    if (is.null(a)) return(cite)
    m <- a$mult
    tagList(
      p(class = "temp-effect", sprintf("Incubation period \u00d7%.2f, initial mortality a \u00d7%.2f, biting rate \u00d7%.2f%s", m[["n_eip"]], m[["mort_a"]], m[["a_bite"]],
                                      if (cv %in% names(TEMP_CURVES)) sprintf(", from %s \u00b0C", format(input$temp_ref)) else "")),
      if (length(a$outside)) p(class = "temp-note temp-warn", sprintf("%s beyond the temperature range the published equation was fitted over, so %s held at the edge of that range.",
                                                                    paste(c(n = "The incubation period", mu = "Mortality", a = "The biting rate")[a$outside], collapse = " and "),
                                                                    if (length(a$outside) > 1) "they are" else "it is")),
      cite)
  })

  # ---- Past runs: every run is kept with its settings, so it can be renamed and reloaded ----
  scenarios <- reactiveVal(list())
  scen_id   <- reactiveVal(0)
  observeEvent(input$scenario_load, {
    sc <- Filter(function(x) x$id == input$scenario_load, scenarios())
    if (length(sc)) apply_loaded(sc[[1]]$snap, "run", quiet = TRUE)   # the run card shows its own message (see pvec.js)
  })
  # The compare icon on a past run opens "Compare to another run" with that run chosen; either run can be changed there
  observeEvent(input$scenario_compare, {
    sc <- Filter(function(x) x$id == input$scenario_compare, scenarios())
    if (!length(sc)) return()
    if (!as.character(sc[[1]]$run) %in% vapply(history(), function(e) as.character(e$run), "")) {
      notify("That run is too old to compare.", type = "warning"); return()
    }
    cmp_a(NULL); cmp_b(as.character(sc[[1]]$run))
    session$sendCustomMessage("openCompare", list())
  })
  # Reload a past run's settings and run them straight away (the page waits for the settings to settle, then presses Run)
  observeEvent(input$scenario_run, {
    sc <- Filter(function(x) x$id == input$scenario_run, scenarios())
    if (!length(sc)) return()
    apply_loaded(sc[[1]]$snap, "run", quiet = TRUE)
    session$sendCustomMessage("runSoon", list())
  })
  # Removing runs happens at once and a toast offers Undo, instead of asking first
  removed_runs <- reactiveVal(NULL)
  # Saving runs: one run's settings as a CSV that Upload inputs can read back, or every past run as one table. The file is made here
  # and handed to the browser (pvec.js, saveFile) to save.
  save_file <- function(name, text) session$sendCustomMessage("saveFile", list(name = name, text = text, mime = "text/csv"))
  csv_text <- function(df, header)
    paste(c(header, capture.output(write.table(df, sep = ",", row.names = FALSE, qmethod = "double", na = ""))), collapse = "\n")
  observeEvent(input$scenario_dl, {
    sc <- Filter(function(x) x$id == input$scenario_dl, scenarios())
    if (!length(sc)) return()
    x <- sc[[1]]; rows <- x$snap
    hdr <- c(sprintf("# Settings of %s (%s) saved from PVEC, the Probabilistic Vectorial Capacity Simulator. Use Upload inputs to restore them.", x$name, x$time),
             sprintf("# Result of this run: median Ct %s, 95%% range %s to %s", fmt3(x$med), fmt3(x$lo), fmt3(x$hi)))
    save_file(sprintf("pvec_%s_settings.csv", gsub("^_|_$", "", gsub("[^A-Za-z0-9]+", "_", x$name))),
              csv_text(data.frame(setting = names(rows), value = unname(rows)), paste(hdr, collapse = "\n")))
  })
  observeEvent(input$scenario_dl_all, {
    sc <- scenarios()
    if (!length(sc)) return(toast("There are no past runs to download yet."))
    keys <- unique(unlist(lapply(sc, function(x) names(x$snap))))
    wide <- do.call(rbind, lapply(sc, function(x) {
      base <- data.frame(run = x$run, name = x$name, time = x$time, median_Ct = signif(x$med, 6), Ct_2.5th_percentile = signif(x$lo, 6),
                         Ct_97.5th_percentile = signif(x$hi, 6), stringsAsFactors = FALSE)
      vals <- setNames(rep("", length(keys)), keys); vals[names(x$snap)] <- unname(x$snap)
      cbind(base, as.data.frame(as.list(vals), stringsAsFactors = FALSE, check.names = FALSE))
    }))
    save_file("pvec_past_runs.csv",
              csv_text(wide, "# Past runs from PVEC, one row per run. The settings columns use the same names as Save inputs."))
  })
  observeEvent(input$scenario_del, {
    sc <- Filter(function(x) x$id == input$scenario_del, scenarios())
    if (!length(sc)) return()
    run <- sc[[1]]$run
    removed_runs(list(scen = sc, hist = Filter(function(e) e$run == run, history())))
    scenarios(Filter(function(x) x$id != input$scenario_del, scenarios()))
    history(Filter(function(e) e$run != run, history()))
    toast(sprintf("Removed %s", sc[[1]]$name), "Undo", "undo_runs")
  })
  observeEvent(input$scenario_clear, {
    n <- length(scenarios()); req(n > 0)
    removed_runs(list(scen = scenarios(), hist = history()))
    scenarios(list()); history(list())
    toast(sprintf("Cleared %d past run%s", n, if (n == 1) "" else "s"), "Undo", "undo_runs")
  })
  observeEvent(input$undo_runs, {
    u <- removed_runs(); req(u)
    sc <- c(scenarios(), u$scen); scenarios(sc[order(vapply(sc, function(x) x$id, 0))])
    hs <- c(history(), u$hist);   history(hs[order(vapply(hs, function(e) as.numeric(e$run), 0))])
    removed_runs(NULL)
  })
  observeEvent(input$scenario_rename, {
    r <- input$scenario_rename; id <- as.integer(r$id); nm <- substr(trimws(r$name), 1, 40)
    scenarios(lapply(scenarios(), function(x) { if (x$id == id) x$name <- if (nzchar(nm)) nm else sprintf("Run %d", x$run); x }))
  })
  cur_snap <- debounce(reactive(settings_rows()), 500)
  output$scenario_list <- renderUI({
    sc <- rev(scenarios())
    if (!length(sc)) return(div(class = "scen-empty", "No runs yet."))
    newest <- sc[[1]]$id
    cs <- cur_snap()
    same_as_now <- function(x) !is.null(cs) && !is.null(x$snap) && nrow(snap_diff(cs, x$snap)) == 0
    item <- function(x)
      tags$li(class = "scen-item", `data-id` = x$id, tabindex = 0, role = "button", title = "Click to reload this run's settings",
              div(class = "scen-top", span(class = "scen-name", x$name),
                  span(class = "scen-tools",
                       tags$button(type = "button", class = "scen-dl", `aria-label` = "Download this run's settings", title = "Download this run's settings (CSV)", icon("download")),
                       tags$button(type = "button", class = "scen-run", `aria-label` = "Reload these settings and run", title = "Reload these settings and run now", icon("play")),
                       if (x$id != newest) tags$button(type = "button", class = "scen-cmp", `aria-label` = "Compare this run with another",
                                                       title = "Compare this run with another", icon("table-columns")),
                       tags$button(type = "button", class = "scen-edit", `aria-label` = "Rename this run", title = "Rename", icon("pen")),
                       tags$button(type = "button", class = "scen-del", `aria-label` = "Remove this run from the list", title = "Remove from the list", icon("xmark")))),
              if (x$id == newest || same_as_now(x))
                div(class = "scen-tags",
                    if (x$id == newest) span(class = "scen-tag showing", title = "The results on screen come from this run", "Showing"),
                    if (same_as_now(x)) span(class = "scen-tag same", title = "The settings on screen match this run", "Current settings")),
              div(class = "scen-sub", sprintf("%s \u00b7 %s \u00b7 %s trials \u00b7 %s", tools::toTitleCase(x$model),
                                              if (identical(x$structure, "synchronous")) "synchronous" else "stable", format(x$n, big.mark = ","), x$time)),
              div(class = "scen-sub", sprintf("Median Ct %s (95%%: %s\u2013%s)", fmt3(x$med), fmt3(x$lo), fmt3(x$hi))))
    top  <- head(sc, 1); rest <- tail(sc, -1)
    tagList(tags$ul(class = "scen-list", lapply(top, item)),
            if (length(rest)) tagList(
              tags$button(type = "button", class = "scen-more", `aria-expanded` = "false",
                          `data-show` = sprintf("Show %d earlier run%s", length(rest), if (length(rest) == 1) "" else "s"),
                          `data-hide` = "Hide earlier runs"),
              div(class = "scen-rest-wrap", div(class = "scen-rest-in", tags$ul(class = "scen-list scen-rest", lapply(rest, item))))))
  })

  # "Run N, model, structure, trials" line at the top of each results tab, with a warning when the settings have moved on
  run_context_ui <- function() renderUI({
    res <- results()
    stale <- stale_flag()
    div(class = "run-context no-print",
        span(class = "rc-main", sprintf("Run %d \u00b7 %s mortality \u00b7 %s age structure \u00b7 %s trials \u00b7 seed %s%s",
                                       res$run, tools::toTitleCase(res$model),
                                       if (identical(res$structure, "synchronous")) "synchronous" else "stable",
                                       format(res$n, big.mark = ","), res$seed,
                                       if (!is.null(res$temp) && res$temp != 0) sprintf(" \u00b7 %+g \u00b0C", res$temp) else "")),
        if (stale) span(class = "rc-stale", role = "status", "\u26a0 Settings changed since this run. ",
                        tags$a(href = "#", class = "rc-rerun", "Re-run")))
  })
  for (tab in c("forecast", "sens", "draws", "surv")) output[[paste0("ctx_", tab)]] <- run_context_ui()

  # Remove the drag box from a chart. This is done now and again once the charts have redrawn, because a redraw can bring the box back.
  clear_brushes <- function(ids) {
    for (id in ids) session$resetBrush(id)
    session$onFlushed(function() { for (id in ids) session$resetBrush(id) }, once = TRUE)
    session$sendCustomMessage("clearBrushes", as.list(sub("brush", "plot", ids)))   # the plot outputs the boxes sit on
  }

  # Dragging across the forecast chart sets the certainty range to the dragged span. The drag box stays on the chart as a marker of
  # that range, and goes away on Reset range, on a new run, or when the range is edited by hand.
  brush_range <- NULL
  observeEvent(input$forecast_brush, {
    b <- input$forecast_brush; ct <- results()$ct
    if (is.null(b) || diff(range(ct)) == 0) return()
    if (b$xmax - b$xmin < 0.01 * diff(range(ct))) return(clear_brushes("forecast_brush"))   # a click or a tiny slip, not a range
    brush_range <<- c(signif(b$xmin, 3), signif(b$xmax, 3))
    updateNumericInput(session, "cert_lo", value = brush_range[1])
    updateNumericInput(session, "cert_hi", value = brush_range[2])
    if (!is.null(thresh_before) && difftime(Sys.time(), thresh_before$time, units = "secs") < 5) {   # the drag began with a "click"
      updateNumericInput(session, "thresh", value = thresh_before$value)
      thresh_before <<- NULL
    }
  })
  observeEvent(c(input$cert_lo, input$cert_hi), {
    if (!is.null(brush_range) && !isTRUE(all.equal(c(input$cert_lo, input$cert_hi), brush_range))) {
      brush_range <<- NULL; clear_brushes("forecast_brush")
    }
  }, ignoreInit = TRUE)
  observeEvent(results_val(), { brush_range <<- NULL; clear_brushes("forecast_brush") }, ignoreInit = TRUE)

  # Clicking the forecast chart places the threshold there
  # (Shiny reports a click as soon as the mouse goes down, so the start of a drag also lands here; the brush handler undoes it)
  thresh_before <- NULL
  observeEvent(input$forecast_click, {
    x <- input$forecast_click$x
    if (!is.null(x) && is.finite(x)) {
      thresh_before <<- list(value = isolate(input$thresh), time = Sys.time())
      updateNumericInput(session, "thresh", value = signif(x, 3))
    }
  })

  # ---- "What this says": a short plain-language reading of each results tab ----
  # The sentence is built as one HTML string, so no stray spaces appear before punctuation
  insight_box <- function(...) div(class = "insight no-print",
    tags$button(type = "button", class = "insight-head", `aria-expanded` = "false",
                span(class = "insight-icon", `aria-hidden` = "true", icon("lightbulb")),
                span(class = "insight-title", "Quick summary"),
                span(class = "insight-hint", `aria-hidden` = "true"),
                span(class = "insight-chev", `aria-hidden` = "true")),
    div(class = "insight-body",
        div(class = "insight-body-in",
            p(HTML(paste0(..., collapse = ""))),
            p(class = "insight-note", "This summary is generated automatically, so please excuse any mistakes or errors. The data below is always the better summary."))))
  insight_text <- function(...) paste0(..., collapse = "")
  B   <- function(x) sprintf("<strong>%s</strong>", htmltools::htmlEscape(as.character(x)))
  pct <- function(x) sprintf("%.0f%%", 100 * x)

  # These boxes interpret the numbers shown in the tiles and charts around them; they do not repeat them
  tail_res <- reactive(tail_info(results()))

  # R0 settings: valid only when both numbers are given
  r0_pars <- reactive({
    b <- input$r0_b; D <- input$r0_dur
    if (is.null(b) || is.null(D) || is.na(b) || is.na(D) || b <= 0 || b > 1 || D <= 0) NULL else c(b = b, D = D)
  })
  output$r0_ui <- renderUI({
    pr <- r0_pars()
    if (is.null(pr)) return(helpText("Enter both numbers to see R\u2080."))
    r0 <- results()$ct * pr[["b"]] * pr[["D"]]; q <- quantile(r0, c(0.025, 0.5, 0.975))
    tagList(
      p(HTML(sprintf("R\u2080 = Ct \u00d7 %s \u00d7 %s. The median R\u2080 is <strong>%s</strong> (95%% of trials fall between %s and %s), and <strong>%s</strong> of trials have R\u2080 above 1.",
                     format(pr[["b"]]), format(pr[["D"]]), fmt3(q[2]), fmt3(q[1]), fmt3(q[3]), sprintf("%.0f%%", 100 * mean(r0 > 1))))),
      actionButton("r0_mark", "Mark R\u2080 = 1 on the forecast chart", class = "btn-default btn-sm"),
      helpText(sprintf("R\u2080 = 1 is where Ct equals %s. Some authors report the square root of this quantity.", signif(1 / (pr[["b"]] * pr[["D"]]), 3))))
  })
  observeEvent(input$r0_mark, {
    pr <- r0_pars(); req(pr)
    updateNumericInput(session, "thresh", value = signif(1 / (pr[["b"]] * pr[["D"]]), 3))
  })

  # The most extreme trials and what put them there
  output$extreme_ui <- renderUI({
    res <- results(); ti <- tail_res()
    if (is.null(ti)) return(helpText("This needs at least 100 trials with some assumptions varying."))
    d <- res$draws; fl <- function(p) if (p <= 0.05) "lo" else if (p >= 0.95) "hi" else ""
    head_row <- tags$tr(tags$th("Trial"), tags$th("Ct"), lapply(ti$varied, function(v) tags$th(labels[[v]])))
    rows <- lapply(ti$top5, function(i)
      tags$tr(tags$td(i), tags$td(class = "ex-ct", fmt3(res$ct[i])),
              lapply(ti$varied, function(v) { p <- ti$pct[[v]][i]
                tags$td(class = paste("ex-cell", fl(p)), fmt3(d[[v]][i]), span(class = "ex-pct", paste0(ordinal(p), " pct"))) })))
    strong <- ti$drivers[ti$drivers$ext >= 0.6, ]
    why <- if (nrow(strong))
      sprintf("The highest 1%% of trials mostly come from %s.",
              paste(sprintf("%s values of %s (median at the %s percentile of its draws)", ifelse(strong$med < 0.5, "low", "high"),
                            labels[strong$id], vapply(strong$med, ordinal, "")), collapse = ", and "))
    else "No single assumption puts the highest trials in the tail; they come from combinations of inputs."
    tagList(
      p(class = "tab-lead", why, if (ti$heavy) sprintf(" The 99th percentile of Ct is %s times the median, which is a heavy tail.", fmt3(ti$ratio))),
      div(style = "overflow-x:auto;", tags$table(class = "extreme-table", tags$thead(head_row), tags$tbody(rows))),
      helpText("The grey figure under each value is where it sits among all of that assumption's draws (1st is the lowest). Orange marks the lowest and highest 5%."))
  })

  # Compare the two population age structures on the same draws
  struct_res <- reactiveVal(NULL)
  observeEvent(results(), struct_res(NULL), ignoreInit = TRUE)
  observeEvent(input$struct_go, {
    res <- results()
    ct2 <- withProgress(message = "Running the other age structure", value = 0,
      ct_other_structure(res, res$structure, progress = function(i, n) setProgress(i / n)))
    struct_res(list(ct2 = ct2, from = res$structure, run = res$run))
  })
  output$struct_out <- renderUI({
    sr <- struct_res(); req(sr)
    tagList(
      if (identical(sr$from, "synchronous"))
        helpText(sprintf("The stable age distribution also needs a growth rate and a first-bite age, which this run did not use, so the defaults were used (r = %s, first bite at %s days).",
                         format(pop_specs$growth_r$value), format(pop_specs$first_bite$value))),
      plotOutput("struct_plot", height = 300),
      uiOutput("struct_table"))
  })
  output$struct_plot <- renderPlot({
    sr <- struct_res(); req(sr); res <- results()
    nm <- if (identical(sr$from, "stable")) c("Stable (this run)", "Synchronous (same draws)") else c("Synchronous (this run)", "Stable (same draws)")
    draw_forecast(res$ct, c(-Inf, Inf), sr$ct2, nm, NA, bins = 50)
  }, alt = "Histograms of Ct for the two population age structures, using the same draws.")
  output$struct_table <- renderUI({
    sr <- struct_res(); req(sr); res <- results()
    nm <- if (identical(sr$from, "stable")) c("Stable (this run)", "Synchronous (same draws)") else c("Synchronous (this run)", "Stable (same draws)")
    f <- function(x) c(fmt3(median(x)), fmt3(mean(x)), fmt3(quantile(x, 0.025)), fmt3(quantile(x, 0.975)))
    df <- data.frame(Structure = nm, rbind(f(res$ct), f(sr$ct2)), stringsAsFactors = FALSE)
    names(df) <- c("Age structure", "Median", "Mean", "2.5th percentile", "97.5th percentile")
    ratio <- if (identical(sr$from, "stable")) median(sr$ct2) / median(res$ct) else median(res$ct) / median(sr$ct2)
    tagList(div(class = "compare-table", HTML(html_table(df))),
            p(class = "tab-lead", sprintf("The median Ct under synchronous emergence is %s times the median under the stable age distribution for these draws.", fmt3(ratio))))
  })

  txt_forecast <- reactive({
    ct <- results()$ct
    if (diff(range(ct)) == 0)
      return(insight_text("Every trial gives the same Ct, because every assumption is fixed. Let at least one assumption vary to see how uncertainty spreads into the result."))
    q <- quantile(ct, c(0.025, 0.5, 0.975)); width <- if (q[1] > 0) q[3] / q[1] else Inf
    spread <- if (width < 1.5) "The forecast is tightly bounded, so the inputs leave little uncertainty about Ct."
              else if (width < 3) "The forecast has a moderate spread, so Ct is reasonably, but not tightly, pinned down."
              else if (is.finite(width)) sprintf("The forecast is wide: the high end is about %s times the low end, so the inputs leave a lot of uncertainty about Ct and a single number would hide it.", fmt3(width))
              else "The forecast is wide, so the inputs leave a lot of uncertainty about Ct."
    skew <- if (mean(ct) > 1.5 * q[2]) " A long tail of very high trials pulls the mean well above the median, so the median is the better \"typical\" value."
            else if (mean(ct) > 1.1 * q[2]) " A few high trials pull the mean above the median, so the median is the better \"typical\" value."
            else if (mean(ct) < 0.9 * q[2]) " A few low trials pull the mean below the median, so the median is the better \"typical\" value."
            else " The mean and median agree, so the distribution is fairly balanced."
    sc <- sens_res()
    drv <- if (!is.null(sc)) sprintf(" The assumption most strongly tied to Ct is %s (see the Sensitivity tab).", B(labels[[sc$prcc$id[which.max(abs(sc$prcc$est))]]]))
    ti <- tail_res()
    tailwarn <- if (!is.null(ti) && ti$heavy) {
      st <- ti$drivers[ti$drivers$ext >= 0.6, ]
      if (nrow(st)) sprintf(" Heavy tail: the highest 1%% of trials mostly come from %s, which is what stretches the high end (see Most extreme trials below).",
                            paste(sprintf("%s values of %s", ifelse(st$med < 0.5, "low", "high"), B(labels[st$id])), collapse = " and "))
      else " Heavy tail: the highest trials come from combinations of inputs rather than one input (see Most extreme trials below)."
    }
    pr <- r0_pars()
    r0s <- if (!is.null(pr)) sprintf(" With your R\u2080 settings, %s of trials have R\u2080 above 1.", B(sprintf("%.0f%%", 100 * mean(ct * pr[["b"]] * pr[["D"]] > 1))))
    insight_text(spread, skew, drv, tailwarn, r0s)
  })
  output$insight_forecast <- renderUI(insight_box(txt_forecast()))

  txt_sens <- reactive({
    sc <- sens_res()
    if (is.null(sc)) return(insight_text("No assumptions are varying, so there is nothing to rank. Let some assumptions vary on the Define assumptions tab."))
    pr <- sc$prcc[order(-abs(sc$prcc$est)), ]; k <- nrow(pr)
    if (k < 3) return(insight_text(B(labels[[pr$id[1]]]), " is the main driver of Ct. Let more assumptions vary to see how they compare."))
    vs  <- sc$vari; ord <- names(vs)[order(-vs)]; top <- head(ord, 3)
    share <- sum(vs[top]); total <- sum(vs)
    weak <- if (k >= 5 && total - share <= 10) sprintf(" The other %d explain little on their own.", k - 3)
    togeth <- if (total < 85) sprintf(" Together all the assumptions explain about %.0f%% of the variance by themselves; the rest comes from assumptions acting together, so changing several at once matters.", total)
    insight_text(sprintf("Three of the %d varying assumptions explain about %.0f%% of the variance in Ct by themselves, so better data on ", k, share),
                B(labels[[top[1]]]), " and ", B(labels[[top[2]]]), " would narrow the forecast the most.", weak, togeth,
                " The bars show how strongly Ct moves with each assumption, not how uncertain the assumption is.")
  })
  output$insight_sens <- renderUI(insight_box(txt_sens()))

  txt_draws <- reactive({
    d <- results()$draws; fixed <- vapply(d, function(x) diff(range(x)) == 0, NA)
    vary <- names(d)[!fixed]; nf <- sum(fixed)
    if (!length(vary)) return(insight_text("All ", length(d), " assumptions are fixed, so every trial used the same values and there is no uncertainty to spread."))
    rel <- vapply(vary, function(id) { q <- quantile(d[[id]], c(0.025, 0.5, 0.975)); if (q[2] == 0) NA_real_ else (q[3] - q[1]) / abs(q[2]) }, 0)
    rel <- rel[!is.na(rel)]
    sc <- sens_res()
    strongest <- if (!is.null(sc)) sc$prcc$id[which.max(abs(sc$prcc$est))]
    widest <- if (length(rel)) names(which.max(rel))
    fixed_names <- if (nf) paste(labels[names(d)[fixed]], collapse = ", ")
    mismatch <- if (!is.null(strongest) && !is.null(widest) && widest != strongest)
      sprintf(" The widest input, %s, is not the strongest driver of Ct, %s, so how much an input varies does not by itself show how much it matters.", B(labels[[widest]]), B(labels[[strongest]]))
    else if (!is.null(strongest) && !is.null(widest))
      sprintf(" %s is both the widest input and the strongest driver of Ct, so it is where better data would help most.", B(labels[[strongest]]))
    insight_text(B(length(vary)), " of ", length(d), " assumptions vary across trials",
                if (nf) sprintf("; the other %d (%s) %s fixed and add no uncertainty", nf, fixed_names, if (nf == 1) "is" else "are"), ".", mismatch)
  })
  output$insight_draws <- renderUI(insight_box(txt_draws()))

  txt_surv <- reactive({
    res <- results(); lts <- surv_lts(); k <- seq_along(lts)
    ls <- surv_life(); ls <- ls[!is.na(ls)]
    alive <- median(vapply(k, function(i) lts[[i]]$lx[res$draws$n_eip[i] + 1], 0)); ne <- median(res$draws$n_eip[k])
    verdict <- if (alive >= 0.9) sprintf("Nearly all mosquitoes outlive the %s-day incubation period, so survival is rarely what limits transmission here.", fmt3(ne))
               else if (alive >= 0.5) sprintf("A sizeable share of mosquitoes die within the %s-day incubation period, so survival limits transmission to some extent.", fmt3(ne))
               else sprintf("Most mosquitoes die within the %s-day incubation period, so survival strongly limits transmission.", fmt3(ne))
    vary <- if (length(ls) > 1 && diff(range(ls)) > 0) {
      q <- quantile(ls, c(0.025, 0.5, 0.975)); r <- (q[3] - q[1]) / q[2]
      sprintf(" Lifespan differs between trials by about %s days (95%% of trials), which is a %s spread, so the mortality assumptions %s for the result.",
              fmt3(q[3] - q[1]), if (r < 0.25) "narrow" else if (r < 0.6) "moderate" else "wide",
              if (r < 0.25) "matter little" else if (r < 0.6) "matter moderately" else "matter a lot")
    }
    insight_text(verdict, vary)
  })
  output$insight_surv <- renderUI(insight_box(txt_surv()))

  # Each new run fills the range boxes: "from" is always 0, "to" is the largest result rounded up so every trial is inside
  floor_sig <- function(x, d = 3) { if (x == 0) return(0); k <- 10^(d - 1 - floor(log10(abs(x)))); floor(x * k) / k }
  ceil_sig  <- function(x, d = 3) { if (x == 0) return(0); k <- 10^(d - 1 - floor(log10(abs(x)))); ceiling(x * k) / k }
  observeEvent(results_val(), {
    ct <- results_val()$ct
    updateNumericInput(session, "cert_lo", value = 0)
    updateNumericInput(session, "cert_hi", value = ceil_sig(max(ct)))
  })

  # "Reset range" appears once the range differs from the one a new run starts with (for example after dragging across the chart)
  output$forecast_reset <- renderUI({
    ct <- results()$ct; lo <- input$cert_lo; hi <- input$cert_hi
    req(!is.null(lo), !is.null(hi))
    if (!identical(as.numeric(c(lo, hi)), c(0, ceil_sig(max(ct)))))
      actionButton("cert_reset", tagList(icon("rotate-left"), " Reset range"), class = "btn-default btn-sm")
  })
  observeEvent(input$cert_reset, {
    brush_range <<- NULL
    clear_brushes("forecast_brush")
    updateNumericInput(session, "cert_lo", value = 0)
    updateNumericInput(session, "cert_hi", value = ceil_sig(max(results()$ct)))
  })

  cert_bounds <- reactive(c(
    if (is.na(input$cert_lo)) -Inf else input$cert_lo,
    if (is.na(input$cert_hi))  Inf else input$cert_hi))

  output$cert_text <- renderUI({
    ct <- results()$ct
    b  <- cert_bounds()
    th <- input$thresh
    parts <- list()
    if (!all(is.infinite(b)))
      parts <- c(parts, list(p(HTML(sprintf("<strong>%.1f%%</strong> chance that Ct falls in the range %s to %s",
                                        100 * mean(ct >= b[1] & ct <= b[2]),
                                        if (is.infinite(b[1])) "below" else format(b[1]),
                                        if (is.infinite(b[2])) "above" else format(b[2]))))))
    if (!is.na(th))
      parts <- c(parts, list(p(HTML(sprintf("<strong>%.1f%%</strong> chance that Ct exceeds %s",
                                        100 * mean(ct > th), format(th))))))
    if (!length(parts))
      return(helpText("Optional: enter a range (from / to) or a threshold, or click the chart to place a threshold, to see the probability of Ct landing there."))
    div(class = "cert-readout", role = "status", `aria-live` = "polite", tagList(parts))
  })

  output$forecast_summary <- renderUI({
    ct <- results()$ct
    st <- summary_text(ct)
    tile <- function(label, value, sub = NULL, lead = FALSE)
      div(class = paste("stat-tile", if (lead) "is-lead"), div(class = "tile-label", label),
          div(class = "tile-value", value), if (!is.null(sub)) div(class = "tile-sub", sub))
    if (st$fixed)
      return(tagList(span(class = "sr-only", st$headline),
                     div(class = "stat-tiles", `aria-hidden` = "true",
                         tile("Ct", fmt3(ct[1]), "every trial, all assumptions fixed", lead = TRUE),
                         tile("Trials", format(length(ct), big.mark = ",")))))
    q <- quantile(ct, c(0.025, 0.975))
    tagList(
      span(class = "sr-only", paste0("Median Ct ", st$median, ". ", st$rest)),
      div(class = "stat-tiles", `aria-hidden` = "true",
          tile("Median Ct", st$median, "middle trial", lead = TRUE),
          tile("95% range", paste0(fmt3(q[1]), "\u2013", fmt3(q[2])), "of trials fall inside"),
          tile("Mean", fmt3(mean(ct)), sprintf("SD %s", fmt3(sd(ct)))),
          tile("Trials", format(length(ct), big.mark = ","))),
      p(class = "forecast-note", st$precision))
  })

  # ---- Compare to another run: any two of the saved runs ----
  cmp_a <- reactiveVal(NULL)       # NULL follows the latest run
  cmp_b <- reactiveVal("none")
  latest_run <- reactive({ sc <- scenarios(); if (length(sc)) as.character(sc[[length(sc)]]$run) })
  run_name <- function(e) {
    sc <- Filter(function(x) x$run == e$run, scenarios())
    if (length(sc)) sc[[1]]$name else sprintf("Run %d", e$run)
  }
  get_run <- function(id) { for (e in history()) if (as.character(e$run) == id) return(e); NULL }
  run_choices <- function(exclude = NULL) {
    sc <- Filter(function(x) !(as.character(x$run) %in% exclude), rev(scenarios()))
    setNames(vapply(sc, function(x) as.character(x$run), ""),
             vapply(sc, function(x) sprintf("%s (%s, %s trials, median Ct %s)", x$name, tools::toTitleCase(x$model),
                                            format(x$n, big.mark = ","), fmt3(x$med)), ""))
  }
  observeEvent(input$compare_a, cmp_a(if (identical(input$compare_a, latest_run())) NULL else input$compare_a), ignoreInit = TRUE)
  observeEvent(input$compare_run, cmp_b(input$compare_run), ignoreInit = TRUE)
  observe({
    ch <- run_choices(); sel <- cmp_a()
    if (is.null(sel) || !sel %in% ch) sel <- latest_run()
    updateSelectInput(session, "compare_a", choices = ch, selected = sel)
  })
  observe({
    a  <- cmp_a(); if (is.null(a)) a <- latest_run()
    ch <- c("Choose a run" = "none", run_choices(exclude = a)); sel <- cmp_b()
    if (is.null(sel) || !sel %in% ch) sel <- "none"
    updateSelectInput(session, "compare_run", choices = ch, selected = sel)
  })
  run_a <- reactive({ id <- cmp_a(); if (is.null(id)) id <- latest_run(); req(id); get_run(id) })
  run_b <- reactive({ id <- cmp_b(); if (is.null(id) || identical(id, "none")) return(NULL); get_run(id) })

  output$compare_out <- renderUI({
    if (length(history()) < 2) return(helpText("Run again with different settings to have two runs to compare."))
    if (is.null(run_b())) return(helpText("Choose a run to compare with."))
    tagList(
      helpText("Bars show each run's share of trials, so runs with different numbers of trials can be compared."),
      plotOutput("compare_plot", height = 320),
      uiOutput("compare_table"),
      uiOutput("compare_diff"))
  })
  output$compare_plot <- renderPlot({
    a <- run_a(); b <- run_b(); req(a, b)
    draw_forecast(a$ct, c(-Inf, Inf), b$ct, c(run_name(a), run_name(b)), NA, bins = 50)
  }, alt = "Histograms of Ct for the two runs being compared, drawn on the same axis.")

  # What differs between the two runs, side by side
  output$compare_diff <- renderUI({
    a <- run_a(); b <- run_b(); req(a, b)
    if (is.null(a$snap) || is.null(b$snap)) return(NULL)
    df <- snap_diff(a$snap, b$snap)
    if (!nrow(df)) return(p(class = "tab-lead", "These two runs used the same settings, so any difference comes from chance alone."))
    names(df) <- c("What differs", run_name(a), run_name(b))
    tagList(h4(class = "mc-head", "What differs between these runs"),
            div(class = "compare-table diff-table", HTML(html_table(df))))
  })

  output$compare_table <- renderUI({
    a <- run_a(); b <- run_b(); req(a, b)
    q   <- function(x, p) unname(quantile(x, p))
    row <- function(label, ct) c(label, format(length(ct), big.mark = ","), fmt3(median(ct)), fmt3(mean(ct)),
                                 fmt3(q(ct, 0.025)), fmt3(q(ct, 0.975)))
    sgn <- function(x) paste0(if (x > 0) "+" else "", fmt3(x))
    df <- as.data.frame(rbind(
      row(run_name(a), a$ct), row(run_name(b), b$ct),
      c("Difference (first minus second)", "", sgn(median(a$ct) - median(b$ct)), sgn(mean(a$ct) - mean(b$ct)), "", "")), stringsAsFactors = FALSE)
    names(df) <- c("Run", "Trials", "Median", "Mean", "2.5th percentile", "97.5th percentile")
    tagList(div(class = "compare-table", HTML(html_table(df))),
            div(class = "table-tools", copy_btn("compare_table")))
  })

  output$forecast_plot <- renderPlot({
    res <- results()
    draw_forecast(res$ct, cert_bounds(), NULL, NULL, input$thresh,
                  bins = if (is.null(input$bins)) 50 else input$bins)
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
  # Wait for the values to settle: a preset or model change updates every field one by one, and checking
  # in between would flag cards whose new minimum is briefly above their old maximum
  val_inputs <- debounce(reactive(list(
    model = input$mort_model, trials = n_trials(), specs = lapply(setNames(setting_ids, setting_ids), get_spec))), 500)
  last_issues <- reactiveVal(NULL)
  observe({
    vi <- val_inputs()
    active <- active_ids(vi$model)
    errs <- vapply(setting_ids, function(id) {
      if (!id %in% active) return("")
      msg <- check_spec(vi$specs[[id]], labels[[id]])
      if (is.null(msg)) "" else substring(msg, nchar(labels[[id]]) + 2)
    }, character(1))
    if (!identical(errs, isolate(last_errs()))) {
      last_errs(errs)
      session$sendCustomMessage("assumpErr", as.list(errs))
    }
    # What would stop a run, listed under the Run button so it can be fixed before clicking
    issues <- lapply(Filter(function(id) nzchar(errs[[id]]), active), function(id) list(id = id, kind = "card", label = labels[[id]], msg = errs[[id]]))
    if (is.na(vi$trials))
      issues <- c(issues, list(list(id = "n_iter", kind = "field", label = "Trials",
                                    msg = sprintf("enter a number between %s and %s", MIN_TRIALS, format(MAX_TRIALS, big.mark = ",")))))
    if (!identical(issues, isolate(last_issues()))) {
      last_issues(issues)
      session$sendCustomMessage("runIssues", list(items = unname(issues)))
    }
    warns <- vapply(setting_ids, function(id) {
      if (!id %in% active || nzchar(errs[[id]])) return("")
      soft_warning(id, vi$specs[[id]])
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

  output$stats <- renderTable({
    d <- stats_df(results()$ct)
    d$Value <- c(format(d$Value[1], big.mark = ","),
                 vapply(d$Value[-1], function(x) format(signif(x, 4), scientific = FALSE), ""))
    d
  }, align = "lr", spacing = "s")

  # The ranking is computed once per run; switching the metric only redraws it
  sens_res <- reactive(sens_contrib(results()))
  # The three assumptions with the largest effect under the chosen measure, as tiles (click one to see its draws)
  output$sens_top <- renderUI({
    sc <- sens_res()
    if (is.null(sc))
      return(div(class = "stat-tiles", div(class = "stat-tile", div(class = "tile-label", "No drivers"),
                 div(class = "tile-sub", "No assumptions are varying, so there is nothing to rank."))))
    m <- input$sens_metric; if (is.null(m)) m <- "prcc"
    vals <- switch(m, rho = sc$rho, contrib = sc$contrib, var = sc$vari, setNames(sc$prcc$est, sc$prcc$id))
    top  <- head(vals[order(-abs(vals))], 3)
    fmt  <- if (identical(m, "contrib")) function(v) sprintf("%+.1f%%", v) else if (identical(m, "var")) function(v) sprintf("%.1f%%", v) else function(v) sprintf("%+.2f", v)
    div(class = "stat-tiles",
      lapply(seq_along(top), function(i)
        div(class = paste("stat-tile driver-tile", if (i == 1) "is-lead"), tabindex = 0, role = "button",
            `data-id` = names(top)[i], title = "Click to see this assumption's drawn values",
            div(class = "tile-label", sprintf("Driver %d", i)),
            div(class = "tile-name", labels[[names(top)[i]]]),
            div(class = "tile-value", fmt(top[[i]])),
            if (identical(m, "var")) div(class = "tile-sub", "of the variance, alone")
            else div(class = if (top[[i]] > 0) "tile-sub up" else "tile-sub down",
                     if (top[[i]] > 0) "\u25b2 raises Ct" else "\u25bc lowers Ct"))))
  })

  output$sens_caption <- renderUI({
    m <- input$sens_metric; if (is.null(m)) m <- "prcc"
    p(class = "metric-caption", switch(m,
      prcc    = "PRCC: the effect of one assumption on Ct after removing the effect of all the others, from -1 to 1.",
      rho     = "Rank correlation: how closely an assumption and Ct rise and fall together, ignoring the other assumptions, from -1 to 1.",
      contrib = "Share of squared correlation: each assumption's portion of the total squared rank correlation, in percent.",
      var     = "Share of variance: the part of the spread in Ct that one assumption explains by itself (its main effect). Shares add up to less than 100% when assumptions act together."))
  })

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
    else if (identical(m, "var"))
      sprintf("Bar chart of the share of the variance in Ct that each assumption explains alone. Largest: %s, %.0f%%.",
              labels[[names(sc$vari)[which.max(sc$vari)]]], max(sc$vari))
    else sprintf("Bar chart of each assumption's share of the squared rank correlation with Ct. Largest: %s, %.0f%%.",
                 labels[[names(sc$contrib)[which.max(abs(sc$contrib))]]], max(abs(sc$contrib)))
  }))

  # Clicking a sensitivity bar shows Ct against that assumption's drawn values
  sens_ids <- reactive({
    sc <- sens_res(); req(sc)
    m <- input$sens_metric; if (is.null(m)) m <- "prcc"
    switch(m, rho = names(sort(sc$rho)), contrib = names(sc$contrib), var = names(sc$vari), sc$prcc$id[order(sc$prcc$est)])
  })

  # Hover details for the forecast and sensitivity charts. The server works out what each bar says and where it is; the browser
  # (pvec.js) draws the highlight and the tooltip, so they follow the mouse instantly instead of waiting on a chart redraw.
  tip_html <- function(...) as.character(tagList(...))
  observe({
    ct <- results()$ct; bins <- if (is.null(input$bins)) 50 else input$bins
    bars <- list()
    if (diff(range(ct)) > 0) {
      hs <- hist(ct, breaks = bins, plot = FALSE); N <- length(ct)
      bars <- lapply(seq_along(hs$counts), function(i) list(
        x0 = hs$breaks[i], x1 = hs$breaks[i + 1], y0 = 0, y1 = hs$counts[i],
        tip = tip_html(strong(sprintf("Ct %s to %s", fmt3(hs$breaks[i]), fmt3(hs$breaks[i + 1]))),
                       div(sprintf("%s trials (%.1f%%)", format(hs$counts[i], big.mark = ","), 100 * hs$counts[i] / N)),
                       div(sprintf("%.0f%% of trials are at or below this bar", 100 * mean(ct < hs$breaks[i + 1]))))))
    }
    session$sendCustomMessage("hoverBars", list(chart = "forecast_plot", bars = bars))
  })
  observe({
    req(identical(input$tabs, "Sensitivity"))      # the ranking is costly, so it is only worked out once its tab is open
    sc <- sens_res()
    if (is.null(sc)) return(session$sendCustomMessage("hoverBars", list(chart = "sens_plot", bars = list())))
    ids <- sens_ids(); m <- input$sens_metric; if (is.null(m)) m <- "prcc"
    bars <- lapply(seq_along(ids), function(i) {
      id <- ids[i]
      val <- switch(m, rho = sc$rho[[id]], contrib = sc$contrib[[id]], var = sc$vari[[id]], sc$prcc$est[sc$prcc$id == id])
      lines <- switch(m,
        prcc = { r <- sc$prcc[sc$prcc$id == id, ]
                 list(div(sprintf("PRCC %+.2f", r$est)), div(sprintf("95%% interval %+.2f to %+.2f", r$lo, r$hi)),
                      div(if (r$est > 0) "\u25b2 raises Ct" else "\u25bc lowers Ct")) },
        rho = list(div(sprintf("Rank correlation %+.2f", val))),
        var = list(div(sprintf("%.1f%% of the variance, alone", val))),
        list(div(sprintf("%+.1f%% of squared rank correlation", val))))
      yc <- 0.7 + 1.2 * (i - 1)                            # horizontal bars are centred at 0.7, 1.9, 3.1, ... and 1 high
      list(x0 = min(0, val), x1 = max(0, val), y0 = yc - 0.5, y1 = yc + 0.5,
           tip = tip_html(strong(labels[[id]]), lines, div(class = "tip-hint", "Click to see its drawn values")))
    })
    session$sendCustomMessage("hoverBars", list(chart = "sens_plot", bars = bars))
  })

  sens_sel <- reactiveVal(NULL)
  show_scatter <- function(id) {
    req(id %in% names(results()$draws))
    sens_sel(id)
    showModal(modalDialog(title = labels[[id]], size = "l", easyClose = TRUE, footer = modalButton("Close"),
      p(class = "tab-lead", "Each dot is one trial. The orange line is the smoothed trend, so a steep line means this assumption moves Ct a lot."),
      plotOutput("sens_scatter", height = "440px")))
  }
  observeEvent(input$sens_click, {
    ids <- sens_ids(); y <- input$sens_click$y
    if (is.null(y) || !length(ids)) return()
    i <- round((y - 0.7) / 1.2) + 1                      # horizontal bars are centred at 0.7, 1.9, 3.1, ...
    if (i >= 1 && i <= length(ids)) show_scatter(ids[i])
  })
  observeEvent(input$expand_driver, show_scatter(input$expand_driver))
  output$sens_scatter <- renderPlot({
    id <- sens_sel(); req(id)
    res <- results(); req(id %in% names(res$draws))
    draw_scatter(res$draws[[id]], res$ct, labels[[id]])
  }, alt = reactive({
    id <- sens_sel(); req(id)
    sprintf("Scatter plot of Ct against the drawn values of %s, one dot per trial, with a smoothed trend line.", labels[[id]])
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
    sc  <- sens_res()
    eff <- if (is.null(sc)) numeric(0) else setNames(abs(sc$prcc$est), sc$prcc$id)
    div(class = "draws-grid", id = "draws_grid",
        lapply(seq_along(names(d)), function(i) {
          id <- names(d)[i]
          x <- d[[id]]; fixed <- diff(range(x)) == 0
          q <- quantile(x, c(0.025, 0.5, 0.975))
          e <- if (fixed || !id %in% names(eff) || is.na(eff[[id]])) 0 else eff[[id]]
          div(class = paste("draw-tile", if (fixed) "is-fixed"), tabindex = 0, role = "button", `data-id` = id,
              `data-fixed` = if (fixed) "1" else "0", `data-order` = i, `data-effect` = e,
              `aria-label` = paste("Enlarge the plot for", labels[[id]]),
              span(class = "draw-zoom", `aria-hidden` = "true", icon("expand")),
              if (fixed) div(class = "fixed-row", strong(labels[[id]]), span(class = "fixed-val", paste("Fixed at", fmt3(x[1]))))
              else plotOutput(paste0("draw_", id), height = "200px"),
              div(class = "draw-foot",
                  span(class = "draw-stats",
                       if (fixed) "Every trial uses this value"
                       else sprintf("Median %s \u00b7 95%% %s\u2013%s", fmt3(q[2]), fmt3(q[1]), fmt3(q[3]))),
                  span(class = paste("draw-badge", if (fixed) "fixed" else "vary"), if (fixed) "Fixed" else "Varies")))
        }))
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

  surv_lts <- reactive(surv_tables(results()))

  # Headline numbers for the survival curves, taken from the same first 100 trials as the lines
  output$surv_tiles <- renderUI({
    res <- results(); lts <- surv_lts(); k <- seq_along(lts)
    half  <- vapply(lts, function(lt) { i <- which(lt$lx < 0.5)[1]; if (is.na(i)) NA_real_ else i - 1 }, 0)
    ex0   <- vapply(lts, function(lt) lt$ex[1], 0)
    alive <- vapply(k, function(i) lts[[i]]$lx[res$draws$n_eip[i] + 1], 0)
    ne    <- median(res$draws$n_eip[k])
    tile  <- function(label, value, sub, lead = FALSE)
      div(class = paste("stat-tile", if (lead) "is-lead"), div(class = "tile-label", label),
          div(class = "tile-value", value), div(class = "tile-sub", sub))
    div(class = "stat-tiles",
      tile("Alive after incubation", sprintf("%.0f%%", 100 * median(alive)),
           sprintf("of mosquitoes live past %s days", fmt3(ne)), lead = TRUE),
      tile("Median lifespan", if (all(is.na(half))) "over 400 days" else sprintf("%s days", fmt3(median(half, na.rm = TRUE))),
           "age when half have died"),
      tile("Life expectancy", sprintf("%s days", fmt3(median(ex0))), "at emergence"),
      tile("Mortality model", tools::toTitleCase(res$model), "from Simulation settings"))
  })

  # Hovering a survival chart picks the nearest trial's line, highlights it and lists that trial's values
  surv_hl  <- reactiveVal(NULL)
  surv_pos <- reactiveVal(NULL)
  observeEvent(c(input$surv_n, input$surv_view, input$surv_age, results()), { surv_hl(NULL); surv_pos(NULL) })
  surv_hover <- function(h, on_surv) {
    lts <- surv_lts(); k <- min(as.integer(input$surv_n), length(lts)); xmax <- input$surv_age
    if (is.null(h) || is.null(h$x) || h$x < 0 || h$x > xmax) { surv_hl(NULL); surv_pos(NULL); return() }
    a <- min(max(round(h$x), 0), xmax)
    v <- vapply(lts[seq_len(k)], function(lt) if (on_surv) lt$lx[a + 1] else lt$u[a + 1], 0)
    span_y <- diff(unlist(h$domain[c("bottom", "top")]))
    i <- which.min(abs(v - h$y))
    if (abs(v[i] - h$y) > 0.06 * abs(span_y)) { surv_hl(NULL); surv_pos(NULL) }
    else { surv_hl(i); surv_pos(list(x = h$coords_css$x, y = h$coords_css$y, age = a, panel = if (on_surv) "s" else "h")) }
  }
  # A redraw swaps the image under the mouse, which sends an empty hover; only leaving the chart area (surv_leave) clears the highlight
  observeEvent(input$surv_hover_s, surv_hover(input$surv_hover_s, TRUE))
  observeEvent(input$surv_hover_h, surv_hover(input$surv_hover_h, FALSE))
  observeEvent(input$surv_leave, { surv_hl(NULL); surv_pos(NULL) })

  # Dragging across a survival chart zooms the age axis out to the right edge of the drag (to the nearest 10 days)
  observeEvent(c(input$surv_brush_s, input$surv_brush_h), {
    b <- if (!is.null(input$surv_brush_s)) input$surv_brush_s else input$surv_brush_h
    if (is.null(b)) return()
    updateSliderInput(session, "surv_age", value = min(150, max(10, 10 * ceiling(b$xmax / 10))))
    # Clear the drag box once the charts have redrawn, or it stays drawn over the new scale
    clear_brushes(c("surv_brush_s", "surv_brush_h"))
  }, ignoreNULL = TRUE)

  # "Reset zoom" appears once the age axis has been changed from its starting 80 days
  output$surv_reset <- renderUI({
    if (!is.null(input$surv_age) && input$surv_age != 80)
      actionButton("surv_zoom_reset", tagList(icon("rotate-left"), " Reset zoom"), class = "btn-default btn-sm")
  })
  observeEvent(input$surv_zoom_reset, {
    clear_brushes(c("surv_brush_s", "surv_brush_h"))
    updateSliderInput(session, "surv_age", value = 80)
  })

  surv_args <- function(view) draw_surv(results(), view, surv_lts(), k = as.integer(input$surv_n), xmax = input$surv_age, hl = surv_hl())
  output$surv_plot_s <- renderPlot(surv_args("surv"),
    alt = "Survivorship curves for the first trials, one line per trial. Hover a line to see that trial's values. Drag across the chart to zoom the age axis.")
  surv_life <- reactive(lifespans(results()))
  output$surv_life_plot <- renderPlot(draw_lifespans(surv_life()),
    alt = "Histogram of the median lifespan of each trial's mosquitoes, with the median and the 95 percent range marked.")
  output$surv_plot_h <- renderPlot(surv_args("hazard"),
    alt = "Daily mortality hazard curves for the first trials, one line per trial. Hover a line to see that trial's values.")

  surv_tip <- function(panel) renderUI({
    i <- surv_hl(); pos <- surv_pos(); req(i, pos, identical(pos$panel, panel))
    res <- results(); d <- res$draws; lt <- surv_lts()[[i]]
    div(class = "surv-tip", style = sprintf("left:%dpx; top:%dpx;", round(pos$x) + 14, round(pos$y) + 14),
        strong(sprintf("Trial %d", i)),
        div(sprintf("Mortality a %s", fmt3(d$mort_a[i]))),
        if (res$model != "exponential") div(sprintf("Mortality b %s", fmt3(res$b[i]))),
        if (res$model == "logistic") div(sprintf("Mortality s %s", fmt3(res$s[i]))),
        div(sprintf("Incubation %s days", fmt3(d$n_eip[i]))),
        div(sprintf("Alive at day %d: %.0f%%", pos$age, 100 * lt$lx[pos$age + 1])))
  })
  output$surv_tip_s <- surv_tip("s")
  output$surv_tip_h <- surv_tip("h")

  # These links sit inside a closed menu; keep them live so their addresses are ready when the menu opens
  local({
    session$onFlushed(function() {
      outputOptions(output, "dl_csv", suspendWhenHidden = FALSE)
      outputOptions(output, "dl_report", suspendWhenHidden = FALSE)
    }, once = TRUE)
  })

  output$dl_report <- downloadHandler(
    filename = function() "vectorial_capacity_report.html",
    content  = function(file) {
      res <- results()
      sm <- list(forecast = tryCatch(txt_forecast(), error = function(e) NULL), sens = tryCatch(txt_sens(), error = function(e) NULL),
                 draws = tryCatch(txt_draws(), error = function(e) NULL), surv = tryCatch(txt_surv(), error = function(e) NULL))
      writeLines(build_report(res, cert_bounds(), NULL, NULL, input$thresh, summaries = sm), file)
    })

  output$validation <- renderTable({
    v <- validation
    num <- vapply(v, is.numeric, NA)
    v[num] <- lapply(v[num], function(x) { x <- round(x, 2); x[x == 0] <- 0; x })   # no "-0.00"
    # The two difference columns get a mark: within 1%, within 5%, or further out
    mark <- function(x) {
      cls <- ifelse(abs(x) < 1, "mc-pass", ifelse(abs(x) < 5, "mc-warn", "mc-fail"))
      sym <- ifelse(abs(x) < 1, "\u2713", ifelse(abs(x) < 5, "~", "\u2717"))
      sprintf("<span class='%s'>%s %.2f</span>", cls, sym, x)
    }
    for (cn in c("Sync. diff (%)", "Stable diff (%)")) v[[cn]] <- mark(v[[cn]])
    v
  }, digits = 2, spacing = "s", sanitize.text.function = function(x) x)

  paper_res <- reactiveVal(paper_cached())   # shown as soon as the tab opens; Run comparison reloads or recomputes it
  observeEvent(input$run_paper, {
    withProgress(message = "Running the comparison", value = 0,
      paper_res(paper_table(step = function(i, k) incProgress(1 / k, detail = sprintf("%d of %d", i, k)))))
  })
  output$paper_tbl <- renderTable({
    r <- paper_res()
    if (is.null(r)) return(data.frame(` ` = "Click Run comparison to compute the table.", check.names = FALSE))
    r
  }, digits = 2, spacing = "s")
  # The plot needs the comparison table, so until Run comparison is clicked show a short prompt instead of blank space
  output$paper_plot_card <- renderUI({
    if (is.null(paper_res()))
      return(div(class = "plot-placeholder", "Click Run comparison above to draw this plot."))
    tagList(plotOutput("paper_plot", height = 430),
            div(class = "dl-row", dl_png("paper_plot", "pvec_comparison.png")))
  })
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
