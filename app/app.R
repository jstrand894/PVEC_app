library(shiny)

# Shown in the page footer; update when you publish a new version
LAST_UPDATED <- "October 1, 2026"
APP_VERSION  <- "1.0"

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

# Default distribution for each assumption (literature-based)
lit_dists <- c(a_bite = "Beta", n_eip = "Uniform", m_dens = "Uniform", vec_comp = "Beta",
               mort_a = "Normal", mort_b = "Normal", mort_s = "Normal")

labels <- c(a_bite   = "Biting rate (bites per day)",
            n_eip    = "Extrinsic incubation period (days)",
            m_dens   = "Mosquito density (per person)",
            vec_comp = "Vector competence",
            mort_a   = "Mortality a (initial hazard)",
            mort_b   = "Mortality b (rate of aging)",
            mort_s   = "Mortality s (deceleration)")

fields       <- c("value", "min", "mode", "max", "mean", "sd", "shape1", "shape2")
dist_choices <- c("Fixed", "Uniform", "Triangular", "PERT", "Beta", "Normal", "Lognormal")

descs <- c(a_bite   = "Bites on humans per mosquito per day.",
           n_eip    = "Days from infection to infectiousness in the mosquito.",
           m_dens   = "Female mosquitoes per human host.",
           vec_comp = "Chance a mosquito becomes infectious after an infectious blood meal.",
           mort_a   = "Daily mortality rate at emergence (per day).",
           mort_b   = "How quickly mortality rises with age (per day).",
           mort_s   = "Slows the rise in mortality at old ages (logistic model only).")

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

# ---- Plots and summaries shared by the screen and the downloadable report ----
sens_contrib <- function(res) {
  d      <- res$draws
  varied <- names(d)[sapply(d, function(v) sd(v) > 0)]
  if (length(varied) == 0 || sd(res$ct) == 0) return(NULL)
  rho <- sapply(varied, function(v) cor(d[[v]], res$ct, method = "spearman"))
  list(rho = rho, contrib = sort(100 * sign(rho) * rho^2 / sum(rho^2)))
}

draw_sens <- function(res, metric = "contrib") {
  sc <- sens_contrib(res)
  if (is.null(sc)) {
    plot.new(); text(0.5, 0.5, "No assumptions are varying, so there is nothing to rank")
    return(invisible())
  }
  vals <- if (identical(metric, "rho")) sort(sc$rho) else sc$contrib
  par(mar = c(5, 17, 3, 2))
  xl <- range(c(0, vals)); xl <- xl + c(-1, 1) * 0.2 * diff(xl)
  mp <- barplot(vals, horiz = TRUE, las = 1, names.arg = labels[names(vals)], xlim = xl,
                col = ifelse(vals > 0, "steelblue", "#D55E00"),
                xlab = if (identical(metric, "rho")) "Rank correlation with Ct (Spearman rho)" else "Contribution to variance (%)",
                main = "Sensitivity of Ct to each assumption")
  text(vals, mp, if (identical(metric, "rho")) sprintf("%.2f", vals) else sprintf("rho %.2f", sc$rho[names(vals)]),
       pos = ifelse(vals > 0, 4, 2), cex = 0.8, xpd = NA)
  abline(v = 0)
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
b64_encode <- function(raw) {
  chars <- c(LETTERS, letters, 0:9, "+", "/")
  pad   <- (3 - length(raw) %% 3) %% 3
  m     <- matrix(c(as.integer(raw), rep(0L, pad)), nrow = 3)
  v     <- m[1, ] * 65536 + m[2, ] * 256 + m[3, ]
  idx   <- cbind(v %/% 262144, (v %/% 4096) %% 64, (v %/% 64) %% 64, v %% 64) + 1
  out   <- chars[t(idx)]
  if (pad > 0) out[(length(out) - pad + 1):length(out)] <- "="
  paste(out, collapse = "")
}

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
         "Bar chart of each assumption's contribution to the variation in Ct."),
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
    "<h1>Probabilistic vectorial capacity simulator: report</h1>",
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
    x  <- draw(3000, s)
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
           mort_a = 0.0001, mort_b = 0.001, mort_s = 0.01)

num <- function(id, f, label, s)
  numericInput(paste0(id, "_", f), label, s[[f]], width = "100%",
               step = if (f %in% c("shape1", "shape2")) 0.1 else steps[[id]])

# Open on the literature-based distributions rather than fixed point values
with_default_dist <- function(id, s) { s$dist <- lit_dists[[id]]; s }

assumption_ui <- function(id, s) {
  s <- with_default_dist(id, s)
  wellPanel(
    div(class = "assump-head",
      strong(labels[[id]]),
      div(class = "edited-tools", span(class = "edited-badge", "edited"),
          actionLink(paste0(id, "_reset"), "reset"))),
    div(class = "assump-desc", descs[[id]]),
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
    plotOutput(paste0(id, "_prev"), height = "52px"),
    div(class = "assump-warn", id = paste0(id, "_warn")),
    div(class = "assump-err", id = paste0(id, "_err"))
  )
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("Probabilistic Vectorial Capacity Simulator"),
  tags$button(id = "expand_sidebar", type = "button", class = "sidebar-arrow-open",
              title = "Show settings", `aria-label` = "Show settings", `aria-expanded` = "false",
              icon("chevron-right")),
  tags$head(tags$style(HTML("
    @media (max-width: 767px) { html { overflow-y: scroll; } }
    .container-fluid { padding-top: 14px; }
    .container-fluid > h2 { margin: 8px 0 28px; }
    body { padding-bottom: 36px; }
    .app-footer { position: fixed; left: 0; bottom: 0; z-index: 1000; padding: 4px 14px;
      font-size: 12px; color: #5a6268; background: rgba(255,255,255,0.9);
      border-top-right-radius: 6px; }
    .well { padding: 12px 14px; }
    .well a:not(.btn) { color: #286090; }
    .well .form-group { margin-bottom: 8px; }
    .well label.control-label { margin-bottom: 2px; font-size: 13px; }
    .well .form-control { height: 30px; padding: 3px 8px; font-size: 13px; }
    .well .selectize-input { min-height: 30px; padding: 4px 8px; }
    #n_iter + .selectize-control.single .selectize-input { padding-right: 22px; font-size: 12px; white-space: nowrap; }
    .well .row > [class*='col-'] { padding-left: 5px; padding-right: 5px; }
    .well .row { margin-left: -5px; margin-right: -5px; }
    #shiny-notification-panel { position: static; width: 100%; margin-top: 10px; }
    #shiny-notification-panel .shiny-notification { position: relative; width: 100%;
      margin: 0 0 8px; right: auto; z-index: 1100; }
    #tabs > li > a[data-value='Forecast']::before,
    #tabs > li > a[data-value='Sensitivity']::before,
    #tabs > li > a[data-value='Assumption draws']::before,
    #tabs > li > a[data-value='Survival curves']::before {
      content: ''; display: inline-block; box-sizing: border-box; width: 7px; height: 7px;
      margin: 0 8.5px 0 1.5px; border-radius: 50%; border: 1.5px solid #6c757d; background: transparent;
      vertical-align: middle; position: relative; top: -1px;
      transition: background-color .3s, border-color .3s, box-shadow .3s; }
    #tabs > li > a.tab-new::before { background: #6f42c1; border-color: #6f42c1;
      box-shadow: 0 0 6px 2px rgba(111,66,193,0.5); animation: dot-pulse 1.4s ease-out 2; }
    @keyframes dot-pulse { 0% { box-shadow: 0 0 0 0 rgba(111,66,193,0.6); }
      100% { box-shadow: 0 0 0 9px rgba(111,66,193,0); } }
    .assump-desc { font-size: 11px; line-height: 1.3; color: #5a6268; margin: 2px 0 8px; }
    .tab-pane.active { animation: fade-in .25s ease-out; }
    @keyframes fade-in { from { opacity: 0; transform: translateY(4px); } to { opacity: 1; transform: none; } }
    .recalculating { opacity: 0.55 !important; transition: opacity .2s; }
    .stale-note { animation: slide-in .25s ease-out; }
    @keyframes slide-in { from { opacity: 0; transform: translateY(-4px); } to { opacity: 1; transform: none; } }
    #preset .radio label span, .btn { transition: background-color .15s, border-color .15s, color .15s; }
    #run.running { opacity: 0.75; cursor: progress; }
    @media (prefers-reduced-motion: reduce) {
      *, *::before { animation: none !important; transition: none !important; } }
    .stale-note { font-size: 12px; color: #8a5a00; background: #fff4d6; border: 1px solid #f0d58a;
      border-radius: 4px; padding: 5px 8px; margin-top: 8px; }
    .preset-reset { font-size: 12px; margin: 0 0 8px; }
    .assump-invalid { border-color: #dc3545 !important; box-shadow: 0 0 0 1px #dc3545; }
    .assump-err { color: #b02a37; font-size: 12px; margin-top: 4px; }
    .assump-err:empty { display: none; }
    .check-result { font-size: 13px; color: #555; margin-top: 8px; }
    input:-moz-ui-invalid { box-shadow: none; }
    input:disabled, .form-control:disabled { background: #eceeef; color: #8a9096; cursor: not-allowed; }
    #stable_note { display: none; margin: -2px 0 8px; }
    /* Fixed frame on larger screens: title, settings and tab bar stay put; only tab content scrolls */
    @media (min-width: 768px) {
      html, body { height: 100%; overflow: hidden; }
      body { padding-bottom: 0; }
      .container-fluid { height: 100vh; display: flex; flex-direction: column; padding-bottom: 34px; }
      .container-fluid > h2 { flex: none; margin: 6px 0 18px; }
      .container-fluid > .row { flex: 1 1 auto; min-height: 0; display: flex; }
      .container-fluid > .row::before, .container-fluid > .row::after { content: none; }
      .container-fluid > .row > .col-sm-3 { overflow-y: auto; max-height: 100%; padding-bottom: 8px; }
      .container-fluid > .row > .col-sm-9 { display: flex; flex-direction: column; min-height: 0; }
      .container-fluid > .row > .col-sm-9 > .tabbable { display: flex; flex-direction: column; flex: 1 1 auto; min-height: 0; }
      .tabbable > .nav-tabs { flex: none; }
      .tabbable > .tab-content { flex: 1 1 auto; min-height: 0; overflow-y: scroll; overflow-x: hidden;
        padding: 0 6px 0 0; }
      /* Spacer at rest; once you scroll it sticks under the tabs and blurs what slides beneath */
      .tabbable > .tab-content::before { content: ''; display: block; position: sticky; top: 0; z-index: 20;
        height: 22px; pointer-events: none;
        background: linear-gradient(to bottom, rgba(255,255,255,0.92), rgba(255,255,255,0));
        -webkit-backdrop-filter: blur(7px); backdrop-filter: blur(7px);
        -webkit-mask-image: linear-gradient(to bottom, #000 55%, transparent);
        mask-image: linear-gradient(to bottom, #000 55%, transparent); }
      /* Slide-out settings panel: the column keeps its width and slides off to the left */
      .container-fluid > .row > .col-sm-3 { transition: margin-left .35s ease, opacity .3s ease, visibility 0s linear 0s; }
      .container-fluid > .row > .col-sm-9 { transition: width .35s ease, margin-left .35s ease; }
      .sidebar-collapsed > .row > .col-sm-3 { margin-left: -25%; opacity: 0; visibility: hidden;
        pointer-events: none; transition: margin-left .35s ease, opacity .25s ease, visibility 0s linear .35s; }
      .sidebar-collapsed > .row > .col-sm-9 { width: calc(100% - 30px); margin-left: 30px; }
      .sidebar-collapsed .sidebar-arrow-open { opacity: 1; visibility: visible; transition: opacity .3s ease .25s, visibility 0s; }
    }
    .howto { position: relative; background: #eef5fb; border: 1px solid #cfe0f0; border-radius: 6px;
      padding: 11px 14px 12px; font-size: 13px; margin-bottom: 14px; }
    .howto-title { font-weight: 700; font-size: 14px; margin-bottom: 8px; }
    .howto-steps { display: grid; grid-template-columns: repeat(auto-fit, minmax(210px, 1fr)); gap: 10px; }
    .howto-step { display: flex; gap: 10px; align-items: flex-start; background: #fff;
      border: 1px solid #d6e4f0; border-radius: 6px; padding: 9px 11px; }
    .howto-num { flex: none; width: 24px; height: 24px; border-radius: 50%; background: #286090; color: #fff;
      font-weight: 700; font-size: 13px; line-height: 24px; text-align: center; }
    .howto-step-title { font-weight: 700; font-size: 13px; margin-bottom: 1px; }
    .howto-step-text { font-size: 12.5px; line-height: 1.4; color: #333; }
    .howto-step-text kbd { font-size: 11px; padding: 0 5px; color: #333; background: #f3f5f7;
      border: 1px solid #ccd3da; border-radius: 3px; box-shadow: none; white-space: nowrap; }
    .howto-note { margin-top: 9px; font-size: 12px; color: #444; display: flex; align-items: center; gap: 7px; }
    .legend-dot { flex: none; width: 7px; height: 7px; border-radius: 50%; background: #6f42c1;
      box-shadow: 0 0 5px 1px rgba(111,66,193,0.5); }
    .howto-close { position: absolute; top: 3px; right: 8px; border: 0; background: none;
      font-size: 20px; line-height: 1; color: #5a6268; cursor: pointer; }
    .tab-content { padding-top: 16px; }
    @media (min-width: 768px) { .tab-content { padding-top: 0; } }
    #rand_seed { font-size: 14px; margin-left: 5px; text-decoration: none; }
    .settings-io { display: flex; flex-wrap: wrap; gap: 8px; align-items: flex-start; justify-content: flex-start; margin: 0 0 10px; }
    .settings-io .form-group { margin: 0; height: 30px; width: auto; }
    .settings-io .input-group { display: block; }
    .settings-io .input-group .form-control { display: none; }
    .settings-io .input-group-btn { display: block; width: auto; }
    .settings-io .btn-file, .settings-io .btn { border-radius: 3px; font-size: 12px; padding: 0 12px; height: 30px;
      line-height: 28px; margin: 0; display: inline-block; box-sizing: border-box; }
    .settings-io .progress { display: none; }
    .dist-help { font-size: 11px; line-height: 1.3; color: #5a6268; margin: -2px 0 8px; }
    .assump-warn { color: #8a5a00; font-size: 12px; margin-top: 4px; }
    .assump-warn:empty { display: none; }
    a:focus-visible, button:focus-visible, .btn:focus-visible, input:focus-visible,
    select:focus-visible, .selectize-input.focus, .nav-tabs > li > a:focus-visible {
      outline: 2px solid #1a73e8; outline-offset: 2px; }
    .btn-file:focus-within { outline: 2px solid #1a73e8; outline-offset: 2px; }
    .assump-prev { margin-top: 4px; }
    .container-fluid { position: relative; }
    .settings-head { display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px; }
    .well { position: relative; }
    .sidebar-arrow, .sidebar-arrow-open { border: 1px solid #ccc; background: #fff; color: #444; border-radius: 50%;
      width: 24px; height: 24px; padding: 0; font-size: 11px; line-height: 22px; text-align: center; cursor: pointer;
      transition: background-color .15s, color .15s, border-color .15s; }
    .sidebar-arrow:hover, .sidebar-arrow-open:hover { background: #337ab7; border-color: #2e6da4; color: #fff; }
    .sidebar-arrow-open { position: absolute; left: 15px; top: 76px; z-index: 30; opacity: 0; visibility: hidden;
      transition: opacity .15s ease, visibility 0s linear .15s; }
    @media (max-width: 767px) { .sidebar-arrow, .sidebar-arrow-open { display: none; } }
    .assump-head { display: flex; justify-content: space-between; align-items: flex-start; gap: 6px; }
    .edited-tools { display: none; flex-direction: column; align-items: flex-end; font-size: 11px; line-height: 1.3; }
    .edited-badge { background: #fff4d6; color: #8a5a00; border: 1px solid #f0d58a; border-radius: 9px; padding: 0 7px; }
    .assump-edited .edited-tools { display: flex; }
    .assump-edited { border-left: 3px solid #e0a800; }
    .fade-btn { position: relative; }
    .fade-btn .fb-a { display: inline-block; transition: opacity .25s ease .2s; }
    .fade-btn .fb-b { position: absolute; top: 0; right: 0; bottom: 0; left: 0; display: flex;
      align-items: center; justify-content: center; white-space: nowrap; opacity: 0;
      transition: opacity .2s ease; pointer-events: none; }
    /* Going in: old text fades out, then the confirmation fades in. Coming back: the reverse. */
    .fade-btn .fb-icon { margin-right: 6px; }
    #copy_link { min-width: 150px; }
    .fade-btn.copied .fb-a { opacity: 0; transition: opacity .2s ease; }
    .fade-btn.copied .fb-b { opacity: 1; transition: opacity .25s ease .2s; }
    .compare-table table { font-size: 13px; margin: 4px 0 4px; }
    .compare-table td, .compare-table th { padding: 3px 10px; border: 1px solid #ddd; }
    .compare-table th { background: #f3f5f7; }
    .table-tools { margin: 6px 0 4px; }
    .print-only { display: none; }
    @media print {
      html, body { height: auto !important; overflow: visible !important; }
      .container-fluid { height: auto !important; display: block !important; padding: 0 !important; }
      .container-fluid > h2 { margin: 0 0 8px !important; }
      .container-fluid > .row { display: block !important; }
      .container-fluid > .row > .col-sm-3, .nav-tabs, .app-footer, .sidebar-arrow, .sidebar-arrow-open, .dl-row, .howto,
      .settings-io, .no-print, .edited-tools, .table-tools, .tab-content::before { display: none !important; }
      .container-fluid > .row > .col-sm-9 { width: 100% !important; float: none !important; display: block !important; }
      .tabbable { display: block !important; }
      .tab-content { overflow: visible !important; height: auto !important; padding: 0 !important; }
      .well { break-inside: avoid; }
      img { max-width: 100% !important; }
      .print-only { display: block; font-size: 12px; color: #444; margin: 0 0 10px; }
    }
    .dl-row { margin: 4px 0 10px; }
    .dl-row .btn { margin-right: 6px; }
    .forecast-summary { font-size: 15px; margin: 6px 0 4px; }
    .forecast-note { font-size: 12px; color: #5a6268; margin: 0 0 8px; }
    .cert-text { margin-top: 25px; }
    .app-meta { font-size: 12px; color: #5a6268; }
    .run-status { font-size: 12px; color: #555; margin-top: 6px; }
    #preset > label.control-label { display: block; margin-bottom: 4px; }
    #preset .shiny-options-group { margin-top: 0; }
    #preset .radio { margin: 0 0 4px; }
    #preset .radio label { display: block; padding: 0; width: 100%; }
    #preset input[type=radio] { position: absolute; opacity: 0; pointer-events: none; }
    #preset .radio label span { display: block; padding: 5px 10px; font-size: 13px; border: 1px solid #ccc;
      border-radius: 4px; background: #fff; color: #333; cursor: pointer; }
    #preset .radio label:hover span { background: #f0f0f0; }
    #preset .radio label:hover input:checked + span { background: #2e6da4; }
    #preset input:checked + span { background: #337ab7; border-color: #2e6da4; color: #fff; }
    #preset input:checked + span::before { content: '\\2713  '; font-weight: bold; }
    #preset input:focus-visible + span { outline: 2px solid #66afe9; outline-offset: 1px; }
    .preset-desc { font-size: 11px; line-height: 1.3; color: #555; margin: 0 0 8px; }
    .author-card { display: flex; align-items: center; gap: 24px; max-width: 640px;
      padding: 20px 24px; background: #f8f9fa; border: 1px solid #e3e6ea;
      border-radius: 12px; box-shadow: 0 2px 6px rgba(0,0,0,0.06); }
    .author-photo { width: 130px; height: 130px; flex: none; border-radius: 50%;
      object-fit: cover; border: 3px solid #fff; box-shadow: 0 1px 4px rgba(0,0,0,0.25); }
    .author-label { font-size: 12px; letter-spacing: 0.08em; text-transform: uppercase;
      color: #5a6268; margin-bottom: 2px; }
    .author-text h4 { margin: 0 0 6px; }
    .author-text p { margin: 0 0 6px; }
    .author-link { font-size: 14px; margin-bottom: 0 !important; }
    .author-note { color: #555; font-size: 14px; }
    @media (max-width: 767px) { .well .row > .col-sm-6 { width: 50%; float: left; }
      .well .row > .col-sm-7 { width: 58.33%; float: left; } .well .row > .col-sm-5 { width: 41.67%; float: left; } }
    @media (max-width: 520px) { .author-card { flex-direction: column; text-align: center; } }
  "))),
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
        column(6, numericInput("r", "Growth rate r", round(r_hat, 4), step = 0.01)),
        column(6, numericInput("sigma", "First bite (d)", 3, min = 0, max = 20))),
      div(id = "stable_note", class = "assump-desc",
          "Growth rate and first-bite age apply only to the stable age distribution."),
      fluidRow(
        column(7, selectInput("n_iter", "Trials",
                              c("500" = 500, "1,000" = 1000, "5,000" = 5000, "10,000 (slow)" = 10000), 1000)),
        column(5, numericInput("seed", HTML(paste0("Seed ", as.character(actionLink("rand_seed", icon("shuffle"), title = "Pick a random seed")))), 1))),
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
      div(class = "preset-reset", actionLink("start_over", "Start over (defaults, clear run history)")),
      actionButton("run", "Run simulation", class = "btn-primary", width = "100%",
                   title = "Shortcut: Cmd or Ctrl + Enter"),
      uiOutput("stale_note"),
      div(class = "run-status", textOutput("run_status"))
    ),
    mainPanel(width = 9,
      tabsetPanel(id = "tabs",
        tabPanel("Define assumptions",
          div(class = "howto", id = "howto",
            div(class = "howto-title", "Getting started"),
            div(class = "howto-steps",
              howto_step(1, "Choose assumptions", "Pick a preset in Simulation settings, or edit the cards below."),
              howto_step(2, "Set up the run", "Choose the mortality model, number of trials and a seed."),
              howto_step(3, "Run and read", "Click ", strong("Run simulation"), " (or press ",
                         tags$kbd("Cmd/Ctrl + Enter"), "), then open the Forecast tab.")),
            div(class = "howto-note", span(class = "legend-dot"),
                "A purple dot on a tab means it has new results you have not looked at yet."),
            tags$button(type = "button", class = "howto-close", `aria-label` = "Dismiss", HTML("&times;"))),
          div(class = "settings-io",
            downloadButton("dl_settings", "Save settings", class = "btn-sm"),
            tags$button(id = "copy_link", type = "button", class = "btn btn-default btn-sm fade-btn",
                        title = "Copies a link that restores these settings",
                        span(class = "fb-a", icon("share-nodes"), " Share settings"),
                        span(class = "fb-b", `aria-live` = "polite",
                             span(class = "fb-icon", icon("link")), span(class = "fb-msg"))),
            fileInput("load_settings", NULL, buttonLabel = "Load settings", placeholder = "",
                      accept = ".csv", width = "auto")),
          h4("Transmission"),
          fluidRow(lapply(names(vc_specs), function(id) column(3, assumption_ui(id, vc_specs[[id]])))),
          h4("Mortality schedule"),
          fluidRow(
            column(3, assumption_ui("mort_a", mort_specs$logistic$mort_a)),
            column(3, conditionalPanel("input.mort_model != 'exponential'",
                                       assumption_ui("mort_b", mort_specs$logistic$mort_b))),
            column(3, conditionalPanel("input.mort_model == 'logistic'",
                                       assumption_ui("mort_s", mort_specs$logistic$mort_s))))),
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
          helpText("Each bar is an assumption's share of the variation in Ct, based on the squared",
                   "rank correlation between that input and Ct. Blue bars raise Ct and orange bars lower",
                   "it. The number beside each bar is the rank correlation (rho). Assumptions that are",
                   "fixed do not vary and are left out. Use the switch below to show the raw rank",
                   "correlation (from -1 to 1) instead of the share of variance."),
          radioButtons("sens_metric", NULL, inline = TRUE,
                       c("Contribution to variance (%)" = "contrib", "Rank correlation (rho)" = "rho")),
          div(class = "dl-row", dl_png("sens_plot", "sensitivity.png")),
          plotOutput("sens_plot", height = 400)),
        tabPanel("Assumption draws",
          helpText("Histograms of the values drawn for each assumption across all trials. An assumption",
                   "that is fixed shows as a single bar."),
          div(class = "dl-row", dl_png("draws_plot", "assumption_draws.png")),
          plotOutput("draws_plot", height = 600)),
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
            sprintf("Largest difference from a published value: %.1f%%. Published values are rounded to one decimal place, so small differences are expected.", max_dev))),
        tabPanel("About",
          h4("What this tool does"),
          p("This app propagates uncertainty in transmission and mosquito mortality parameters",
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
                "Developed this tool to make probabilistic vectorial capacity forecasts."),
              p(class = "author-link",
                tags$a(href = "https://www.jackson-strand.com", target = "_blank",
                       rel = "noopener", "www.jackson-strand.com")))))
      )
    )
  ),
  div(class = "app-footer", paste("Last updated:", LAST_UPDATED)),
  # Shiny deletes and recreates its notification panel for each pop-up or progress bar,
  # so watch for it and move it under the settings box every time
  tags$script(HTML("
    $(function() {
      var well = $('.well').first();
      function rehome() {
        var p = document.getElementById('shiny-notification-panel');
        if (p && p.previousElementSibling !== well[0]) well.after(p);
      }
      new MutationObserver(rehome).observe(document.body, {childList: true});
      rehome();

      // Show a busy state on the Run button, delayed so quick updates do not flicker
      var busyTimer = null;
      $(document).on('shiny:busy', function() {
        busyTimer = setTimeout(function() { $('#run').addClass('running').prop('disabled', true).text('Running...'); }, 250);
      });
      $(document).on('shiny:idle', function() {
        clearTimeout(busyTimer);
        $('#run').removeClass('running').prop('disabled', false).text('Run simulation');
      });

      // Save any plot exactly as shown (buttons carry the plot id and file name)
      $(document).on('click', '.dl-img', function() {
        var img = $('#' + $(this).data('target') + ' img')[0];
        var name = $(this).data('file') || 'plot.png';
        if (!img) return;
        fetch(img.src).then(function(r) { return r.blob(); }).then(function(blob) {
          var a = document.createElement('a');
          a.href = URL.createObjectURL(blob);
          a.download = name;
          document.body.appendChild(a); a.click(); a.remove();
          setTimeout(function() { URL.revokeObjectURL(a.href); }, 1000);
        });
      });

      $(document).on('click', '.howto-close', function() { $('#howto').slideUp(150); });
      // Allow loading the same settings file twice in a row
      $(document).on('click', '#load_settings', function() { this.value = ''; });

      // Grey out growth rate and first-bite age when they do not apply
      function syncStructure() {
        var synchronous = $('#structure').val() === 'synchronous';
        $('#r, #sigma').prop('disabled', synchronous);
        $('#stable_note').toggle(synchronous);
      }
      $('#structure').on('change', syncStructure);
      setTimeout(syncStructure, 300);

      // Red outline and message on an assumption box with invalid values
      Shiny.addCustomMessageHandler('assumpErr', function(errs) {
        Object.keys(errs).forEach(function(id) {
          var msg = errs[id] || '';
          $('#' + id + '_err').text(msg);
          $('#' + id + '_dist').closest('.well').toggleClass('assump-invalid', msg !== '');
        });
      });

      Shiny.addCustomMessageHandler('assumpWarn', function(w) {
        Object.keys(w).forEach(function(id) { $('#' + id + '_warn').text(w[id] || ''); });
      });

      // Cmd or Ctrl + Enter runs the simulation
      $(document).on('keydown', function(e) {
        if ((e.metaKey || e.ctrlKey) && e.key === 'Enter') {
          e.preventDefault();
          if (document.activeElement) document.activeElement.blur();
          setTimeout(function() { if (!$('#run').prop('disabled')) $('#run').click(); }, 60);
        }
      });

      // Copy text to the clipboard, with a fallback for pages where the clipboard API is blocked
      function copyText(text) {
        return new Promise(function(resolve) {
          function fallback() {
            var ta = document.createElement('textarea');
            ta.value = text; ta.style.position = 'fixed'; ta.style.opacity = 0;
            document.body.appendChild(ta); ta.select();
            var ok = false;
            try { ok = document.execCommand('copy'); } catch (e) {}
            ta.remove(); resolve(ok);
          }
          if (navigator.clipboard && window.isSecureContext) {
            navigator.clipboard.writeText(text).then(function() { resolve(true); }, fallback);
          } else { fallback(); }
        });
      }
      function flash(btn, text, success) {
        var b = btn.find('.fb-b'), msg = b.find('.fb-msg');
        (msg.length ? msg : b).text(text);
        b.find('.fb-icon').toggle(!!success);
        btn.addClass('copied');
        clearTimeout(btn.data('flashTimer'));
        btn.data('flashTimer', setTimeout(function() { btn.removeClass('copied'); }, 2450));  // 0.45s fade-in + 2s on screen
      }

      // Copy a table as tab-separated text, which pastes into Excel or Word as a table
      $(document).on('click', '.copy-table', function() {
        var btn = $(this);
        var rows = $('#' + btn.data('target') + ' table tr').map(function() {
          return $(this).find('th, td').map(function() { return $(this).text().trim(); }).get().join('\\t');
        }).get();
        if (!rows.length) { flash(btn, 'Nothing to copy yet'); return; }
        window.__lastCopied = rows.join('\\n');
        copyText(window.__lastCopied).then(function(ok) { flash(btn, ok ? 'Copied!' : 'Copy failed', ok); });
      });

      // Shareable link: every setting goes into the address after the # sign
      var settingIds = ['a_bite', 'n_eip', 'm_dens', 'vec_comp', 'mort_a', 'mort_b', 'mort_s'];
      var settingFields = ['value', 'min', 'mode', 'max', 'mean', 'sd', 'shape1', 'shape2'];
      function topWin() { try { void window.top.location.href; return window.top; } catch (e) { return window; } }
      function settingsParams() {
        var p = new URLSearchParams();
        p.set('model', $('#mort_model').val()); p.set('structure', $('#structure').val());
        p.set('r', $('#r').val()); p.set('sigma', $('#sigma').val());
        p.set('trials', $('#n_iter').val()); p.set('seed', $('#seed').val());
        settingIds.forEach(function(id) {
          p.set(id + '.dist', $('#' + id + '_dist').val());
          settingFields.forEach(function(f) {
            var v = $('#' + id + '_' + f).val();
            if (v !== undefined && v !== null && v !== '') p.set(id + '.' + f, v);
          });
        });
        return p;
      }
      $(document).on('click', '#copy_link', function() {
        var btn = $(this), w = topWin();
        var url = w.location.href.split('#')[0] + '#' + settingsParams().toString();
        window.__lastLink = url;
        try { w.history.replaceState(null, '', url); } catch (e) {}
        copyText(url).then(function(ok) { flash(btn, ok ? 'Copied!' : 'Link in address bar', ok); });
      });
      function sendHashSettings() {
        var h = topWin().location.hash.replace(/^#/, '');
        if (!h) return;
        var p = new URLSearchParams(h);
        if (!p.has('model')) return;
        var o = {}; p.forEach(function(v, k) { o[k] = v; });
        Shiny.setInputValue('url_settings', o, {priority: 'event'});
      }
      if (Shiny.shinyapp && Shiny.shinyapp.isConnected()) { sendHashSettings(); }
      else { $(document).one('shiny:connected', sendHashSettings); }

      // Hide or show the settings panel so the results can use the full width
      function setSidebar(collapsed) {
        $('.container-fluid').first().toggleClass('sidebar-collapsed', collapsed);
        $('#collapse_sidebar').attr('aria-expanded', String(!collapsed));
        $('#expand_sidebar').attr('aria-expanded', String(!collapsed));
        // Let the slide finish, then redraw plots at the new width and move keyboard focus to the arrow that is now visible
        setTimeout(function() {
          $(window).trigger('resize');
          $(collapsed ? '#expand_sidebar' : '#collapse_sidebar').trigger('focus');
        }, 380);
      }
      $(document).on('click', '#collapse_sidebar', function() { setSidebar(true); });
      $(document).on('click', '#expand_sidebar', function() { setSidebar(false); });

      // Start over: drop any settings from the address, then reload the app
      Shiny.addCustomMessageHandler('startOver', function(msg) {
        try { var w = topWin(); w.history.replaceState(null, '', w.location.pathname + w.location.search); } catch (e) {}
        window.location.reload();
      });

      // Badge and reset link on assumption cards that differ from the chosen preset
      Shiny.addCustomMessageHandler('editedCards', function(ed) {
        Object.keys(ed).forEach(function(id) {
          $('#' + id + '_dist').closest('.well').toggleClass('assump-edited', !!ed[id]);
        });
      });

      // Purple dot on result tabs when a run has produced new results you have not viewed yet
      var resultTabs = ['Forecast', 'Sensitivity', 'Assumption draws', 'Survival curves'];
      Shiny.addCustomMessageHandler('newResults', function(msg) {
        // On phones the results sit below the settings, so bring them into view
        if (window.innerWidth < 768 && !window.__firstRunDone) { window.__firstRunDone = true; }
        else if (window.innerWidth < 768) { setTimeout(function() { $('#tabs')[0].scrollIntoView(); }, 400); }
        resultTabs.forEach(function(v) {
          var a = $('#tabs a[data-value=\"' + v + '\"]');
          if (!a.parent().hasClass('active')) a.addClass('tab-new').attr({title: 'New results', 'aria-label': v + ', new results'});
        });
      });
      $(document).on('shown.bs.tab', '#tabs a', function() {
        $(this).removeClass('tab-new').removeAttr('title').removeAttr('aria-label');
      });
    });
  "))
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

  active_ids <- function(model)
    c(names(vc_specs), "mort_a",
      if (model != "exponential") "mort_b",
      if (model == "logistic") "mort_s")

  setting_ids <- c(names(vc_specs), names(mort_specs$logistic))
  loaded <- reactiveVal(NULL)

  apply_cfg <- function(v) {
    get1 <- function(k) if (k %in% names(v)) v[[k]] else NA_character_
    num  <- function(k) suppressWarnings(as.numeric(get1(k)))
    if (get1("structure") %in% c("stable", "synchronous"))
      updateSelectInput(session, "structure", selected = get1("structure"))
    if (!is.na(num("r")))     updateNumericInput(session, "r", value = num("r"))
    if (!is.na(num("sigma"))) updateNumericInput(session, "sigma", value = num("sigma"))
    if (!is.na(num("seed")))  updateNumericInput(session, "seed", value = num("seed"))
    if (get1("trials") %in% c("500", "1000", "5000", "10000"))
      updateSelectInput(session, "n_iter", selected = get1("trials"))
    for (id in setting_ids) {
      d <- get1(paste0(id, ".dist"))
      if (is.na(d) || !d %in% dist_choices) next
      sp <- list(dist = d)
      for (f in fields) sp[[f]] <- num(paste0(id, ".", f))
      set_spec(id, sp)
    }
  }

  output$dl_settings <- downloadHandler(
    filename = function() "vectorial_capacity_settings.csv",
    content  = function(file) {
      rows <- list(model = input$mort_model, structure = input$structure, r = input$r,
                   sigma = input$sigma, trials = input$n_iter, seed = input$seed)
      for (id in setting_ids) {
        sp <- get_spec(id)
        rows[[paste0(id, ".dist")]] <- sp$dist
        for (f in fields) rows[[paste0(id, ".", f)]] <- sp[[f]]
      }
      writeLines("# Settings saved from the Probabilistic Vectorial Capacity Simulator. Use Load settings to restore them.", file)
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
    all <- c(vc_specs, mort_specs[[input$mort_model]])
    for (id in names(all)) {
      s <- all[[id]]
      s$dist <- if (preset == "lit") lit_dists[[id]] else "Fixed"
      set_spec(id, s)
    }
  }
  observeEvent(input$preset, apply_preset(input$preset), ignoreInit = TRUE)
  observeEvent(input$reset_preset, apply_preset(input$preset))

  # The preset's own spec for one assumption (what "unedited" means)
  preset_spec <- function(id) {
    sp <- c(vc_specs, mort_specs[[input$mort_model]])[[id]]
    sp$dist <- if (identical(input$preset, "lit")) lit_dists[[id]] else "Fixed"
    sp
  }

  # Mark assumption cards whose values differ from the preset, and let each be reset on its own
  last_edited <- reactiveVal(NULL)
  observe({
    ed <- vapply(setting_ids, function(id) !same_spec(get_spec(id), preset_spec(id)), logical(1))
    if (!identical(ed, isolate(last_edited()))) {
      last_edited(ed)
      session$sendCustomMessage("editedCards", as.list(ed))
    }
  })
  lapply(setting_ids, function(id)
    observeEvent(input[[paste0(id, "_reset")]], set_spec(id, preset_spec(id)), ignoreInit = TRUE))

  # Snapshot of everything that feeds a run, to tell when results are out of date
  cur_sig <- reactive({
    ids <- active_ids(input$mort_model)
    list(model = input$mort_model, structure = input$structure, r = input$r,
         sigma = input$sigma, n = input$n_iter, seed = input$seed,
         specs = lapply(setNames(ids, ids), get_spec))
  })
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
    set.seed(input$seed)
    d <- as.data.frame(lapply(specs, function(s) draw(n, s)))
    d$n_eip <- pmin(pmax(round(d$n_eip), 1), 150)
    b_i <- if ("mort_b" %in% ids) d$mort_b else rep(0, n)
    s_i <- if ("mort_s" %in% ids) d$mort_s else rep(0, n)

    ct <- numeric(n)
    withProgress(message = "Running trials", value = 0, {
      for (i in seq_len(n)) {
        lt <- life_table(hazard(model, AGES, d$mort_a[i], b_i[i], s_i[i]))
        Cx <- age_specific_vc(lt, d$n_eip[i], d$m_dens[i] * d$a_bite[i]^2, d$vec_comp[i])
        ct[i] <- if (input$structure == "stable") ct_stable(lt, Cx, input$r, input$sigma)
                 else ct_synchronous(Cx)
        if (i %% 100 == 0) incProgress(100 / n, detail = sprintf("%d of %d", i, n))
      }
    })

    settings <- c(
      sprintf("Probabilistic vectorial capacity simulator, version %s", APP_VERSION),
      sprintf("Run time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      sprintf("Mortality model: %s", model),
      sprintf("Population age structure: %s", input$structure),
      sprintf("Growth rate r: %s", input$r),
      sprintf("Age at first bite (days): %s", input$sigma),
      sprintf("Trials: %d", n),
      sprintf("Random seed: %s", input$seed),
      unlist(Map(function(s, l) sprintf("%s: %s, %s", l, s$dist, describe_spec(s)), specs, labels[ids])))

    new_run <- run_count() + 1
    entry <- list(run = new_run, ct = ct,
                  label = sprintf("Run %d: %s, %s, %s trials, median Ct %s", new_run, model,
                                  input$structure, format(n, big.mark = ","), signif(median(ct), 3)))
    history(c(tail(history(), 9), list(entry)))
    results_val(list(ct = ct, draws = d, model = model, b = b_i, s = s_i,
                     settings = settings, run = new_run))
    run_sig(cur_sig())
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
    errs <- vapply(c(names(vc_specs), names(mort_specs$logistic)), function(id) {
      if (!id %in% active) return("")
      msg <- check_spec(get_spec(id), labels[[id]])
      if (is.null(msg)) "" else substring(msg, nchar(labels[[id]]) + 2)
    }, character(1))
    if (!identical(errs, isolate(last_errs()))) {
      last_errs(errs)
      session$sendCustomMessage("assumpErr", as.list(errs))
    }
    warns <- vapply(c(names(vc_specs), names(mort_specs$logistic)), function(id) {
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

  output$sens_plot <- renderPlot(draw_sens(results(), input$sens_metric), alt = reactive({
    sc <- sens_contrib(results())
    if (is.null(sc)) "No assumptions vary, so there is nothing to rank."
    else if (identical(input$sens_metric, "rho"))
      sprintf("Bar chart of the rank correlation between each assumption and Ct. Strongest: %s, rho %.2f.",
              labels[[names(sc$rho)[which.max(abs(sc$rho))]]], sc$rho[[which.max(abs(sc$rho))]])
    else sprintf("Bar chart of each assumption's contribution to the variation in Ct. Largest: %s, %.0f%%.",
                 labels[[names(sc$contrib)[which.max(abs(sc$contrib))]]], max(abs(sc$contrib)))
  }))

  output$draws_plot <- renderPlot(draw_draws(results()$draws), alt = reactive(
    sprintf("Histograms of the values drawn for each of %d assumptions across all trials.", ncol(results()$draws))))

  output$surv_plot <- renderPlot(draw_surv(results()), alt = "Survivorship and daily mortality hazard curves for the first 100 trials, one line per trial.")

  output$dl_report <- downloadHandler(
    filename = function() "vectorial_capacity_report.html",
    content  = function(file) {
      res <- results(); prev <- overlay_data()
      writeLines(build_report(res, cert_bounds(), prev,
                              c(sprintf("Run %d (current)", res$run), sprintf("Run %d", prev$run)), input$thresh), file)
    })

  output$validation <- renderTable(validation, digits = 2)
}

shinyApp(ui, server)
