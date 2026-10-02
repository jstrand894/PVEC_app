# Checks on the model and sampling code. Run from the repository root:  Rscript tests/test_core.R
# Stops with an error if any check fails.
suppressMessages(source("app/app.R", local = TRUE))      # defines the functions; does not start the app

results <- list()
check <- function(ok, what) { results[[length(results) + 1]] <<- isTRUE(ok); cat(sprintf("%-4s %s\n", if (isTRUE(ok)) "PASS" else "FAIL", what)) }

mk_specs <- function(model, structure, preset) {
  ids <- c(names(vc_specs), "mort_a", if (model != "exponential") "mort_b", if (model == "logistic") "mort_s")
  allspec <- c(vc_specs, mort_specs[[model]])
  s <- lapply(ids, function(id) { x <- allspec[[id]]; x$dist <- if (preset == "lit") lit_dists[[id]] else "Fixed"; x }); names(s) <- ids
  if (structure == "stable") s <- c(s, lapply(pop_specs, function(x) { x$dist <- "Fixed"; x }))
  s
}

# --- Model ---------------------------------------------------------------------------------------
# Constant mortality must reproduce the classical formula m a^2 exp(-mu n) / mu
mu <- 0.0313; n <- 10; MA2 <- 0.46 * 0.5^2
lt <- life_table(hazard("exponential", AGES, mu, 0, 0)); Cx <- age_specific_vc(lt, n, MA2, 1)
check(abs(Cx[n + 1] / (MA2 * exp(-mu * n) / mu) - 1) < 1e-3, "age-specific VC matches the classical closed form for constant mortality")
check(max_dev < 1, sprintf("Styer et al. check: largest difference from published values %.2f%% (limit 1%%)", max_dev))
check(all(diff(run_model("logistic", "stable", mk_specs("logistic", "stable", "fixed"), 50, 1)$ct) == 0), "all assumptions fixed gives one Ct for every trial")
a <- run_model("logistic", "stable", mk_specs("logistic", "stable", "lit"), 200, 7); b <- run_model("logistic", "stable", mk_specs("logistic", "stable", "lit"), 200, 7)
check(identical(a$ct, b$ct), "the same seed reproduces the same result")
check(!identical(a$ct, run_model("logistic", "stable", mk_specs("logistic", "stable", "lit"), 200, 8)$ct), "a different seed gives a different result")

# --- The block-at-a-time model gives the same Ct as the one-trial-at-a-time functions ---------------------
scalar_ct <- function(model, structure, d, b, s) vapply(seq_len(nrow(d)), function(i) {
  lt <- life_table(hazard(model, AGES, d$mort_a[i], b[i], s[i]))
  Cx <- age_specific_vc(lt, d$n_eip[i], d$m_dens[i] * d$a_bite[i]^2, d$vec_comp[i])
  if (structure == "stable") ct_stable(lt, Cx, d$growth_r[i], pmin(pmax(round(d$first_bite[i]), 0), N_CLASS - 1)) else ct_synchronous(Cx)
}, 0)
for (mod in c("exponential", "gompertz", "logistic")) for (st in c("synchronous", "stable")) {
  spc <- mk_specs(mod, st, "lit")
  if (st == "stable") for (id in names(pop_specs)) { spc[[id]]$dist <- "Uniform" }
  rr <- run_model(mod, st, spc, 300, 4, chunk = 70)                        # 70 does not divide 300: uneven last block
  d <- rr$draws; d$n_eip <- pmin(pmax(round(d$n_eip), 1), 150)
  check(max(abs(rr$ct / scalar_ct(mod, st, d, rr$b, rr$s) - 1)) < 1e-9, sprintf("block model equals the one-trial-at-a-time model (%s, %s)", mod, st))
}
wide <- mk_specs("logistic", "stable", "lit")                             # extreme ranges: hazards that overflow exp()
wide$mort_b$dist <- "Uniform"; wide$mort_b$min <- 0.5; wide$mort_b$max <- 3; wide$n_eip$min <- 1; wide$n_eip$max <- 150
rw <- run_model("logistic", "stable", wide, 100, 2); dw <- rw$draws; dw$n_eip <- pmin(pmax(round(dw$n_eip), 1), 150)
check(all(is.finite(rw$ct)) && max(abs(rw$ct / scalar_ct("logistic", "stable", dw, rw$b, rw$s) - 1)) < 1e-9, "block model handles hazards that overflow, like the one-trial model")
check(identical(run_model("gompertz", "stable", mk_specs("gompertz", "stable", "lit"), 700, 5, chunk = 50)$ct,
                run_model("gompertz", "stable", mk_specs("gompertz", "stable", "lit"), 700, 5, chunk = 700)$ct), "block size does not change the result")
steps <- c(); invisible(run_model("exponential", "synchronous", mk_specs("exponential", "synchronous", "lit"), 600, 1, progress = function(i, n) steps <<- c(steps, i)))
check(identical(as.numeric(steps), c(250, 500, 600)), "progress is reported after each block and ends at the number of trials")

# A run leaves the session's random-number stream as it was
set.seed(99); invisible(run_model("exponential", "synchronous", mk_specs("exponential", "synchronous", "lit"), 50, 1)); after <- runif(1)
set.seed(99); check(identical(after, runif(1)), "running a simulation does not disturb the session's random numbers")

# The report's image encoder agrees with standard base64
check(identical(b64_encode(charToRaw("Man")), "TWFu") && identical(b64_encode(charToRaw("Ma")), "TWE="), "base64 encoder matches the standard")

# A card's preview is the same every time it is drawn
set.seed(1); pa <- qdraw(ppoints(1000), sp("Beta", 0, 0.25, 0.4, 0.76, 0, 0, 2, 3)); set.seed(2); pb <- qdraw(ppoints(1000), sp("Beta", 0, 0.25, 0.4, 0.76, 0, 0, 2, 3))
check(identical(pa, pb), "distribution previews do not depend on random numbers")

# --- Sampling ------------------------------------------------------------------------------------
set.seed(3)
ok <- vapply(setdiff(dist_choices, "Fixed"), function(dn) { s <- sp("Fixed", 0.5, 0.2, 0.4, 0.9, 0.5, 0.1, 2, 3); s$dist <- dn
  if (dn == "Lognormal") { s$mean <- 0.5; s$sd <- 0.15 }; ks.test(qdraw(runif(20000), s), draw(20000, s))$statistic < 0.02 }, TRUE)
check(all(ok), "inverse-CDF sampler agrees with the independent samplers for every distribution")

sp2 <- mk_specs("gompertz", "synchronous", "lit"); N <- 20000
for (rho in c(-0.9, -0.3, 0.5, 0.95)) {
  g <- generate_draws(sp2, N, data.frame(a = "mort_a", b = "mort_b", rho = rho, stringsAsFactors = FALSE))
  check(abs(g$achieved$achieved - rho) < 0.02, sprintf("requested rank correlation %+.2f achieved as %+.3f", rho, g$achieved$achieved))
}
g <- generate_draws(sp2, N, data.frame(a = "mort_a", b = "mort_b", rho = -0.9, stringsAsFactors = FALSE)); set.seed(5); ind <- generate_draws(sp2, N)
check(all(vapply(c("mort_a", "mort_b"), function(id) suppressWarnings(ks.test(g$draws[[id]], ind$draws[[id]])$statistic) < 0.02, TRUE)), "linking assumptions leaves each one's own distribution unchanged")
g3 <- generate_draws(mk_specs("logistic", "synchronous", "lit"), N, data.frame(a = c("a_bite", "n_eip", "a_bite"), b = c("n_eip", "m_dens", "m_dens"), rho = c(0.9, 0.9, -0.9), stringsAsFactors = FALSE))
check(any(grepl("reduced by", g3$notes)), "an impossible set of correlations is detected and reduced")

set.seed(8); m <- 3000; up <- data.frame(mort_a = rlnorm(m, log(0.0066), 0.2)); up$mort_b <- 0.0623 * (up$mort_a / 0.0066)^-1.5 * exp(rnorm(m, 0, 0.02))
r4 <- run_model("gompertz", "synchronous", sp2, 1000, 3, uploaded = list(df = up))
key <- paste(signif(up$mort_a, 12), signif(up$mort_b, 12)); got <- paste(signif(r4$draws$mort_a, 12), signif(r4$draws$mort_b, 12))
check(all(got %in% key) && !anyDuplicated(got), "uploaded draws are used as whole rows, without replacement when the file is large enough")
check(abs(cor(r4$draws$mort_a, r4$draws$mort_b, method = "spearman") - cor(up$mort_a, up$mort_b, method = "spearman")) < 0.05, "uploaded draws keep their joint correlation")

# --- Sensitivity ---------------------------------------------------------------------------------
set.seed(21); N <- 4000; x1 <- rnorm(N); x2 <- 0.6 * x1 + 0.8 * rnorm(N); x3 <- rnorm(N); y <- 2 * x1 - x2 + 0.5 * x3 + 0.3 * rnorm(N)
X <- data.frame(x1, x2, x3); pc <- prcc_calc(X, y)
Rm <- cor(cbind(as.data.frame(lapply(X, rank)), y = rank(y))); P <- solve(Rm); ref <- -P[1:3, 4] / sqrt(diag(P)[1:3] * P[4, 4])
check(all(abs(pc$est - ref) < 1e-6), "PRCC equals the textbook inverse-correlation-matrix formula")
check(!is.null(tryCatch(prcc_calc(data.frame(x1 = x1, x1b = x1, x3 = x3), y), error = function(e) NULL)), "PRCC does not fail when two inputs are identical")

# --- Fitting a distribution to a reported interval -------------------------------------------------
sb <- list(min = 0, max = 1, mean = NA, sd = NA)
fb <- fit_range("Beta", 0.2, 0.7, 0.445, sb)$fields
q <- qbeta(c(0.025, 0.975), fb$shape1, fb$shape2)
check(abs(fb$shape1 / (fb$shape1 + fb$shape2) - 0.445) < 0.01 && all(abs(q - c(0.2, 0.7)) < 0.01), "Beta fit reproduces the reported mean and 95% interval")
fn <- fit_range("Normal", 10, 20, NA, sb)$fields
check(abs(fn$mean - 15) < 1e-9 && abs(qnorm(0.975, fn$mean, fn$sd) - 20) < 1e-9, "Normal fit reproduces the interval")
fl <- fit_range("Lognormal", 2, 8, NA, sb)$fields; sl <- sqrt(log(1 + fl$sd^2 / fl$mean^2)); ml <- log(fl$mean) - sl^2 / 2
check(all(abs(qlnorm(c(0.025, 0.975), ml, sl) - c(2, 8)) < 1e-6), "Lognormal fit reproduces the interval")
check(!fit_range("Beta", 0.2, 1.4, NA, sb)$ok && !fit_range("Normal", 5, 3, NA, sb)$ok && !fit_range("PERT", 1, 2, NA, sb)$ok, "fit refuses impossible or unsupported input")

# --- The equations written on the About tab, implemented literally, agree with the model ------------
lit_ct <- function(model, structure, a, m, cc, n, ma, mb, ms, r, sigma) {
  x <- 0:399
  mu <- switch(model, exponential = rep(ma, 400), gompertz = ma * exp(mb * x),
               logistic = ma * exp(mb * x) / (1 + (ma * ms / mb) * (exp(mb * x) - 1)))
  l <- function(k) exp(-vapply(k, function(kk) sum(mu[seq_len(kk)]), 0))           # sum of mu(0..k-1)
  e <- function(k) vapply(k, function(kk) sum(l(kk:400)) / l(kk), 0) - 0.5          # sum over ages k and older
  C <- sapply(0:199, function(xx) m * a^2 * cc * l(xx + n) / l(xx) * e(xx + n))
  C[!is.finite(C)] <- 0                                                              # no mosquitoes survive that far
  if (structure == "synchronous") return(mean(C[4:7]))
  w <- l(0:199) * exp(-r * (0:199)); w <- w / sum(w)
  sum(w[(sigma + 1):200] * C[(sigma + 1):200])
}
for (mod in c("exponential", "gompertz", "logistic")) for (st in c("synchronous", "stable")) {
  sp <- mk_specs(mod, st, "fixed"); g <- function(id) sp[[id]]$value
  app <- run_model(mod, st, sp, 2, 1)$ct[1]
  by_hand <- lit_ct(mod, st, g("a_bite"), g("m_dens"), g("vec_comp"), g("n_eip"), g("mort_a"),
                    if (mod != "exponential") g("mort_b") else 0, if (mod == "logistic") g("mort_s") else 0,
                    if (st == "stable") g("growth_r") else 0, if (st == "stable") g("first_bite") else 0)
  check(abs(app / by_hand - 1) < 1e-6, sprintf("About-tab equations reproduce the model (%s, %s)", mod, st))
}

# --- Paper comparison -------------------------------------------------------------------------------
pc1 <- paper_comparison(n = 3000, seed = 7); pc2 <- paper_comparison(n = 3000, seed = 7)
check(nrow(pc1) == 6 && identical(pc1, pc2), "paper comparison has six rows and reproduces with the same seed")
check(all(abs(pc1[["Difference from published (%)"]]) < 1), "deterministic column matches the published values within 1%")
uni <- "Uniform +/-20%"
check(all(pc1[[paste(uni, "2.5th percentile")]] < pc1$Deterministic & pc1$Deterministic < pc1[[paste(uni, "97.5th percentile")]]) &&
      all(abs(pc1[[paste(uni, "median")]] / pc1$Deterministic - 1) < 0.1), "uniform runs are centred on the deterministic value")
check(ncol(pc1) == 13 && all(pc1[["Literature-based 2.5th percentile"]] < pc1[["Literature-based median"]] &
      pc1[["Literature-based median"]] < pc1[["Literature-based 97.5th percentile"]]), "literature-based run is included, with an interval around its median")
# The literature-based column is the app's literature-based preset, run with the same trials and seed
lit_row <- paper_comparison()
for (k in 1:nrow(lit_row)) {
  m <- lit_row[["Mortality model"]][k]; st <- lit_row[["Age structure"]][k]
  ref <- run_model(m, st, mk_specs(m, st, "lit"), 10000, 2026)$ct
  check(isTRUE(all.equal(median(ref), lit_row[["Literature-based median"]][k])) &&
        isTRUE(all.equal(unname(quantile(ref, 0.975)), lit_row[["Literature-based 97.5th percentile"]][k])),
        sprintf("literature-based column equals the app's literature preset (%s, %s)", m, st))
}
fig <- tempfile(fileext = ".pdf"); pdf(fig); ok_fig <- tryCatch({ draw_paper(pc1); TRUE }, error = function(e) FALSE); dev.off()
check(ok_fig, "comparison figure draws from the table")

# The precomputed table shipped with the app must equal what the model gives now. If this fails, run deploy.R.
cache <- "app/www/pvec_comparison.csv"
fresh <- paper_comparison()
saved <- if (file.exists(cache)) read.csv(cache, check.names = FALSE, stringsAsFactors = FALSE)
check(!is.null(saved) && identical(names(saved), names(fresh)) && identical(saved[[1]], fresh[[1]]) && identical(saved[[2]], fresh[[2]]) &&
      all(abs(as.matrix(saved[-(1:2)]) - as.matrix(fresh[-(1:2)])) < 1e-8), "precomputed comparison table is up to date (otherwise run deploy.R)")
check(identical(paper_table(cache = cache), saved) && identical(paper_table(cache = "no_such_file.csv")[[1]], fresh[[1]]), "comparison table loads from the saved copy and falls back to computing it")

# --- Variance share, tail check and the other age structure -------------------------------------------------------
set.seed(11); nn <- 4000
x1 <- runif(nn); x2 <- runif(nn); x3 <- runif(nn)
yv <- 3 * x1 + 1 * x2 + rnorm(nn, sd = 0.2)                 # x1 explains most of the variance, x3 none
vi <- c(first_order_index(x1, yv), first_order_index(x2, yv), first_order_index(x3, yv))
check(vi[1] > 0.8 && vi[1] < 0.95 && vi[2] > 0.05 && vi[2] < 0.2, "variance share recovers the main effects of an additive model")
check(vi[3] < 0.02, "variance share is about zero for an input with no effect")
yi <- (x1 - 0.5) * (x2 - 0.5)                              # a pure interaction: neither input explains anything alone
check(first_order_index(x1, yi) + first_order_index(x2, yi) < 0.1, "variance shares credit nothing to inputs that only act together")
ym <- x1 * x2                                               # a milder interaction: main effects add up to less than 100%
check(first_order_index(x1, ym) + first_order_index(x2, ym) < 0.95, "variance shares add up to less than 100% when inputs interact")
check(first_order_index(rep(1, nn), yv) == 0 && first_order_index(x1, rep(2, nn)) == 0, "variance share handles constant inputs and constant outputs")

rl <- run_model("logistic", "stable", mk_specs("logistic", "stable", "lit"), 300, 5)
rl$model <- "logistic"
sc <- sens_contrib(rl)
check(!is.null(sc$vari) && all(sc$vari >= 0 & sc$vari <= 100) && setequal(names(sc$vari), names(sc$rho)), "sens_contrib reports a variance share for every varying input")

tt <- list(ct = exp(rnorm(2000)) , draws = data.frame(a = runif(2000), b = runif(2000)))
tt$ct <- tt$ct * (1 + 30 * (tt$draws$a < 0.02))             # the top of the forecast comes from low values of a
ti <- tail_info(tt)
check(!is.null(ti) && ti$drivers$id[1] == "a" && ti$drivers$med[1] < 0.1 && ti$heavy, "tail check finds the input that drives the heaviest trials")
check(is.null(tail_info(list(ct = rep(1, 500), draws = data.frame(a = runif(500))))) && is.null(tail_info(list(ct = rnorm(50), draws = data.frame(a = rnorm(50))))),
      "tail check stays quiet for fixed outputs and for small runs")
check(ordinal(0.01) == "1st" && ordinal(0.02) == "2nd" && ordinal(0.13) == "13th" && ordinal(0.93) == "93rd", "percentile labels read correctly")

for (mod in c("exponential", "logistic")) {
  rs <- run_model(mod, "stable", mk_specs(mod, "stable", "lit"), 200, 3); rs$model <- mod; rs$structure <- "stable"
  other <- ct_other_structure(rs, "stable", chunk = 70)
  check(max(abs(other / scalar_ct(mod, "synchronous", rs$draws, rs$b, rs$s) - 1)) < 1e-9, sprintf("the same draws run synchronously agree with the one-trial model (%s)", mod))
  ry <- run_model(mod, "synchronous", mk_specs(mod, "synchronous", "lit"), 200, 3); ry$model <- mod
  other2 <- ct_other_structure(ry, "synchronous", chunk = 70)
  dd <- ry$draws; dd$growth_r <- pop_specs$growth_r$value; dd$first_bite <- pop_specs$first_bite$value
  check(max(abs(other2 / scalar_ct(mod, "stable", dd, ry$b, ry$s) - 1)) < 1e-9, sprintf("the same draws run on the stable age distribution use the default r and first-bite age (%s)", mod))
}

# --- Temperature what-if and the comparison of two saved runs ----------------------------------------------------
tm <- temp_multipliers(2, -10, 5, 3)
check(abs(tm[["n_eip"]] - 0.9^2) < 1e-12 && abs(tm[["mort_a"]] - 1.05^2) < 1e-12 && abs(tm[["a_bite"]] - 1.03^2) < 1e-12, "temperature multipliers compound the percent change per degree")
check(all(temp_multipliers(0, -10, 5, 3) == 1), "no temperature change leaves every multiplier at 1")
sp0 <- mk_specs("logistic", "stable", "lit")
base <- run_model("logistic", "stable", sp0, 200, 9)
same <- run_model("logistic", "stable", sp0, 200, 9, adjust = c(n_eip = 1, mort_a = 1, a_bite = 1))
check(identical(base$ct, same$ct), "a multiplier of 1 gives the identical result")
warm <- run_model("logistic", "stable", sp0, 200, 9, adjust = temp_multipliers(3, -10, 5, 3))
check(all(warm$draws$mort_a > 0) && abs(median(warm$draws$mort_a / base$draws$mort_a) - 1.05^3) < 1e-9 &&
      abs(median(warm$draws$a_bite / base$draws$a_bite) - 1.03^3) < 1e-9, "the temperature what-if scales mortality a and the biting rate of every trial")
check(all(warm$draws$n_eip >= 1 & warm$draws$n_eip <= 150) && median(warm$draws$n_eip) < median(base$draws$n_eip), "warming shortens the incubation period and keeps it within 1 to 150 days")

sa <- c(model = "logistic", structure = "stable", trials = "1000", seed = "1", temp = "0", a_bite.dist = "Beta", a_bite.min = "0.25", a_bite.max = "0.76",
        a_bite.shape1 = "2", a_bite.shape2 = "3", n_eip.dist = "Fixed", n_eip.value = "10")
sb <- sa; sb[["n_eip.value"]] <- "8"; sb[["temp"]] <- "2"; sb[["trials"]] <- "2,000"
dd <- snap_diff(sa, sb)
check(setequal(dd$Setting, c("Trials", "Temperature change", labels[["n_eip"]])) && !("Seed" %in% dd$Setting), "comparing two saved runs lists exactly the settings that differ")
check(nrow(snap_diff(sa, sa)) == 0, "two identical saved runs show no differences")
check(identical(active_ids_for("exponential", "synchronous"), c(names(vc_specs), "mort_a")) && "growth_r" %in% active_ids_for("logistic", "stable"), "active assumptions follow the mortality model and age structure")

# --- Published temperature curves ---------------------------------------------------------------------------------
ae <- TEMP_CURVES$aedes_dengue; an <- TEMP_CURVES$anopheles_pf
check(abs(ae$mu(27.6) - 0.0273) < 0.001 && abs(ae$n(25) - (4 + exp(5.15 - 0.123 * 25))) < 1e-12 && abs(ae$a(25) - 0.2018) < 1e-9,
      "the Aedes aegypti and dengue curves return the published equations' values (lowest mortality near 27.6 C)")
check(abs(an$mu(25) - 1 / 9.6) < 1e-12 && abs(an$n(26) - 11.1) < 1e-12 && is.null(an$a), "the Anopheles and Plasmodium falciparum curves follow Martens mortality and 111 degree-days above 16 C")
for (cv in names(TEMP_CURVES)) check(all(curve_multipliers(cv, TEMP_CURVES[[cv]]$ref, 0)$mult == 1), sprintf("no temperature change leaves every multiplier at 1 on the %s curve", cv))
w <- curve_multipliers("aedes_dengue", 27, 3)
check(w$mult[["n_eip"]] < 1 && w$mult[["a_bite"]] > 1 && length(w$outside) == 0, "warming shortens incubation and raises biting on the Aedes curve, inside its fitted ranges")
hot <- curve_multipliers("aedes_dengue", 27, 8)
check(setequal(hot$outside, c("n", "mu", "a")) || all(c("mu", "a") %in% hot$outside), "going past the fitted range is flagged")
check(is.null(temperature_adjust("generic", NULL, 0, -10, 5, 3)) && !is.null(temperature_adjust("generic", NULL, 2, -10, 5, 3)) &&
      identical(temperature_adjust("generic", NULL, 2, -10, 5, 3)$mult, temp_multipliers(2, -10, 5, 3)), "the generic temperature option uses the per-degree changes and no change at 0")

if (!all(unlist(results))) stop(sprintf("%d check(s) failed", sum(!unlist(results))), call. = FALSE)
cat(sprintf("\nAll %d checks passed.\n", length(results)))
