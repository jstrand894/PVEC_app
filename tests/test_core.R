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

# --- Paper comparison -------------------------------------------------------------------------------
pc1 <- paper_comparison(n = 3000, seed = 7); pc2 <- paper_comparison(n = 3000, seed = 7)
check(nrow(pc1) == 6 && identical(pc1, pc2), "paper comparison has six rows and reproduces with the same seed")
check(all(abs(pc1[["Difference from published (%)"]]) < 1), "deterministic column matches the published values within 1%")
check(all(pc1[["2.5th percentile"]] < pc1$Deterministic & pc1$Deterministic < pc1[["97.5th percentile"]]) &&
      all(abs(pc1[["Probabilistic median"]] / pc1$Deterministic - 1) < 0.1), "probabilistic runs are centred on the deterministic value")

if (!all(unlist(results))) stop(sprintf("%d check(s) failed", sum(!unlist(results))), call. = FALSE)
cat(sprintf("\nAll %d checks passed.\n", length(results)))
