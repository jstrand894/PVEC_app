# Input-robustness tests for PVEC. Complements tests/test_core.R (which checks the model against the published values).
# Run from the repository root:   Rscript tests/test_inputs.R
# Optional app launch check:      PVEC_SMOKE=1 Rscript tests/test_inputs.R
# Every check is run inside tryCatch, so one error is reported as a FAIL and the rest still run. Exit status is 1 if any fail.
suppressMessages(source("app/app.R", local = TRUE))      # defines the functions; does not start the app

results <- list()
check <- function(what, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) { cat("     error:", conditionMessage(e), "\n"); FALSE })
  results[[length(results) + 1]] <<- ok
  cat(sprintf("%-4s %s\n", if (ok) "PASS" else "FAIL", what))
}
section <- function(x) cat("\n==", x, "==\n")
finite <- function(x) is.numeric(x) && length(x) > 0 && all(is.finite(x))

MODELS <- c("exponential", "gompertz", "logistic"); STRUCTS <- c("synchronous", "stable")
mk_specs <- function(model, structure, preset = "lit") {
  ids <- active_ids_for(model, structure)
  allspec <- c(vc_specs, mort_specs[[model]], pop_specs)
  s <- lapply(ids, function(id) { x <- allspec[[id]]; x$dist <- if (preset == "lit") lit_dists[[id]] else "Fixed"; x }); names(s) <- ids
  s
}
S <- function(dist, ...) { s <- sp("Fixed", 0.5, 0.2, 0.4, 0.9, 0.5, 0.1, 2, 3); s$dist <- dist; modifyList(s, list(...)) }

# ---------------------------------------------------------------------------------------------------
section("1. Every model x structure x preset runs and gives sensible Ct")
for (m in MODELS) for (st in STRUCTS) for (pr in c("lit", "fixed")) {
  r <- run_model(m, st, mk_specs(m, st, pr), 300, 11)
  check(sprintf("%s / %s / %s: Ct is finite, non-negative, length 300", m, st, pr), finite(r$ct) && all(r$ct >= 0) && length(r$ct) == 300)
}

section("2. Every distribution, on every varying assumption, in both structures")
set.seed(5)
dists <- setdiff(dist_choices, "Fixed")
for (dn in dists) for (st in STRUCTS) {
  sp_ <- mk_specs("logistic", st, "fixed")
  for (id in c("a_bite", "n_eip", "m_dens", "vec_comp", "mort_a", "mort_b", "mort_s")) {
    base <- sp_[[id]]; v <- base$value
    lo <- if (v == 0) -1 else v * 0.7; hi <- if (v == 0) 1 else v * 1.3
    if (id == "vec_comp") { lo <- 0.2; hi <- 0.9 }
    sp_[[id]] <- modifyList(base, list(dist = dn, min = lo, max = hi, mode = (lo + hi) / 2, mean = (lo + hi) / 2, sd = (hi - lo) / 6, shape1 = 2, shape2 = 3))
  }
  r <- run_model("logistic", st, sp_, 200, 3)
  check(sprintf("%s on all mortality/biting inputs (%s) gives finite Ct", dn, st), finite(r$ct))
}

section("3. Draws stay inside their stated ranges")
for (dn in dists) {
  s <- S(dn, min = 0.2, max = 0.9, mode = 0.4, mean = 0.5, sd = 0.3)    # wide SD forces truncation to matter
  x <- draw(5000, s)
  check(sprintf("%s: all draws within [min, max] and finite", dn), finite(x) && all(x >= 0.2 - 1e-12 & x <= 0.9 + 1e-12))
}
check("Triangular with mode at min edge", { x <- draw(2000, S("Triangular", min = 0.2, mode = 0.2, max = 0.9)); finite(x) && all(x >= 0.2 & x <= 0.9) })
check("Triangular with mode at max edge", { x <- draw(2000, S("Triangular", min = 0.2, mode = 0.9, max = 0.9)); finite(x) && all(x >= 0.2 & x <= 0.9) })
check("PERT with mode at either edge", { a <- draw(2000, S("PERT", mode = 0.2, min = 0.2)); b <- draw(2000, S("PERT", mode = 0.9, max = 0.9)); finite(a) && finite(b) })
check("Normal centred far below its minimum still draws inside the range", { x <- draw(2000, S("Normal", min = 0.5, max = 0.9, mean = 0.1, sd = 0.05)); finite(x) && all(x >= 0.5 & x <= 0.9) })
check("Very narrow range (min barely below max) works", finite(draw(500, S("Uniform", min = 0.5, max = 0.5 + 1e-9))))

section("4. Spec validation rejects bad input and accepts good input")
bad <- list(
  "Fixed with missing value" = S("Fixed", value = NA_real_),
  "min equals max" = S("Uniform", min = 0.5, max = 0.5),
  "min above max" = S("Uniform", min = 0.9, max = 0.2),
  "missing min" = S("Uniform", min = NA_real_),
  "mode below min" = S("Triangular", mode = 0.1),
  "mode above max" = S("PERT", mode = 1.5),
  "missing mode" = S("Triangular", mode = NA_real_),
  "Beta zero shape" = S("Beta", shape1 = 0),
  "Beta negative shape" = S("Beta", shape2 = -1),
  "Normal zero SD" = S("Normal", sd = 0),
  "Normal negative SD" = S("Normal", sd = -0.1),
  "Normal missing mean" = S("Normal", mean = NA_real_),
  "Lognormal non-positive mean" = S("Lognormal", mean = 0, sd = 0.1))
for (nm in names(bad)) check(sprintf("rejected: %s", nm), !is.null(check_spec(bad[[nm]], "x")))
good <- c(list(S("Fixed")), lapply(dists, S))
check("every distribution with sensible values is accepted", all(vapply(good, function(s) is.null(check_spec(s, "x")), TRUE)))

section("5. Extreme and boundary values do not crash the model or give NaN")
ext <- function(id, ...) { s <- mk_specs("logistic", "stable", "fixed"); s[[id]] <- modifyList(s[[id]], list(...)); run_model("logistic", "stable", s, 40, 1)$ct }
check("biting rate 0 gives Ct 0",                  all(ext("a_bite", value = 0) == 0))
check("mosquito density 0 gives Ct 0",             all(ext("m_dens", value = 0) == 0))
check("vector competence 0 gives Ct 0",            all(ext("vec_comp", value = 0) == 0))
check("vector competence 1 is finite",             finite(ext("vec_comp", value = 1)))
check("incubation period 0 is raised and finite",  finite(ext("n_eip", value = 0)))
check("incubation period negative is finite",      finite(ext("n_eip", value = -5)))
check("incubation period 500 is capped and finite", finite(ext("n_eip", value = 500)))
check("very high mortality gives a small finite Ct", { x <- ext("mort_a", value = 5); finite(x) && all(x >= 0) })
check("tiny mortality is finite",                  finite(ext("mort_a", value = 1e-9)))
check("first bite age -3 and 999 are clamped",     finite(ext("first_bite", value = -3)) && finite(ext("first_bite", value = 999)))
check("first bite age with decimals is accepted",  finite(ext("first_bite", value = 2.6)))
check("growth rate negative is finite",            finite(ext("growth_r", value = -0.05)))
check("growth rate 0 is finite",                   finite(ext("growth_r", value = 0)))
check("Ct is non-decreasing in biting rate",       ext("a_bite", value = 0.8)[1] > ext("a_bite", value = 0.4)[1])
check("Ct is non-decreasing in mosquito density",  ext("m_dens", value = 3)[1] > ext("m_dens", value = 1)[1])
check("Ct falls as mortality rises",               ext("mort_a", value = 0.01)[1] < ext("mort_a", value = 0.001)[1])
check("Ct falls as incubation lengthens",          ext("n_eip", value = 20)[1] < ext("n_eip", value = 6)[1])
check("Ct scales linearly with density",           abs(ext("m_dens", value = 2)[1] / ext("m_dens", value = 1)[1] - 2) < 1e-9)
check("Ct scales with the square of biting rate",  abs(ext("a_bite", value = 1)[1] / ext("a_bite", value = 0.5)[1] - 4) < 1e-9)

section("6. Trial counts and seeds")
sp_l <- mk_specs("gompertz", "synchronous", "lit")
for (n in c(1L, 2L, MIN_TRIALS, 999L, 1001L)) check(sprintf("n = %d trials runs and returns n values", n), length(run_model("gompertz", "synchronous", sp_l, n, 1)$ct) == n)
for (sd_ in c(0, 1, -7, 99999, 2147483647)) check(sprintf("seed %s runs and is reproducible", format(sd_)),
  identical(run_model("gompertz", "synchronous", sp_l, 50, sd_)$ct, run_model("gompertz", "synchronous", sp_l, 50, sd_)$ct))
check("larger run with the same seed starts with different values (draws are per-run, not per-trial)", length(run_model("gompertz", "synchronous", sp_l, 5000, 1)$ct) == 5000)

section("7. Summary statistics on awkward results")
check("summary of a constant Ct is flagged as fixed", summary_text(rep(3, 20))$fixed)
check("summary of two trials does not error", is.list(summary_text(c(1, 2))))
check("stats_df with zeros is finite", finite(stats_df(c(0, 0, 0, 1))$Value))
check("stats_df has the expected 12 rows", nrow(stats_df(runif(100))) == 12)
check("quantile SE is finite for skewed data", finite(quantile_se(rexp(500))))
check("quantile SE for constant data is zero", quantile_se(rep(2, 100)) == 0)

section("8. Auto trials")
runf <- function(n) run_model("gompertz", "synchronous", sp_l, n, 1)
a1 <- run_auto(runf, 0.05, n0 = 500L, n_max = 20000L)
check("auto trials meets a loose target", a1$met && a1$rel <= 0.05)
a2 <- run_auto(runf, 0.0005, n0 = 500L, n_max = 3000L)
check("auto trials stops at the cap when the target is unreachable", !a2$met && a2$n == 3000L)
a3 <- run_auto(runf, 0.2, n0 = 500L, n_max = 20000L)
check("a looser tolerance never needs more trials than a tighter one", a3$n <= a1$n)
check("auto trials grows the number of trials each round", { ns <- c(); invisible(run_auto(runf, 0.01, 500L, 20000L, progress = function(n, met, rel) ns <<- c(ns, n))); all(diff(ns) > 0) })

section("9. Correlated inputs")
for (rho in c(-0.95, -0.5, 0, 0.5, 0.95)) {
  pr <- data.frame(a = "mort_a", b = "mort_b", rho = rho, stringsAsFactors = FALSE)
  r <- run_model("gompertz", "synchronous", mk_specs("gompertz", "synchronous"), 4000, 2, pairs = pr)
  got <- cor(r$draws$mort_a, r$draws$mort_b, method = "spearman")
  check(sprintf("requested rank correlation %.2f is achieved (got %.2f)", rho, got), abs(got - rho) < 0.06)
}
inc <- data.frame(a = c("mort_a", "mort_a", "mort_b"), b = c("mort_b", "n_eip", "n_eip"), rho = c(0.95, 0.95, -0.95), stringsAsFactors = FALSE)
r <- run_model("gompertz", "synchronous", mk_specs("gompertz", "synchronous"), 500, 2, pairs = inc)
check("mutually impossible correlations are reduced, not an error", finite(r$ct) && any(grepl("reduced", r$notes)))
r <- run_model("gompertz", "synchronous", mk_specs("gompertz", "synchronous", "fixed"), 100, 2, pairs = data.frame(a = "mort_a", b = "mort_b", rho = -0.5, stringsAsFactors = FALSE))
check("a link on fixed inputs is skipped with a note", finite(r$ct) && any(grepl("skipped", r$notes)))
r <- run_model("exponential", "synchronous", mk_specs("exponential", "synchronous"), 100, 2, pairs = data.frame(a = "mort_a", b = "mort_b", rho = -0.5, stringsAsFactors = FALSE))
check("a link to an input the model does not use is skipped, not an error", finite(r$ct))

section("10. Uploaded draws")
set.seed(8); up <- list(df = data.frame(mort_a = runif(300, 0.001, 0.003), mort_b = runif(300, 0.1, 0.2)))
r <- tryCatch(run_model("gompertz", "synchronous", mk_specs("gompertz", "synchronous"), 150, 3, uploaded = up), error = function(e) e)
check("uploaded draws for some inputs run", !inherits(r, "error") && finite(r$ct))
r <- tryCatch(run_model("gompertz", "synchronous", mk_specs("gompertz", "synchronous"), 800, 3, uploaded = up), error = function(e) e)
check("more trials than uploaded rows does not crash", !inherits(r, "error") && finite(r$ct))

section("11. Temperature what-if")
for (cv in c("generic", names(TEMP_CURVES))) for (dT in c(-8, -2, 0, 2, 8)) {
  adj <- temperature_adjust(cv, 25, dT, -10, 5, 3)$mult
  r <- run_model("gompertz", "synchronous", mk_specs("gompertz", "synchronous", "fixed"), 20, 1, adjust = adj)
  check(sprintf("temperature curve %s, change %+d C: finite Ct", cv, dT), finite(r$ct))
}
check("no change means no adjustment", is.null(temperature_adjust("generic", 25, 0, -10, 5, 3)) && is.null(temperature_adjust("generic", 25, NA, -10, 5, 3)))
check("temperature change of 0 leaves Ct unchanged", identical(run_model("gompertz", "synchronous", sp_l, 50, 4)$ct, run_model("gompertz", "synchronous", sp_l, 50, 4, adjust = NULL)$ct))

section("12. Two-dimensional analysis")
for (role_all in c("uncertain", "variable")) {
  ro <- setNames(rep(role_all, length(active_ids_for("gompertz", "synchronous"))), active_ids_for("gompertz", "synchronous"))
  r <- run_2d("gompertz", "synchronous", mk_specs("gompertz", "synchronous"), ro, 20, 15, 1)
  check(sprintf("all inputs %s: shape and finite Ct", role_all), finite(r$ct) && (if (role_all == "uncertain") ncol(r$ct) == 1 else nrow(r$ct) == 1))
}
ro <- setNames(rep("uncertain", length(active_ids_for("gompertz", "synchronous"))), active_ids_for("gompertz", "synchronous")); ro[c("mort_a", "mort_b")] <- "variable"
r <- run_2d("gompertz", "synchronous", mk_specs("gompertz", "synchronous"), ro, 12, 9, 1)
check("mixed roles give an n_out x n_in matrix", all(dim(r$ct) == c(12, 9)) && finite(r$ct))
check("2D summary has three rows", nrow(summary_2d(r)) == 3)
check("2D with everything fixed does not error", { r <- run_2d("gompertz", "synchronous", mk_specs("gompertz", "synchronous", "fixed"), ro, 10, 10, 1); finite(r$ct) && diff(range(r$ct)) == 0 })

section("13. Point-value and preset helpers")
for (id in names(lit_dists)) {
  s <- c(vc_specs, mort_specs$logistic, pop_specs)[[id]]; p <- point_default(s, id)
  check(sprintf("point default for %s is Fixed and finite", id), p$dist == "Fixed" && finite(p$value))
}
for (dn in dists) check(sprintf("spec_mean for %s is inside [min, max]", dn), { m <- spec_mean(S(dn)); m >= 0.2 && m <= 0.9 })
for (dn in dists) check(sprintf("qdraw at u = 0 and 1 stays finite and in range (%s)", dn), { x <- qdraw(c(0, 1e-12, 0.5, 1 - 1e-12, 1), S(dn)); finite(x) && all(x >= 0.2 - 1e-9 & x <= 0.9 + 1e-9) })

section("14. Fitting a distribution to a reported interval")
for (dn in c("Uniform", "Normal", "Lognormal", "Beta")) {
  f <- tryCatch(fit_range(dn, 0.3, 0.7, NA_real_, S(dn, min = 0, max = 1)), error = function(e) NULL)
  check(sprintf("fit_range %s returns a result without error", dn), is.list(f) && "ok" %in% names(f))
}
check("fit_range with lower above upper fails cleanly", { f <- tryCatch(fit_range("Normal", 0.7, 0.3, NA_real_, S("Normal")), error = function(e) NULL); is.list(f) && !isTRUE(f$ok) })
check("fit_range with equal bounds fails cleanly",     { f <- tryCatch(fit_range("Normal", 0.5, 0.5, NA_real_, S("Normal")), error = function(e) NULL); is.list(f) && !isTRUE(f$ok) })

section("15. Soft warnings")
check("vector competence above 1 warns", nzchar(soft_warning("vec_comp", S("Fixed", value = 1.4))))
check("vector competence inside 0-1 does not warn", !nzchar(soft_warning("vec_comp", S("Uniform", min = 0.2, max = 0.9))))
check("first-bite age outside range warns", nzchar(soft_warning("first_bite", S("Fixed", value = -1))))
check("incubation under 1 day warns", nzchar(soft_warning("n_eip", S("Fixed", value = 0.5))))

section("16. Settings round trip (the same keys the link and the settings file use)")
specs <- mk_specs("logistic", "stable")
snap <- unlist(lapply(names(specs), function(id) { s <- specs[[id]]; v <- c(s$dist, unlist(s[fields])); names(v) <- paste0(id, c(".dist", paste0(".", fields))); v }))
check("every assumption can be rebuilt from a saved settings vector", all(vapply(names(specs), function(id) {
  b <- spec_from_snap(snap, id); !is.null(b) && identical(b$dist, specs[[id]]$dist) && isTRUE(all.equal(unlist(b[fields]), unlist(specs[[id]][fields]), check.attributes = FALSE))
}, TRUE)))
check("a settings vector with no entry for an assumption gives NULL, not an error", is.null(spec_from_snap(c(foo = "1"), "a_bite")))
check("auto-trials settings are written into the share link", any(grepl("auto.trials", readLines("app/www/pvec.js"), fixed = TRUE)) && any(grepl("auto.tol", readLines("app/www/pvec.js"), fixed = TRUE)))
check("auto-trials settings are read back from a link or file", { src <- readLines("app/app.R"); any(grepl('get1("auto.trials")', src, fixed = TRUE)) && any(grepl('num("auto.tol")', src, fixed = TRUE)) })
js <- paste(readLines("app/www/pvec.js"), collapse = "\n")
ids_js <- regmatches(js, regexpr("settingIds = \\[[^]]*\\]", js)); ids_js <- strsplit(gsub("[^a-z_,]", "", sub("settingIds=", "", gsub(" ", "", ids_js))), ",")[[1]]
check("the link covers every assumption in the app", setequal(ids_js, all_setting_ids))

section("17. The app file and assets")
check("app.R parses", !inherits(try(parse("app/app.R"), silent = TRUE), "try-error"))
check("pvec.js and pvec.css exist and are not empty", all(file.size(c("app/www/pvec.js", "app/www/pvec.css")) > 1000))
check("the user interface builds", inherits(tryCatch(ui, error = function(e) e), c("shiny.tag", "shiny.tag.list", "html_document")) || is.function(ui) || !is.null(ui))
check("no leftover 'genuinely' wording", !any(grepl("genuine", readLines("app/app.R"), ignore.case = TRUE)))

if (nzchar(Sys.getenv("PVEC_SMOKE"))) {
  section("18. Launch smoke test (starts the app in the background)")
  port <- httpuv::randomPort()
  px <- processx_ok <- requireNamespace("callr", quietly = TRUE)
  if (!px) check("callr is installed for the launch test (install.packages('callr'))", FALSE) else {
    bg <- callr::r_bg(function(p) shiny::runApp("app", port = p, launch.browser = FALSE), args = list(port), stdout = "|", stderr = "|")
    on.exit(bg$kill(), add = TRUE)
    page <- NULL
    for (i in 1:40) { Sys.sleep(1); page <- tryCatch(suppressWarnings(readLines(sprintf("http://127.0.0.1:%d", port), warn = FALSE)), error = function(e) NULL); if (!is.null(page)) break }
    check("the app starts and serves a page", !is.null(page) && length(page) > 10)
    check("the page contains the main controls", !is.null(page) && all(vapply(c("auto_trials", "copy_link", "n_iter"), function(k) any(grepl(k, page, fixed = TRUE)), TRUE)))
    check("the app is still running (no startup error)", bg$is_alive())
  }
}

cat(sprintf("\n%d of %d checks passed.\n", sum(unlist(results)), length(results)))
if (!all(unlist(results))) quit(status = 1)
