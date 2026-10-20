# Compares three ways of computing vectorial capacity across a grid of scenarios.
# Run from the repository root:  Rscript analysis/scenario_comparison.R
#   1. Static VC           classical m a^2 V exp(-mu n) / mu with one constant daily mortality mu, no age structure
#   2. Age-dependent VC    one set of point values, Styer's age-dependent mortality (Gompertz / logistic), Ct from the app
#   3. Probabilistic VC    the same model with the "Uncertain ranges" preset (and Uniform +/-20% for the Styer inputs)
# In the EIP rows the incubation period is held at that value and the other inputs vary. Static VC uses the constant hazard whose
# mean lifespan equals the mortality model's (Styer's exponential fit, mu = 0.0313, for the exponential rows).
# Writes analysis/scenario_comparison.csv and analysis/scenario_comparison.md.
suppressMessages(source("app/app.R", local = TRUE))
N <- 10000; SEED <- 2026

# Input sets: Styer's published values, the app's Point values (centre of the Uncertain ranges), and EIP sweeps around the latter
app_pt <- list(a_bite = 0.454, n_eip = 9, m_dens = 0.46, vec_comp = 0.525)
styer  <- list(a_bite = 0.75,  n_eip = 10, m_dens = 1.5, vec_comp = 1)
inputs <- list(
  list(label = "Styer inputs",            pt = styer,                         prob = "uniform20"),
  list(label = "App point values",        pt = app_pt,                        prob = "lit"),
  list(label = "App, EIP 6.5 d (held)",  pt = modifyList(app_pt, list(n_eip = 6.5)), prob = "lit", hold = "n_eip"),
  list(label = "App, EIP 14 d (held)",    pt = modifyList(app_pt, list(n_eip = 14)),  prob = "lit", hold = "n_eip"))

specs_for <- function(m, st, pt, prob, hold = character()) {   # `hold`: inputs kept at their point value
  ids  <- c(names(vc_specs), "mort_a", if (m != "exponential") "mort_b", if (m == "logistic") "mort_s", if (st == "stable") names(pop_specs))
  base <- c(vc_specs, mort_specs[[m]], pop_specs)[ids]
  Map(function(x, id) {
    if (id %in% names(pt)) x$value <- pt[[id]]
    if (prob == "point" || id %in% hold) { x$dist <- "Fixed"; return(x) }
    if (prob == "uniform20" && !id %in% names(pop_specs)) { x$dist <- "Uniform"; x$min <- x$value * 0.8; x$max <- x$value * 1.2; return(x) }
    if (prob == "lit") { x$dist <- lit_dists[[id]]; return(x) }
    x$dist <- "Fixed"; x }, base, names(base))
}

static_vc <- function(pt, mu) pt$m_dens * pt$a_bite^2 * pt$vec_comp * exp(-mu * pt$n_eip) / mu

rows <- list()
for (inp in inputs) for (m in names(styer_pars)) for (st in c("synchronous", "stable")) {
  p <- styer_pars[[m]]
  pt_specs <- specs_for(m, st, inp$pt, "point")
  e0   <- life_stats(m, p[["a"]], p[["b"]], p[["s"]])$e0
  mu_s <- exp_equivalent_hazard(e0)                                   # constant hazard with the same mean lifespan
  det  <- run_model(m, st, pt_specs, 2, SEED)$ct[1]
  pr   <- run_model(m, st, specs_for(m, st, inp$pt, inp$prob, inp$hold), N, SEED)$ct
  rows[[length(rows) + 1]] <- data.frame(
    Inputs = inp$label, Mortality = m, Structure = st, `Mean lifespan (d)` = e0,
    `Static VC` = static_vc(inp$pt, mu_s),
    `Age-dependent VC` = det,
    `Prob. median` = median(pr), `Prob. mean` = mean(pr),
    `Prob. 2.5%` = unname(quantile(pr, 0.025)), `Prob. 97.5%` = unname(quantile(pr, 0.975)),
    `Prob. median / age-dep.` = median(pr) / det,
    `Static / age-dep.` = static_vc(inp$pt, mu_s) / det,
    `Share of prob. runs above age-dep.` = mean(pr > det),
    check.names = FALSE)
}
res <- do.call(rbind, rows)
write.csv(res, "analysis/scenario_comparison.csv", row.names = FALSE)

fmt <- function(x) ifelse(abs(x) >= 100, sprintf("%.0f", x), ifelse(abs(x) >= 10, sprintf("%.1f", x), sprintf("%.2f", x)))
out <- res; for (j in 4:ncol(out)) out[[j]] <- fmt(res[[j]])
lines <- c(paste0("| ", paste(names(out), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(out)), collapse = "|"), "|"),
           apply(out, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
writeLines(lines, "analysis/scenario_comparison.md")
options(width = 250); print(out, row.names = FALSE)
