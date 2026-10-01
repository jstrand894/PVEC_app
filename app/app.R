library(shiny)

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

# ---- Model pipeline (same math as Parts 3 to 5) ----
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

# Vectorized version of the Part 5 loop, identical result
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

# ---- Styer validation and r, computed once at startup ----
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

# ---- Default assumptions ----
sp <- function(dist, value, min, mode, max, mean, sd, shape1 = 2, shape2 = 2)
  list(dist = dist, value = value, min = min, mode = mode, max = max,
       mean = mean, sd = sd, shape1 = shape1, shape2 = shape2)

vc_specs <- list(
  a_bite   = sp("Fixed", 0.75, 0.25, 0.42,  0.76, 0.45,  0.10, 2, 3),
  n_eip    = sp("Fixed", 10,   4,    6.5,   14,   9,     2.5),
  m_dens   = sp("Fixed", 1.5,  0.42, 0.465, 0.51, 0.465, 0.03),
  vec_comp = sp("Fixed", 1,    0.25, 0.525, 0.8,  0.525, 0.12, 2, 2))

mort_specs <- list(
  logistic = list(
    mort_a = sp("Fixed", 0.0018, 1e-6, 0.0018, 0.01, 0.0018, 0.0004),
    mort_b = sp("Fixed", 0.1416, 1e-6, 0.1416, 0.5,  0.1416, 0.017),
    mort_s = sp("Fixed", 1.0730, 0,    1.0730, 5,    1.0730, 0.234)),
  gompertz = list(
    mort_a = sp("Fixed", 0.0066, 1e-6, 0.0066, 0.03, 0.0066, 0.0015),
    mort_b = sp("Fixed", 0.0623, 1e-6, 0.0623, 0.3,  0.0623, 0.017),
    mort_s = sp("Fixed", 0,      0,    0,      5,    0,      0.1)),
  exponential = list(
    mort_a = sp("Fixed", 0.0313, 1e-6, 0.0313, 0.1,  0.0313, 0.007),
    mort_b = sp("Fixed", 0,      0,    0,      1,    0,      0.1),
    mort_s = sp("Fixed", 0,      0,    0,      5,    0,      0.1)))

# Distributions used in the Part 10 run
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

# ---- Assumption box UI ----
shows <- function(id, dists)
  sprintf("['%s'].indexOf(input.%s_dist) > -1", paste(dists, collapse = "','"), id)

num <- function(id, f, label, s) numericInput(paste0(id, "_", f), label, s[[f]], width = "100%")

assumption_ui <- function(id, s) {
  wellPanel(
    strong(labels[[id]]),
    selectInput(paste0(id, "_dist"), "Distribution", dist_choices, s$dist),
    conditionalPanel(shows(id, "Fixed"), num(id, "value", "Value", s)),
    conditionalPanel(shows(id, setdiff(dist_choices, "Fixed")),
      fluidRow(column(6, num(id, "min", "Min", s)), column(6, num(id, "max", "Max", s)))),
    conditionalPanel(shows(id, c("Triangular", "PERT")), num(id, "mode", "Likeliest", s)),
    conditionalPanel(shows(id, "Beta"),
      fluidRow(column(6, num(id, "shape1", "Shape 1", s)), column(6, num(id, "shape2", "Shape 2", s)))),
    conditionalPanel(shows(id, c("Normal", "Lognormal")),
      fluidRow(column(6, num(id, "mean", "Mean", s)), column(6, num(id, "sd", "SD", s))),
      helpText("Truncated at min and max"))
  )
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("Vectorial capacity Monte Carlo, Styer et al. 2007 reconstruction"),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("mort_model", "Mortality model",
                  c("Logistic" = "logistic", "Gompertz" = "gompertz", "Exponential" = "exponential")),
      radioButtons("structure", "Population age structure",
                   c("Stable age distribution" = "stable", "Synchronous emergence" = "synchronous")),
      numericInput("r", "Intrinsic rate of increase r", round(r_hat, 4), step = 0.01),
      numericInput("sigma", "Age at first bite (days)", 3, min = 0, max = 20),
      selectInput("n_iter", "Number of trials", c(500, 1000, 5000, 10000), 1000),
      numericInput("seed", "Random seed", 1),
      hr(),
      strong("Load a preset"), br(), br(),
      actionButton("preset_styer", "Styer 2007 fixed values", width = "100%"), br(), br(),
      actionButton("preset_lit", "Literature ranges (Part 10)", width = "100%"),
      hr(),
      actionButton("run", "Run simulation", class = "btn-primary btn-lg", width = "100%"),
      br(), br(),
      textOutput("run_status")
    ),
    mainPanel(width = 9,
      tabsetPanel(id = "tabs",
        tabPanel("Define assumptions",
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
          fluidRow(
            column(3, numericInput("cert_lo", "Certainty range, lower", NA)),
            column(3, numericInput("cert_hi", "Certainty range, upper", NA)),
            column(6, h4(textOutput("certainty")))),
          plotOutput("forecast_plot", height = 400),
          tableOutput("stats")),
        tabPanel("Sensitivity", plotOutput("sens_plot", height = 400)),
        tabPanel("Assumption draws", plotOutput("draws_plot", height = 600)),
        tabPanel("Survival curves", plotOutput("surv_plot", height = 450)),
        tabPanel("Validation",
          p("Reconstruction of Styer et al. 2007 with their fixed constants. Published values come",
            "from the Figure 5 inset and are never used as inputs, except that r is backed out of",
            "the exponential stable case."),
          tableOutput("validation"))
      )
    )
  )
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

  # Swap in fitted values when the mortality model changes, keep chosen distributions
  observeEvent(input$mort_model, {
    for (id in names(mort_specs[[input$mort_model]])) {
      s <- mort_specs[[input$mort_model]][[id]]
      s$dist <- input[[paste0(id, "_dist")]]
      set_spec(id, s)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$preset_styer, {
    all <- c(vc_specs, mort_specs[[input$mort_model]])
    for (id in names(all)) set_spec(id, all[[id]])
  })

  observeEvent(input$preset_lit, {
    all <- c(vc_specs, mort_specs[[input$mort_model]])
    for (id in names(all)) {
      s <- all[[id]]
      s$dist <- lit_dists[[id]]
      set_spec(id, s)
    }
  })

  results_val <- reactiveVal(NULL)
  run_count   <- reactiveVal(0)
  run_info    <- reactiveVal("No runs yet")

  # Everything downstream waits until at least one run exists
  results <- reactive(req(results_val()))

  output$run_status <- renderText(run_info())

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

    results_val(list(ct = ct, draws = d, model = model, b = b_i, s = s_i))
    run_count(run_count() + 1)

    secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    msg  <- sprintf("Run %d finished at %s, %s trials in %.1f seconds",
                    run_count(), format(Sys.time(), "%I:%M:%S %p"),
                    format(n, big.mark = ","), secs)
    run_info(msg)

    # Skip the pop up and tab switch for the automatic run when the app first opens
    if (input$run > 0) {
      showNotification(msg, type = "message", duration = 5)
      if (input$tabs == "Define assumptions") updateTabsetPanel(session, "tabs", selected = "Forecast")
    }
  }, ignoreNULL = FALSE)

  cert_bounds <- reactive(c(
    if (is.na(input$cert_lo)) -Inf else input$cert_lo,
    if (is.na(input$cert_hi))  Inf else input$cert_hi))

  output$certainty <- renderText({
    ct <- results()$ct
    b  <- cert_bounds()
    sprintf("Certainty is %.1f%% that Ct falls in this range", 100 * mean(ct >= b[1] & ct <= b[2]))
  })

  output$forecast_plot <- renderPlot({
    ct <- results()$ct
    b  <- cert_bounds()
    h  <- hist(ct, breaks = 50, plot = FALSE)
    plot(h, col = ifelse(h$mids >= b[1] & h$mids <= b[2], "steelblue", "grey85"),
         border = "white", main = "Forecast of total vectorial capacity", xlab = "Ct")
    abline(v = median(ct), lwd = 2, lty = 2)
  })

  output$stats <- renderTable({
    ct <- results()$ct
    q  <- quantile(ct, c(0.025, 0.10, 0.25, 0.50, 0.75, 0.90, 0.975))
    data.frame(
      Statistic = c("Trials", "Mean", "SD", "Min", "Max",
                    "2.5th percentile", "10th percentile", "25th percentile", "Median",
                    "75th percentile", "90th percentile", "97.5th percentile"),
      Value = c(length(ct), mean(ct), sd(ct), min(ct), max(ct), q))
  }, digits = 3)

  output$sens_plot <- renderPlot({
    res    <- results()
    d      <- res$draws
    varied <- names(d)[sapply(d, function(v) sd(v) > 0)]
    if (length(varied) == 0 || sd(res$ct) == 0) {
      plot.new(); text(0.5, 0.5, "No assumptions are varying, so there is nothing to rank")
      return(invisible())
    }
    rho     <- sapply(varied, function(v) cor(d[[v]], res$ct, method = "spearman"))
    contrib <- sort(100 * sign(rho) * rho^2 / sum(rho^2))
    par(mar = c(5, 17, 3, 2))
    barplot(contrib, horiz = TRUE, las = 1, names.arg = labels[names(contrib)],
            col = ifelse(contrib > 0, "steelblue", "firebrick"),
            xlab = "Contribution to variance (%)", main = "Sensitivity of Ct to each assumption")
    abline(v = 0)
  })

  output$draws_plot <- renderPlot({
    d <- results()$draws
    par(mfrow = c(ceiling(ncol(d) / 3), 3))
    for (v in names(d))
      hist(d[[v]], breaks = 40, col = "grey70", border = "white", main = labels[[v]], xlab = "")
  })

  output$surv_plot <- renderPlot({
    res <- results()
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
  })

  output$validation <- renderTable(validation, digits = 2)
}

shinyApp(ui, server)