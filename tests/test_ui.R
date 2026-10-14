# Browser-level tests for PVEC: starts the app in headless Chrome and clicks through it with shinytest2.
# Run from the repository root:   Rscript tests/test_ui.R
# Needs shinytest2 and Google Chrome. Takes a minute or two. Exit status is 1 if any check fails.
Sys.setenv(NOT_CRAN = "true")      # shinytest2 skips itself unless it knows this is not a CRAN check
suppressMessages(library(shinytest2))

results <- list()
check <- function(what, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) { cat("     error:", conditionMessage(e), "\n"); FALSE })
  results[[length(results) + 1]] <<- ok
  cat(sprintf("%-4s %s\n", if (ok) "PASS" else "FAIL", what))
}
section <- function(x) cat("\n==", x, "==\n")

new_app <- function() AppDriver$new("app", timeout = 30000, load_timeout = 60000, width = 1400, height = 1000, seed = 1)
# Open the app at a web address that carries a link fragment (the app reads it when it connects)
open_link <- function(frag) {
  port <- httpuv::randomPort()
  bg <- callr::r_bg(function(p) shiny::runApp("app", port = p, launch.browser = FALSE, test.mode = TRUE), args = list(port))
  for (i in 1:60) { Sys.sleep(1); if (!is.null(tryCatch(suppressWarnings(readLines(sprintf("http://127.0.0.1:%d", port), n = 1, warn = FALSE)), error = function(e) NULL))) break }
  app <- AppDriver$new(sprintf("http://127.0.0.1:%d/", port), timeout = 30000, load_timeout = 60000, width = 1400, height = 1000)
  app$wait_for_idle(2000)
  # Chrome's driver percent-encodes a fragment given in the address, so hand the app the link the same way its own page script does
  app$run_js(sprintf("var o = {}; new URLSearchParams(%s).forEach(function(v, k) { o[k] = v; }); Shiny.setInputValue('url_settings', o, {priority: 'event'});", jsonlite::toJSON(frag, auto_unbox = TRUE)))
  list(app = app, bg = bg)
}
js <- function(app, code) app$get_js(code)
val <- function(app, id) app$get_value(input = id)
set <- function(app, ...) { app$set_inputs(..., wait_ = FALSE); app$wait_for_idle(800) }
click <- function(app, id) { app$click(id, wait_ = FALSE); app$wait_for_idle(800) }
run_and_wait <- function(app) {
  app$click("run", wait_ = FALSE); Sys.sleep(1)
  app$wait_for_js("document.querySelectorAll('.shiny-progress').length === 0 && document.querySelectorAll('.stat-tile').length >= 1", timeout = 180000)
  app$wait_for_idle(timeout = 120000, duration = 1000)
}
open_tab <- function(app, name) { app$run_js(sprintf("$('.nav-tabs a[data-value=\"%s\"]').click()", name)); app$wait_for_idle(1500) }
has_error <- function(app) isTRUE(js(app, "document.querySelectorAll('.shiny-output-error').length > 0"))
toasts <- function(app) js(app, "Array.from(document.querySelectorAll('.shiny-notification, .pv-toast, .toast')).map(function(e){return e.innerText}).join(' | ')")

app <- new_app()
on.exit(try(app$stop(), silent = TRUE), add = TRUE)

section("1. The page loads")
check("the page has a title and no startup error", nzchar(js(app, "document.title")) && !has_error(app))
check("the main controls are present", all(vapply(c("run", "mort_model", "structure", "n_iter", "auto_trials", "seed", "preset"), function(id)
  isTRUE(js(app, sprintf("!!document.getElementById('%s') || document.getElementsByName('%s').length > 0", id, id))), TRUE)))
check("all nine assumption cards are present", js(app, "document.querySelectorAll('.assump-card').length") == 9)
check("the console has no errors on load", { m <- app$get_logs(); !any(m$location == "browser" & m$level == "error" & !grepl("favicon|font", m$message)) })

section("2. A run with the default settings")
run_and_wait(app); open_tab(app, "Forecast")
check("a forecast is drawn and no output shows an error", !has_error(app) && js(app, "document.querySelectorAll('#forecast_plot img, #forecast_plot canvas').length") >= 1)
check("the summary tiles are shown", js(app, "document.querySelectorAll('.stat-tile').length") >= 2)
check("the summary text mentions the Ct range", grepl("Ct", js(app, "document.getElementById('forecast_summary').innerText")))

section("3. Different models and structures all run")
for (m in c("gompertz", "exponential", "logistic")) for (st in c("synchronous", "stable")) {
  set(app, mort_model = m, structure = st); run_and_wait(app); open_tab(app, "Forecast")
  check(sprintf("%s / %s runs and the forecast shows", m, st), !has_error(app) && nchar(js(app, "document.getElementById('forecast_summary').innerText")) > 20)
  open_tab(app, "Define assumptions")
}

section("4. Trials: values, bad input, and Auto trials")
set(app, mort_model = "gompertz", structure = "synchronous"); open_tab(app, "Define assumptions")
set(app, n_iter = "2,500"); run_and_wait(app); open_tab(app, "Forecast")
check("2,500 trials runs", grepl("2,500", js(app, "document.getElementById('forecast_summary').innerText")) && !has_error(app))
for (bad in c("", "abc", "5", "-100", "99999999")) {
  set(app, n_iter = bad); app$click("run"); app$wait_for_idle(1500)
  check(sprintf("trials = '%s' is refused with a message, no crash", bad), !has_error(app) && nzchar(toasts(app)))
}
open_tab(app, "Define assumptions"); set(app, n_iter = "1,000", auto_trials = TRUE)
check("turning on Auto trials shows the precision field", isTRUE(js(app, "document.getElementById('auto_tol').offsetParent !== null")))
set(app, auto_tol = 5); run_and_wait(app)
check("an auto run finishes without an error", !has_error(app) && js(app, "document.querySelectorAll('.stat-tile').length") >= 2)
for (bad in c(0, 50)) {
  set(app, auto_tol = bad); app$click("run"); app$wait_for_idle(1500)
  check(sprintf("auto precision %s is refused", bad), !has_error(app) && nzchar(toasts(app)))
}
set(app, auto_tol = 5, auto_trials = FALSE)

section("5. Flag as uncertain, and removing the flag")
set(app, mort_model = "gompertz")
js(app, "$('#card_a_bite .assump-toggle').click(); 0"); app$wait_for_idle(800)
set(app, a_bite_dist = "Fixed", a_bite_value = 0.5, a_bite_flag_pct = 20)
card_w <- function() js(app, "document.getElementById('card_a_bite').getBoundingClientRect().right")
check("the flag box fits inside its card", js(app, "(function(){var c=document.getElementById('card_a_bite').getBoundingClientRect(), b=document.getElementById('a_bite_flag').getBoundingClientRect(); return b.right <= c.right + 1 && b.left >= c.left - 1})()"))
click(app, "a_bite_flag")
check("flagging switches to Triangular", identical(val(app, "a_bite_dist"), "Triangular"))
check("flagging sets min, likeliest and max around the value", isTRUE(all.equal(c(val(app, "a_bite_min"), val(app, "a_bite_mode"), val(app, "a_bite_max")), c(0.4, 0.5, 0.6))))
check("the remove link appears", isTRUE(js(app, "!!document.getElementById('a_bite_unflag')")))
run_and_wait(app)
check("a run with the flagged range works", !has_error(app))
click(app, "a_bite_unflag")
check("removing the flag goes back to Fixed at the original value", identical(val(app, "a_bite_dist"), "Fixed") && isTRUE(all.equal(val(app, "a_bite_value"), 0.5)))
check("the remove link disappears", isTRUE(js(app, "!document.getElementById('a_bite_unflag')")))
set(app, a_bite_value = 0); click(app, "a_bite_flag")
check("flagging a value of 0 is refused and nothing changes", identical(val(app, "a_bite_dist"), "Fixed"))
set(app, a_bite_value = 0.5, a_bite_flag_pct = 150); click(app, "a_bite_flag")
check("flagging with 150% is refused and nothing changes", identical(val(app, "a_bite_dist"), "Fixed"))
set(app, a_bite_value = 0.75)

section("6. Every distribution can be chosen on a card without an error")
for (dn in c("Uniform", "Triangular", "PERT", "Beta", "Normal", "Lognormal", "Fixed")) {
  set(app, a_bite_dist = dn)
  check(sprintf("a_bite = %s: the card renders with no error", dn), !has_error(app) && identical(val(app, "a_bite_dist"), dn))
}
set(app, a_bite_dist = "Uniform", a_bite_min = 0.9, a_bite_max = 0.2); app$click("run"); app$wait_for_idle(1500)
check("min above max is refused with a message", !has_error(app) && nzchar(toasts(app)))
set(app, a_bite_dist = "Fixed", a_bite_value = 0.75)

section("7. Presets")
set(app, preset = "fixed"); check("Point values makes every assumption Fixed", all(vapply(c("a_bite", "n_eip", "m_dens", "vec_comp", "mort_a", "mort_b"), function(id) identical(val(app, paste0(id, "_dist")), "Fixed"), TRUE)))
run_and_wait(app); check("a Point values run gives one Ct for every trial", !has_error(app) && js(app, "document.getElementById('forecast_summary').innerText.indexOf('same Ct') >= 0 || document.querySelectorAll('.stat-tile').length >= 1"))
set(app, preset = "lit"); check("Uncertain ranges restores the distributions", !identical(val(app, "n_eip_dist"), "Fixed") && !identical(val(app, "mort_a_dist"), "Fixed"))

section("8. Copy link carries every setting, and opening it restores them")
set(app, mort_model = "gompertz", structure = "synchronous", n_iter = "3,000", seed = 777, auto_trials = TRUE, auto_tol = 4)
js(app, "$('#copy_link').click(); 0"); app$wait_for_idle(800)
link <- js(app, "window.__lastLink")
check("a link was produced", is.character(link) && grepl("#", link, fixed = TRUE))
check("the link includes the auto-trials switch and precision", grepl("auto.trials=TRUE", link, fixed = TRUE) && grepl("auto.tol=4", link, fixed = TRUE))
check("the link includes model, structure, trials and seed", all(vapply(c("model=gompertz", "structure=synchronous", "trials=3000", "seed=777"), grepl, TRUE, x = link, fixed = TRUE)))
frag <- sub("^[^#]*#", "", link)
lk <- open_link(frag); app2 <- lk$app
on.exit(try(lk$bg$kill(), silent = TRUE), add = TRUE)
Sys.sleep(2); app2$wait_for_idle(1500)
check("opening the link restores the model and structure", identical(val(app2, "mort_model"), "gompertz") && identical(val(app2, "structure"), "synchronous"))
check("opening the link restores Auto trials and its precision", isTRUE(val(app2, "auto_trials")) && isTRUE(all.equal(val(app2, "auto_tol"), 4)))
check("opening the link restores the seed", isTRUE(all.equal(val(app2, "seed"), 777)))
app2$stop(); lk$bg$kill()

section("9. Temperature what-if")
set(app, auto_trials = FALSE, n_iter = "1,000", temp_on = TRUE)
for (cv in c("generic", "aedes_dengue", "anopheles_pf")) for (dT in c(-4, 3)) {
  set(app, temp_curve = cv, temp_delta = dT); run_and_wait(app)
  check(sprintf("temperature curve %s, change %+d C runs without an error", cv, dT), !has_error(app))
}
set(app, temp_on = FALSE)

section("10. Linked mortality a and b")
set(app, mort_model = "gompertz", preset = "lit")
check("the a-b link card is shown for varying a and b", isTRUE(js(app, "!!document.getElementById('ab_unlink')")))
click(app, "ab_unlink")
check("Unlink them switches the card to the independent state", isTRUE(js(app, "!!document.getElementById('ab_link')")))
run_and_wait(app); check("a run with a and b unlinked works", !has_error(app))
click(app, "ab_link"); run_and_wait(app)
check("re-linking and running works", !has_error(app) && isTRUE(js(app, "!!document.getElementById('ab_unlink')")))

section("11. Other tabs render after a run")
for (tab in c("sens", "draws", "surv", "twod")) {
  js(app, sprintf("$('.tool-tab[data-panel=%s]').click(); 0", tab)); app$wait_for_idle(1500)
  check(sprintf("the %s panel opens with no output error", tab), !has_error(app))
}

section("12. Reset")
set(app, n_iter = "4,321", seed = 5)
click(app, "reset_open")
check("Reset opens its pop-up", isTRUE(js(app, "!!document.querySelector('.modal.in, .modal.show')")))
click(app, "reset_preset")
check("Reset values to this preset restores the trial count", identical(val(app, "n_iter"), "1,000"))

cat(sprintf("\n%d of %d checks passed.\n", sum(unlist(results)), length(results)))
if (!all(unlist(results))) quit(status = 1)
