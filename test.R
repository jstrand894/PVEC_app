# ---------------------------------------------------------------------------
# PVEC: what to run, and when. Run lines one at a time (Cmd+Enter in RStudio),
# not the whole file.
# Working directory must be the vc_app folder (check with getwd() below).
# ---------------------------------------------------------------------------

# ---- 0. One-time setup (only the first time, or on a new machine) ----------
# install.packages("shiny")
# install.packages("shinylive")   # needed for section 3
getwd()          # should end in /vc_app
list.files()     # should list app, docs, deploy.R ...

# ---- 1. DAY-TO-DAY: live version in your browser (use this while editing) ---
# Opens the app in your browser. With autoreload on, saving app/app.R or
# app/www/pvec.css refreshes the page automatically. If a CSS change doesn't
# show, stop the app (red stop button / Esc) and run these two lines again.
# Stop the app before running anything else in this console.
options(shiny.autoreload = TRUE)
shiny::runApp("app", launch.browser = TRUE)

# ---- 2. Preview the built static site (what GitHub Pages will serve) -------
# Run section 3 first, then this. Slower than section 1, and it does NOT update
# when you edit app.R: rebuild (section 3) and reload to see changes.
# library(shiny)
# httpuv::runStaticServer("docs")

# ---- 3. Rebuild the static site (before publishing / checking the web build) -
# Exports app/ into docs/ as a Shinylive site that runs entirely in the browser.
# shinylive::export("app", "docs")
# file.create("docs/.nojekyll")   # needed once so GitHub Pages serves it correctly

# ---- Not needed now (one-time project setup, already done) -----------------
# dir.create("app")
# file.rename("app.R", "app/app.R")

# ---- Tests (run in the terminal from the vc_app folder, not the R console) --
#   Rscript tests/test_core.R      # model vs the published Styer et al. values, sampling, 2D, auto trials
#   Rscript tests/test_inputs.R    # input robustness: every distribution, edge values, bad input, links, temperature
#   PVEC_SMOKE=1 Rscript tests/test_inputs.R   # same, plus starts the app in the background (needs install.packages("callr"))
#   Rscript tests/test_ui.R        # browser test: opens the app in headless Chrome and clicks through it (about 3 minutes)
# Each prints PASS/FAIL per check and exits with an error if any fail. Run both before publishing.
