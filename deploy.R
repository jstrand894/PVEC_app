# Rebuild the published site. Run from the repo root in R:  source("deploy.R")
# Then commit and push app/ and docs/ (GitHub Pages serves docs/).

# 1. Stamp today's date in the app footer and About tab
src <- readLines("app/app.R")
today <- sub(" 0", " ", format(Sys.Date(), "%B %d, %Y"), fixed = TRUE)
src <- sub('^LAST_UPDATED <- ".*"', sprintf('LAST_UPDATED <- "%s"', today), src)
writeLines(src, "app/app.R")

# 2. Export with a proper page title, browser-tab icon and loading message
favicon <- paste0(
  "data:image/svg+xml,",
  utils::URLencode(paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 32 32'>",
    "<rect width='32' height='32' rx='6' fill='#337ab7'/>",
    "<rect x='5' y='18' width='4' height='9' fill='#fff'/><rect x='10' y='11' width='4' height='16' fill='#fff'/>",
    "<rect x='15' y='6' width='4' height='21' fill='#fff'/><rect x='20' y='13' width='4' height='14' fill='#fff'/>",
    "<rect x='25' y='20' width='3' height='7' fill='#fff'/></svg>"), reserved = TRUE))

# The loading message sits behind the app, so it shows while R starts up and is covered
# by the app once it has drawn.
loading <- paste0(
  "<div style='position:fixed;top:38%;left:0;right:0;text-align:center;z-index:0;",
  "font-family:Helvetica,Arial,sans-serif;color:#6c757d'>",
  "<div style='font-size:18px'>Loading the simulator...</div>",
  "<div style='font-size:13px;margin-top:6px'>The first load can take 10 to 30 seconds while R starts in your browser.</div>",
  "</div>")

shinylive::export(
  "app", "docs",
  template_params = list(
    title = "Probabilistic vectorial capacity simulator",
    include_in_head = paste0(sprintf("<link rel='icon' href=\"%s\">", favicon),
                             "<style>#root { position: relative; z-index: 1; }</style>"),
    include_before_body = loading))

file.create("docs/.nojekyll")
message("Done. Commit and push app/ and docs/ to publish.")
