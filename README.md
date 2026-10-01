# Probabilistic Vectorial Capacity Simulator

An interactive tool that propagates uncertainty in transmission and mosquito mortality
parameters through an age-specific vectorial capacity model. Each assumption can be fixed
or given a probability distribution. The simulation draws many parameter sets at random and
reports the resulting distribution of total vectorial capacity (Ct), together with a
sensitivity ranking of the inputs.

**Use it in your browser:** https://jstrand894.github.io/vc_app/

The app runs entirely in your browser (R compiled to WebAssembly through
[Shinylive](https://posit-dev.github.io/r-shinylive/)). There is no server, and nothing you
enter leaves your computer. The first load can take 10 to 30 seconds while R starts.

## What you can do

- Choose a mortality model (logistic, Gompertz or exponential) and a population age
  structure (stable age distribution or synchronous emergence).
- Set each assumption to a fixed value or a distribution (uniform, triangular, PERT, beta,
  normal or lognormal), with a live preview of its shape.
- Start from literature-based distributions or from fixed point estimates.
- Run 500 to 10,000 trials, compare any of your last ten runs on one plot, and read off
  ranges, thresholds and the simulation's own precision.
- Download plots (PNG), per-trial results (CSV), a self-contained HTML report, or your
  settings (CSV) to reload later. Tables have a copy button that pastes into a spreadsheet.
- Share a setup with **Share settings**: it copies a link that carries every setting after
  the `#` in the address, so opening it restores them (it never leaves your browser).
- See which assumptions you have changed from the preset (an "edited" badge, with a reset
  link on each card), and switch the Sensitivity plot between share of variance and raw
  rank correlation.
- Hide the settings panel to give plots the full width. Printing a tab prints just its
  content.

## Model and sources

Vectorial capacity follows the classical formulation of Macdonald (1957) and Garrett-Jones
(1964), extended to age-dependent mortality and extrinsic incubation following Styer et al.
(2007). The **Model check** tab reruns the deterministic model with the fixed values from
Styer et al. and compares the result with their published estimates.

- Macdonald G (1957) *The Epidemiology and Control of Malaria.* Oxford University Press.
- Garrett-Jones C (1964) Nature 204:1173-1175. https://doi.org/10.1038/2041173a0
- Styer LM, Carey JR, Wang J-L, Scott TW (2007) Am J Trop Med Hyg 76:111-117.
  https://doi.org/10.4269/ajtmh.2007.76.111

## Run it locally

You need R with the `shiny` package.

```r
shiny::runApp("app")
```

## Publish a new version

The published site is the contents of `docs/`, which GitHub Pages serves from the `main`
branch. After changing `app/app.R`, rebuild it:

```r
source("deploy.R")
```

This exports the app with Shinylive, updates the "last updated" date and writes the result
to `docs/`. Commit and push `app/` and `docs/`. A GitHub Actions workflow
(`.github/workflows/rebuild-site.yml`) can also do this automatically when `app/` changes.

## Repository layout

| Path | What it is |
|---|---|
| `app/app.R` | The whole app: model, interface and server |
| `app/www/` | Images used by the app |
| `docs/` | The exported site that GitHub Pages serves (generated, do not edit) |
| `deploy.R` | Rebuilds `docs/` |
| `pics/` | Original photo used on the About tab |
| `CITATION.cff` | Citation metadata for this software |

## Settings file format

**Save settings** writes a two-column CSV (`setting`, `value`) listing the mortality model,
age structure, growth rate, first-bite age, trials, seed, and the distribution and
parameters of every assumption. Lines starting with `#` are comments. **Load settings**
reads the same format back.

## Citation

See `CITATION.cff` (GitHub's "Cite this repository" button reads it). If this tool
accompanies a paper, please cite the paper as well.

## Author and licence

Jackson R. Strand, Montana State University. https://www.jackson-strand.com

Released under the MIT licence (see `LICENSE`).
