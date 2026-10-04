<p>
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="app/www/pvec_logo_dark.svg">
    <img src="app/www/pvec_logo.svg" alt="PVEC" height="56" align="middle">
  </picture>
  <img src="app/www/pvec_mosquito.svg" alt="" height="52" align="middle">
</p>

# Probabilistic Vectorial Capacity Simulator

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23136677.svg)](https://doi.org/10.5281/zenodo.23136677)

PVEC (Probabilistic VECtorial capacity) is an interactive tool that propagates uncertainty in transmission and mosquito mortality
parameters through an age-specific vectorial capacity model. Each assumption can be fixed
or given a probability distribution. The simulation draws many parameter sets at random and
reports the resulting distribution of total vectorial capacity (Ct), together with a
sensitivity ranking of the inputs.

**Use it in your browser:** https://jstrand894.github.io/PVEC_app/

The app runs entirely in your browser (R compiled to WebAssembly through
[Shinylive](https://posit-dev.github.io/r-shinylive/)). There is no server, and nothing you
enter leaves your computer. The first load can take 10 to 30 seconds while R starts.

## What's new in version 1.2

- **Charts draw in.** After a run, and each time you open a results tab, bars rise, lines sweep across and the axes stay put.
- **Results wait for the Run bar.** A loading skeleton covers the results until the bar finishes, including when the page first opens.
- **Last run effect on Ct.** Each assumption card shows how strongly it moved Ct in your most recent run, labelled with the run's name, and fades when you edit it.
- **Past runs at a glance.** Each earlier run shows how its median compares with the latest, and a small line tracks the median across runs.
- **Cleaner results tabs.** The run details share the Quick summary bar, Download sits in the chart corner, "How to read this chart" moves below the chart, and hovering a bar fills a fixed box instead of covering the chart.
- **New on the About tab.** Animations (a cohort ageing, two ways to age a population, the incubation race), definitions that pop up on the equation symbols, a short FAQ, small distribution pictures in the Presets table, and more ways to copy the citation (APA, MLA, Chicago, BibTeX, RIS).

## What you can do

**Set up a run**
- Choose a mortality model (logistic, Gompertz or exponential) and a population age structure (stable age
  distribution or synchronous emergence). Both are button rows, with a "?" explaining each choice.
- Set each assumption to a fixed value or a distribution (uniform, triangular, PERT, beta, normal or
  lognormal), with a live preview of its shape. Assumptions are compact cards that open in place, and a jump bar
  moves between sections.
- Start from literature-based distributions or from fixed point estimates, and see which cards you have changed
  from the preset (with a reset link on each).
- Record where each assumption comes from in its **Source** box, and use **Fit from a reported range** to turn a
  published 95% interval (and mean, if given) into the parameters of a distribution.
- Link assumptions that move together with a rank correlation, or upload joint parameter draws (for example
  posterior samples from a fitted model). A hint suggests linking mortality *a* and *b*, which are usually
  estimated from the same data.
- Run 500 to 50,000 trials (type any number, or step through the usual sizes with the arrows) with a seed.
- Try a **temperature what-if**: one slider shifts the incubation period, the initial mortality hazard and the
  biting rate of every trial together. Use published curves for *Aedes aegypti* and dengue virus (Liu-Helmersson
  et al. 2014) or for *Anopheles* and *Plasmodium falciparum* (Martens 1997 mortality and the 111 degree-day
  parasite development rule, with the biting rate left unchanged), or set your own percent change per degree.

**Read the results** (each tab opens with a short, automatically generated *Quick summary*; click it to read it)
- **Forecast:** the distribution of Ct with its median, range and precision; click the chart to place a threshold;
  a certainty range and a bin control; a look at the **most extreme trials** and what puts them in the tail;
  an optional conversion to **R&#8320;**; the same draws run through the **other age structure**; and a comparison of any
  past run with the latest one, with the settings that differ listed side by side.
- **Sensitivity:** PRCC, rank correlation, share of squared correlation, or **share of variance** (the part of the
  spread in Ct one assumption explains on its own). Click a driver or a bar to see Ct plotted against that assumption.
- **Assumption draws:** every assumption's drawn values, filtered, sorted by effect on Ct, and enlarged on click.
- **Survival curves:** survivorship and hazard for up to 100 trials (hover a line for that trial's values), with a
  choice of how many lines and ages to show, and the spread of median lifespan across trials.
- **Model check:** the deterministic model against the values in Styer et al., marked within 1%, within 5% or further,
  plus the paper comparison table and figure.
- **About:** a one-minute summary, how to cite, the model equations, methods notes and sources linked to the
  assumptions they support.

**Keep and share your work**
- Every run is kept in **Past runs**: the latest is always shown and earlier ones open from an arrow. Click a run to
  reload its settings, use the play button to reload and run them in one step, the pencil to rename it, the compare
  icon to compare it with another run, or the cross to remove it. Tags show which run the results come from and which
  run matches your current settings. The HTML report includes each tab's quick summary and the variance-share chart.
- Download plots (PNG), per-trial results (CSV), a self-contained HTML report, or your settings (CSV) to reload later.
  Tables have a copy button that pastes into a spreadsheet.
- **Share settings** copies a link that carries every setting after the `#` in the address, so opening it restores
  them. It never leaves your browser.
- **Reset** can set every value back to the preset, or **Start over** to return to how the page looks on a first visit.
- A one-minute guided tour is available from the Getting started box. The whole side panel scrolls, a light/dark
  theme toggle is in the header, and the settings panel can be hidden to give plots the full width. Printing a
  tab prints just its content.

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

## Tests

```bash
Rscript tests/test_core.R
```

Checks the age-specific model against the classical closed form and the Styer et al. values,
that the same seed reproduces a run, that requested correlations are achieved without
changing any assumption's own distribution, that uploaded draws are used as whole rows,
that PRCC matches the textbook formula and the variance share recovers known effects, that
fitting a reported interval returns the same interval, that the tail check finds a planted
driver, that the other age structure and the temperature what-if behave as described, that
two saved runs list exactly the settings that differ, and that the equations written on the
About tab, implemented literally, reproduce the model. The GitHub workflow runs them before
rebuilding the site.

## Repository layout

| Path | What it is |
|---|---|
| `app/app.R` | The whole app: model, interface and server |
| `app/www/` | The page script (`pvec.js`), styles (`pvec.css`), and images: the PVEC logo (outlined, so it does not need the Futura font), the purple mosquito used as the browser-tab icon, and the About photo |
| `docs/` | The exported site that GitHub Pages serves (generated, do not edit) |
| `deploy.R` | Rebuilds `docs/` |
| `tests/test_core.R` | Model and sampling checks |
| `pics/` | Original photo and the original editable logo (`PVEC_logo.svg`, uses the Futura font) |
| `CITATION.cff` | Citation metadata for this software |

## Settings file format

**Save inputs** writes a two-column CSV (`setting`, `value`) listing the mortality model,
age structure, trials, seed, the temperature what-if (`temp` and the three per-degree changes),
any rank correlations, and the distribution and parameters of every assumption (including the
growth rate and first-bite age). Files saved before the temperature control existed still load. Lines starting with `#` are comments. **Upload inputs**
reads the same format back.

## Citation

See `CITATION.cff` (GitHub's "Cite this repository" button reads it). The archived release is on Zenodo: https://doi.org/10.5281/zenodo.23136677. If this tool
accompanies a paper, please cite the paper as well.

## Author and licence

Jackson R. Strand, Montana State University. https://www.jackson-strand.com

Released under the MIT licence (see `LICENSE`).
