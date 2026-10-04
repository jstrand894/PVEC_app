# PVEC v1.2: Probabilistic Vectorial Capacity Simulator

PVEC propagates uncertainty in transmission and mosquito mortality parameters through an age-specific
vectorial capacity model. Each assumption can be fixed or given a probability distribution. The simulation
draws many parameter sets and reports the distribution of total vectorial capacity (Ct) with a sensitivity
ranking of the inputs. It runs entirely in the browser (R compiled to WebAssembly with Shinylive), so
nothing a user enters leaves their computer.

- **Live app:** https://jstrand894.github.io/PVEC_app/
- **Source:** https://github.com/jstrand894/PVEC_app
- **Licence:** MIT

## Scientific scope

- Model: Macdonald (1957) and Garrett-Jones (1964) vectorial capacity, extended to age-dependent mortality
  (logistic, Gompertz or exponential) and extrinsic incubation following Styer et al. (2007).
- Population age structure: stable age distribution or synchronous emergence.
- Distributions: uniform, triangular, PERT, beta, normal, lognormal, or fixed. Rank correlations between
  assumptions, or uploaded joint draws (e.g. posterior samples).
- Sensitivity: PRCC with 95% intervals, rank correlation, share of squared correlation, and share of variance.
- Optional temperature what-if (Liu-Helmersson et al. 2014 for *Aedes aegypti* and dengue virus; Martens 1997
  and the 111 degree-day rule for *Anopheles* and *Plasmodium falciparum*).
- Optional conversion to R0, which needs extra inputs supplied by the user.

## Validation

- The **Model check** tab reruns the deterministic model with the fixed values from Styer et al. (2007)
  and compares the result with their published estimates.
- `tests/test_core.R` checks the model against the classical closed form and the Styer et al. values, seed
  reproducibility, achieved correlations, PRCC against the textbook formula, recovery of known effects by the
  variance share, interval fitting, and that the equations on the About tab reproduce the model.
  Run with `Rscript tests/test_core.R`.

## Changes in this version

- Charts draw in after a run and when a results tab is opened.
- Results wait for the Run bar to finish, with a loading skeleton.
- Each assumption card shows how strongly it moved Ct in the most recent run.
- Past runs show how their median compares with the latest, with a line tracking the median across runs.
- Reorganised results tabs: run details in the Quick summary bar, Download in the chart corner, and
  "How to read this chart" below the chart.
- About tab additions: animations, pop-up definitions on equation symbols, a short FAQ, distribution pictures in
  the Presets table, more citation formats (APA, MLA, Chicago, BibTeX, RIS), and an **Intended use** section
  stating what the tool is and is not meant for.

## Reproducibility notes

- A run is reproduced by its settings, seed and app version. The HTML report and the settings CSV record these.
- Seeded results can differ between R versions. This archived release is the version to cite and to pin.
- Settings files from earlier versions still load.

## Known limitations

Ct is an index of transmission potential, not a forecast. Default ranges come from the literature, not local
field data. The model has no seasonality, space or vector control, and propagates parametric uncertainty only
(structural uncertainty from the choice of mortality form or age structure is not included in the Ct interval).
See the Limitations section of the About tab.

## How to cite

Strand, J. R. (2026). *PVEC: Probabilistic Vectorial Capacity Simulator* (Version 1.2) [Computer software].
https://jstrand894.github.io/PVEC_app/

Once Zenodo assigns a DOI, add it to the citation and to `CITATION.cff`. Also cite the Styer et al. (2007)
paper for the model.
