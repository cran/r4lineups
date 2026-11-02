# r4lineups

Statistical tools for assessing lineup fairness, confidence‑accuracy analyses, and face similarity in eyewitness identification research.

## Authorship and acknowledgements

Colin Tredoux and Tamsyn Naylor are the key authors of `r4lineups` and were
jointly responsible for the original package's design and development.

Development used multiple dynamically updated frontier models: Anthropic
Claude Sonnet 4 and 5 and Claude Opus 4.6 and 4.8, and OpenAI Codex with
GPT-5-family models including GPT-5.5 and GPT-5.6 Sol. Additional Claude and
Codex sessions were used, but exact session-level model identifiers were not
consistently recorded. These tools assisted with code review and drafting,
debugging, test scaffolding, documentation, package-check remediation, and
manuscript editing. Colin Tredoux and Tamsyn Naylor reviewed, modified, and
validated the AI-assisted outputs, made the statistical and software-design
decisions, and retain full responsibility for accuracy, originality,
licensing, and scholarly claims.

This package implements computational approaches developed by many
researchers. We gratefully acknowledge their foundational work and cite their
publications in the documentation for the relevant functions and vignettes.
Any error in translating a published method into code is an error in this
package, not in the cited work. Please report suspected errors through the
[GitHub issue tracker](https://github.com/CGTZA2/r4lineups/issues).

## What this package includes

- **Lineup bias & proportions**: target‑position proportions, all‑member proportions, bootstrap CIs + distributions
- **Effective size**: E (Tredoux, 1998) and E (Malpass, 1981, adjusted)
- **Functional size & diagnosticity**: functional size, diagnosticity ratios, homogeneity tests
- **ROC / CAC / RAC**: confidence‑accuracy methods for lineup data
- **SDT summary comparisons**: d′/c/β from counts, variance estimates (Miller/Gourevitch/bootstrap), z‑tests
- **mSDT (multi‑item SDT)**: max filler distribution, parameter estimation from rejection rates, joint plots
- **Wixted lineup-memory models**: optional pyWitness-backed Independent Observations, Ensemble, and Integration fitting
- **Full ROC (Smith & Yang)**: full ROC data and plots
- **EIG & PPV range**: information‑gain and PPV‑range analyses
- **Calibration & ANRI**: calibration metrics, ANRI and bootstrap comparisons
- **Bayesian curves & utility**: prior‑posterior curves, expected utility, DPP
- **Winter 2‑HT model**: model fitting and bootstrap CIs
- **Face similarity**: deep‑learning embeddings, target‑foil similarity, pairwise matrices, plots
- **Simulation**: synthetic lineup data and power analysis

## Installation

From CRAN:

```r
install.packages("r4lineups")
```

The current release is also available from r-universe, which is updated sooner
than CRAN:

```r
install.packages("r4lineups",
                 repos = c("https://cgtza2.r-universe.dev",
                           "https://cloud.r-project.org"))
```

Or from the source:

```r
# from GitHub
remotes::install_github("CGTZA2/r4lineups")

# from local source
devtools::install()
```

If you prefer base R:

```r
R CMD INSTALL .
```

### Optional Wixted model engine

The lineup-memory fitting interface uses a separately installed, pinned
pyWitness engine. Installation is explicit and isolated; it is never triggered
when r4lineups loads or during ordinary package checks.

```r
install_pywitness()
check_pywitness_deps(initialize = TRUE)
```

## Core data format (confidence‑based analyses)

Functions like ROC/CAC/RAC/EIG/PPV expect a data frame with:

- `target_present` (logical or 0/1)
- `identification` (e.g., "suspect", "filler", "reject")
- `confidence` (numeric)

See the built‑in example `lineup_example` and vignette tables for concrete layouts.

## Quick starts

### Lineup bias and effective size

```r
library(r4lineups)
data(nortje2012)

lineup_vec <- nortje2012$lineup_1
lineup_table <- table(lineup_vec)

# Target proportion (position 3)
lineup_prop_vec(lineup_vec, target_pos = 3, k = 6)

# Effective size (Tredoux)
esize_T(lineup_table)

# Bootstrap distributions
bias_dist <- lineup_bias_boot_dist(lineup_vec, target_pos = 3, k = 6, R = 1000)
plot_lineup_bias_distribution(bias_dist, target_pos = 3)

esize_dist <- esize_boot_dist(lineup_vec, k = 6, metric = "tredoux", R = 1000)
plot_esize_distribution(esize_dist, metric = "E (Tredoux, 1998)")
```

### ROC / CAC

```r
data(lineup_example)
roc <- make_rocdata(lineup_example, lineup_size = 6)
cac <- make_cac(lineup_example, lineup_size = 6, confidence_bins = c(0, 60, 80, 100))
```

### Full ROC

```r
fullroc <- make_fullroc(lineup_example, conf_bins = c(0, 60, 80, 100), lineup_size = 6)
plot_fullroc(fullroc)
```

### Competing lineup-memory models

Fit one fair simultaneous-lineup procedure or condition at a time. The result
contains R-native parameter, cell, fit, and engine tables and can be saved
without a live Python session.

```r
fits <- fit_lineup_models(
  lineup_example,
  models = c("independent", "ensemble", "integration"),
  lineup_size = 6,
  confidence_bins = c(0, 60, 80, 100)
)
print(fits)
plot(fits, type = "fit")
```

pyWitness minimizes Pearson chi-squared. `loglik_at_estimate` is diagnostic,
not maximized; AIC and BIC are therefore not reported. BEST-Rest and Ensemble
are equivalent after criterion rescaling and must not be ranked as independent
models.

### Face similarity

```r
install_r4lineups_python()
lineup <- lineup_similarity("target.jpg", c("foil1.jpg", "foil2.jpg"))
plot_lineup_similarity(lineup)
```

## Shiny app

The app covers lineup fairness from mock-witness choices (bias, effective and
functional size, diagnosticity and its homogeneity), Bayesian posteriors, Lee
and Penrod's Multi-d′ model, ROC/CAC/full ROC with pAUC comparison, calibration
and decision analyses (ANRI, DPP, expected utility, Bayesian and BREE curves,
RAC), expected information gain and PPV ranges, and face similarity when Python
is set up. Confidence-based data are loaded once, on the Data tab, and any
column can be used to compare two groups. Every number shown comes from an
exported r4lineups function, so results can be reproduced in R. The app needs
the suggested packages `shiny`, `bslib` and `DT`.

Package documentation is available at <https://cgtza2.github.io/r4lineups/>.

```r
library(r4lineups)
run_r4lineups_app()
```

The app also runs entirely in the browser, with no installation, at
<https://cgtza2.github.io/r4lineups/app/>. Data loaded there stay on your
computer. The browser version leaves out face similarity, which needs Python.

## Vignettes

- `Vignette.Rmd` — core lineup fairness measures + ROC basics
- `bayesian_inference.Rmd` — Bayesian posterior summaries for effective size, functional size, diagnosticity, calibration, and SDT comparisons
- `fullroc_analysis.Rmd` — full ROC (Smith & Yang)
- `calibration_decision_analysis.Rmd` — calibration, Bayesian curves, utility, DPP, ANRI
- `information_error_rate_analysis.Rmd` — EIG + PPV‑range
- `winter_2ht_model.Rmd` — Winter 2‑HT model
- `model_comparison.Rmd` — comparing 2‑HT, EIG, and full ROC via `compare_models()`
- `pauc_statistical_comparison.Rmd` — statistical comparison of ROC curves (pAUC)
- `simulation_power_analysis.Rmd` — data simulation and power analysis
- `rac_analysis.Rmd` — response time–accuracy (RAC) analysis
- `face_similarity.Rmd` — face similarity walkthrough (requires optional Python setup; code chunks not evaluated at build time)
- `sdt_glm_analysis.Rmd` — SDT via GLM/GLMM (old/new recognition)
- `sdt_summary_comparisons.Rmd` — SDT summary‑level comparisons from counts
- `msdt_model.Rmd` — multi‑item SDT (mSDT) core functions
- `lineup_memory_models.Rmd` — optional Wixted/pyWitness model-fitting workflow

## Notes

- `notes/` contains working papers and test scripts and is **not** part of the package build.
- `pdfs/` contains local audit sources and is ignored by Git and package builds.
- For face similarity, Python dependencies are managed via `install_r4lineups_python()`.
- For Wixted model fitting, pyWitness is managed separately via `install_pywitness()`.
- Tests can be run with `testthat::test_dir("tests/testthat")`; package checks use standard `R CMD build` and `R CMD check` workflows.
