# r4lineups vignette map

This directory is organized as the long-form documentation companion to the
JOSS paper. The paper should describe the software and statement of need; these
vignettes carry the methodological detail, worked examples, and reporting
guidance.

| Vignette | Purpose | Data shape | Main functions |
| --- | --- | --- | --- |
| `Vignette.Rmd` | Getting started and core mock-witness fairness measures | Numeric lineup-choice vectors and tables | `lineup_prop_vec()`, `allprop()`, `esize_T()`, `func_size()`, `diag_ratio_T()` |
| `bayesian_inference.Rmd` | Bayesian posterior summaries for fairness, diagnosticity, calibration, and SDT comparisons | Mock-witness vectors/tables; confidence data; SDT summary counts | `esize_T_bayes()`, `func_size_bayes()`, `diag_ratio_T_bayes()`, `calibration_bayes()`, `sdt_compare()` |
| `sdt_summary_comparisons.Rmd` | Summary-level SDT comparisons from counts | 2 x 2 count summaries | `sdt_summary_from_counts()`, `sdt_summary_variance()`, `compare_sdt_summary()` |
| `sdt_glm_analysis.Rmd` | Trial-level SDT via GLM/GLMM | Old/new recognition data frames | `fit_sdt_glm()`, `fit_sdt_glmm()`, `extract_sdt_metrics()` |
| `fullroc_analysis.Rmd` | Full ROC analyses using suspect, filler, and reject outcomes | `target_present`, `identification`, `confidence` | `make_fullroc_data()`, `make_fullroc()`, `plot_fullroc()` |
| `rac_analysis.Rmd` | Response time-accuracy characteristic analyses | Confidence data with response times | `make_racdata()`, `make_rac()`, `make_rac_gg()` |
| `calibration_decision_analysis.Rmd` | Calibration, ANRI, Bayesian curves, utility, and DPP | Confidence-based lineup data | `make_calibration()`, `compute_anri()`, `make_bayes_curves()`, `make_utility_curves()`, `make_dpp()` |
| `information_error_rate_analysis.Rmd` | Expected information gain and PPV/error-rate ranges | Confidence-based lineup data | `make_eig()`, `make_ppv_range()`, `plot_ppv_range()` |
| `winter_2ht_model.Rmd` | Winter two-high-threshold model | Aggregate TP/TA response counts or standardized data | `fit_winter_2ht()`, `boot_winter_2ht()`, `plot_2ht_parameters()` |
| `msdt_model.Rmd` | Multi-item SDT helper functions | Rejection rates and model parameters | `estimate_msdt_params()`, `plot_msdt_joint()` |
| `multid_model.Rmd` | Lee & Penrod Multi-d′ model | TP/TA suspect, filler and rejection data or counts | `make_multid()`, `compare_multid()` |
| `lineup_memory_models.Rmd` | Wixted lineup-memory model fitting through the optional pinned pyWitness engine | Fair simultaneous-lineup trial data with ordered confidence | `fit_lineup_models()`, `check_pywitness_deps()`, `install_pywitness()` |
| `model_comparison.Rmd` | Cross-model summaries for the same data | Standardized confidence-based lineup data | `compare_models()`, `format_comparison_table()` |
| `pauc_statistical_comparison.Rmd` | Bootstrap pAUC comparisons | Two standardized confidence-based datasets | `compare_pauc()` |
| `simulation_power_analysis.Rmd` | Simulated lineup data and power analysis | Simulation parameters | `simulate_lineup_data()`, `simulate_power_analysis()` |
| `face_similarity.Rmd` | Optional face-embedding similarity workflow | Local face image files plus optional Python dependencies | `install_r4lineups_python()`, `lineup_similarity()`, `plot_lineup_similarity()` |

## Execution policy

Vignettes that rely on optional Python dependencies, local images, remote
downloads, or long simulations keep those chunks non-evaluated during automated
package checks. Runnable examples use built-in package data wherever possible so
that package checks remain deterministic and network-free.

## Suggested reader path

1. Start with `Vignette.Rmd` for core fairness measures and data conventions.
2. Use `bayesian_inference.Rmd` when posterior intervals and direct probability
   statements are preferred.
3. Use the ROC, calibration, EIG/PPV, and model-comparison vignettes for
   confidence-based target-present/target-absent studies.
4. Use the model-specific vignettes only when the data and research question
   justify those assumptions.
5. Treat face similarity as an optional audit aid that complements, but does not
   replace, mock-witness or outcome-based validation.
