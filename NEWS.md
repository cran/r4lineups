# r4lineups 2.3.0 (2026-10-29)

## Breaking changes

* `make_dpp()` now computes the deviation from perfect performance as
  Smith, Lampinen, Wells, Smalarz and Mackovichova (2019) define it: DPP(c) =
  1 - Culprit(c) + Innocent(c) at each ROC operating point, averaged over the
  operating points (their Equation 2). The previous statistic,
  1 - AUC(observed) / AUC(perfect) over the observed false-ID range, was a
  different quantity under the same name, and it cited an unrelated paper. On
  a single operating point with culprit rate .8 and innocent-suspect rate .1,
  the old function returned .6; the published measure is .3. The new version
  reproduces Smith et al.'s Table 4 (.864 and .816). The return value now has
  `dpp`, `dpp_by_point` and `best_point` in place of `auc_observed`,
  `auc_perfect`, `auc_gap`, `perfect_roc` and `max_fa`. Optional `alpha` and
  `beta` arguments give their weighted Equation 3.
* `compare_dpp()` now bootstraps the difference in average DPP (stratified by
  target presence within each procedure) and returns its standard error and a
  percentile interval, with a print method. `plot_dpp()` plots DPP at each
  operating point; `plot_dpp_comparison()` overlays two procedures and loses
  its `layout` argument. `plot_dpp()` arguments are now `show_mean`,
  `show_chance` and `show_dpp`.

## New features

* Every analysis that uses target-absent lineups gains an `innocent_suspect`
  argument: `"designated"`, `"estimated"` (filler IDs / lineup size), or
  `"auto"` (the default). The app's Data tab has a matching control. See the
  bug fix below.
* `make_cacdata()`, `make_cac()`, `ppv_by_confidence()`,
  `ppv_range_by_confidence()` and `make_ppv_range()` gain `base_rate`.
  Count-based accuracy, the Mickes (2015) definition, stays the default, but
  it equals the probability of guilt only when the target-present and
  target-absent samples are the same size: with HR = .6 and FAR = .1 it gives
  .857, .923 or .667 for TP:TA splits of 100:100, 200:100 and 100:300. These
  functions now say so in a message when the arms differ. `base_rate = 0.5`
  computes accuracy from the hit and false-ID rates instead, which gives .857
  for all three; other values apply Bayes' rule at that prior. The
  rate-based CAC standard error is a delta-method approximation. Existing
  results are unchanged.

## Bug fixes

* `fit_max_sdt()` could report non-convergence at a correctly found minimum.
  It chose the best of its L-BFGS-B starts by value and kept that start's
  convergence code, so a start that stopped with a line-search code at a flat
  minimum was reported as not converged even when the Nelder-Mead polish
  converged to the same value. This happened with R 4.6.2 on Linux, where
  `compare_max_sdt()` then refused to compare two fits to identical
  conditions. A polish that converges without being worse than the best
  start is now the reported solution, and `compare_max_sdt()` gives the
  convergence codes when it refuses. Estimates are unchanged.

* `make_rocdata()` and `make_utility_curves()` took a confidence criterion
  from every response, rejections included. The extra criteria only
  duplicated existing operating points, so the ROC path and partial AUC were
  unaffected, but averages over points were reweighted by how rejection
  confidence happened to be coded. Changing only the rejection confidences
  (from 0 to values between 51 and 99) moved average DPP from .417 to .647
  and equal-variance z-ROC d' from 1.879 to 1.398. The criteria are now the
  confidence levels of the responses that can move an operating point:
  target-present suspect IDs, plus target-absent suspect IDs with a
  designated suspect or filler IDs without one. Both codings now give DPP
  .475 and d' 1.757. Results change for data in which rejections or filler
  IDs carry confidence levels that no counted ID uses; `lineup_example` is
  unaffected. `make_rocdata()`, `make_dpp()` and `make_utility_curves()` gain
  `thresholds` to set the criteria, and `make_rocdata()` and `make_dpp()`
  return the criteria used. `compare_dpp()` and the `fit_sdt_roc()`
  bootstrap reuse the full-sample criteria in every replicate, so that all
  replicates estimate the same average.

* Target presence coded 0/1, which the validator accepts, broke the
  bootstraps in `compare_pauc()` (an error) and `fit_sdt_roc()`, where every
  replicate failed silently and the intervals were `NA` with
  `n_successful = 0`. Both now resample 0/1 and logical coding identically.
  `fit_sdt_roc()` also warns when bootstrap samples fail, giving the number
  and the first error.

* `func_size_report()` bootstrapped functional size, 1/p, and `boot.ci()`
  silently dropped the resamples with no target choices, where it is
  infinite. With one target choice among 30 mock witnesses, 353 of 1,000
  resamples were dropped and the percentile interval was [10, 30]. The
  bootstrap now resamples p and inverts its limits, which gives the same
  percentile and BCa intervals when every resample is finite and an
  unbounded upper limit when it is not ([10, Inf] here). The result gains an
  exact interval (inverted Clopper-Pearson, [5.81, 1185.44] here) and
  `n_infinite`, the number of resamples without target choices. The app's
  Bias & size tab shows both, and displays infinite limits instead of
  leaving them blank.

* `standardize_lineup_data()` kept only the columns it maps, so it dropped
  `suspect_pos` and `lineup_member`. A designated suspect who was never chosen
  then fell back to the filler estimate after standardization (false-ID rate
  .10 instead of 0), and effective-size-corrected PPV lost its member choices
  (.833 instead of .50 when target-absent choices fall on one member). Every
  column it does not map is now carried through unchanged.

* `compare_multid()` differenced the two bootstrap matrices row by row. The
  conditions are independent samples, but fits made with the same seed have
  correlated draws, so the standard error collapsed. Comparing counts
  (60, 25, 15, 10, 30, 60) with (60, 27, 13, 10, 30, 60), both fitted with
  seed 1, gave d'(GFp) a standard error of .014 and p < .0001; the correct
  values are .347 and .86. Identical inputs gave a zero-width interval. The
  standard error is now the square root of the sum of the two bootstrap
  variances, and the interval comes from randomly re-paired draws.
  `compare_multid()` gains `seed`.

* `get_face_landmarks()` failed with every current MediaPipe release. The
  image was read into R and passed back as an int32 array, which OpenCV and
  MediaPipe reject, and the function used `mediapipe.solutions.face_mesh`,
  which current MediaPipe no longer has. The image now stays a uint8 numpy
  array, and when `solutions` is absent the MediaPipe Tasks `FaceLandmarker`
  is used. Its model is downloaded once to
  `tools::R_user_dir("r4lineups", "cache")`. Both routes return the 478-point
  mesh (468 face points plus 10 iris points); the documentation said 468.
  `check_python_deps()` now records in a `"face_mesh"` attribute whether
  landmarks can actually be extracted, not only whether MediaPipe imports.
  An opt-in test (`R4LINEUPS_RUN_FACE=true`) exercises the whole path.

* In the app's Bias & size tab, ticking "Small-sample corrected E'" changed
  the reported estimate but left its intervals on the plug-in estimator, so the
  value box could show a corrected E' with a plug-in interval that did not
  contain it. `esize_boot_dist()` gains `unbiased = FALSE`, which bootstraps
  the corrected estimator (dropping, with a warning, resamples where it is
  unbounded). The app now bootstraps the corrected E' when it reports it, and
  lists the plug-in estimate with its own normal-theory and bootstrap
  intervals on separately labelled rows.

* Target-absent data with a designated innocent suspect who was never chosen
  were treated as having no designated suspect, so the false-ID rate was
  estimated from filler IDs instead of being zero. With 100 target-absent
  lineups, 60 filler IDs and no suspect IDs, the ROC reached a false-ID rate
  of .10 instead of 0. `"auto"` now treats the lineups as designated when any
  target-absent row is a suspect ID *or* the data carry a non-missing
  `suspect_pos` column (as `simulate_lineup_data()` output and
  `lineup_example` do). Otherwise it falls back to the filler estimate with a
  message. Bootstraps (`compare_pauc()`, `fit_sdt_roc()`, `bootstrap_anri()`,
  `compare_anri()`), `make_calibration_by_condition()`,
  `make_utility_difference()`, `ppv_range_by_confidence()` and the
  `make_multid()` confidence breakdown now resolve the design once on the full
  data, so a resample that happens to contain no suspect IDs no longer switches
  estimator.

* `ppv_by_confidence()` dropped every confidence bin that contained no
  target-absent trials, and with it that bin's guilty-suspect IDs, so both the
  bin and the overall PPV were wrong. With 90 guilty IDs at confidence 100 and
  all target-absent responses at 50, the overall PPV was .50 instead of .909.
  Such bins are now kept, with no innocent-suspect IDs (PPV 1) and an `NA`
  error rate and effective size. Only bins empty in both arms are dropped.
  `make_ppv_range()` and its plots inherit the fix.

* `boot_winter_2ht(method = "bca")` failed with a `dimnames` error, because
  the interval matrix was transposed into the wrong shape, and even before
  that its acceleration was fixed at 0, so the "BCa" interval was only
  bias-corrected. The acceleration is now the jackknife estimate (one refit
  per non-empty response cell, weighted by its count), the bias correction
  handles ties and stays finite, and both methods return a parameters x
  (Lower, Upper) matrix.

* The posterior means reported by `func_size_bayes()`, `diag_ratio_T_bayes()`
  and `diag_ratio_T_bayes_compare()` were averages of reciprocal draws. When
  the relevant Beta posterior has shape a <= 1 (for example, no suspect
  choices under the Jeffreys prior), the true mean is infinite and the average
  never settled: it was about 11 million, 3 million and 36 million for 10^3,
  10^4 and 10^5 draws of the same functional size. The means are now computed
  exactly, `(a + b - 1) / (a - 1)` for `1/p`, and are `Inf` when they do not
  exist. Print methods say so; a difference of two infinite means is `NA`.
  Medians, intervals and threshold probabilities are unchanged.

* `compare_pauc()` overstated partial AUC whenever the ROC had a vertical
  segment, that is, when a confidence level added correct IDs but no false IDs.
  Its integrator kept only the upper endpoint of each such segment, so the next
  sloping segment started from too high a correct-ID rate. A curve through
  (0, 0), (0, .5), (.1, .5) and (.1, 1) has area .05; `compare_pauc()` reported
  .075 while `make_rocdata()` reported .05. Both now use one internal
  integrator that keeps every endpoint and interpolates within the segment that
  crosses the cutoff, and their results agree. The `compare_pauc()` bootstrap
  and `simulate_power_analysis()` use the same integrator.

* `make_utility_curves()` stored the reject-all strategy at a criterion of
  `min(confidence) - 1`. Read as "act on suspect IDs with confidence at or
  above the criterion", that value accepts every identification, the opposite
  of the strategy. With 16/20 target-present and 2/20 target-absent suspect
  IDs at confidence 80 and a base rate of .01, the optimum was reject-all
  (expected utility .49) but was reported as criterion 79. Reject-all now sits
  at `max(confidence) + 1`, every row of `utility_data` and `max_utility`
  carries a `strategy` (`"threshold"` or `"reject_all"`), and the plot labels
  the reject-all point. Expected utilities are unchanged.

* `esize_T_bayes()` now warns when `threshold` is at or above the number of
  lineup members. Because E' cannot exceed k and equals it only at exact
  uniformity, P(E' < k | data) is 1 for any data, so such a threshold carries
  no information.

* `simulate_power_analysis()` returns an `n_total` column. A sample size of
  n simulates n target-present and n target-absent witnesses in each of the
  two conditions, 4n in all; the documentation and plot axis now say so.

## Documentation

* A third independent review of the vignettes (30 October 2026) found
  interpretations that the code or its output contradicted. Corrected:
  - a single-condition 2-HT fit has zero residual degrees of freedom, not
    one, so it has no goodness-of-fit test (`winter_2ht_model`,
    `model_comparison`);
  - the ordinary bootstrap gives a degenerate [0, 0] interval for a member
    nobody chose, so the main vignette now recommends the exact binomial
    interval near 0 or 1;
  - the plug-in E' is biased downward for near-fair lineups but not always,
    and `unbiased = TRUE` uses the unbiased estimator of the concentration,
    not of E' (`Vignette`, `?esize_T`);
  - the Bayesian vignette no longer offers P(E' < k | data) as a fairness
    benchmark;
  - the power vignette states its sample-size unit, and its 80% crossing
    is computed from the results;
  - the "undesignated" PPV example kept `suspect_pos`, so all three
    corrections gave 1; it now gives .83, .77 and .45;
  - the BREE and prior-posterior explanations describe the plots as drawn;
  - the mSDT target-absent joint density is not symmetric, and the
    rejection-rate inversion has a feasibility bound;
  - unvalidated grade bands are removed from the model-comparison and
    face-similarity vignettes, and the face-similarity examples now match the
    functions' arguments and return values;
  - smaller qualifications in the RAC, pAUC, lineup-model and simulation
    vignettes.
* The vignettes describe the package as it is: notes about earlier versions
  and past bugs are removed (the history is in this file), and wording, reference
  formatting (APA, hanging indent) and several explanations were revised after
  review.
* `plot.model_comparison()` draws its panels with a shared compact theme, so
  titles no longer overrun, and clips the 2-HT Wald intervals to [0, 1]
  instead of dropping bars that cross zero.

## Statistical audit corrections

* Independent-lineup effective-size comparisons now resample samples separately
  and use within-sample BCa influences. The legacy row-statistic help now
  distinguishes paired from independent resampling.
* Numerical utility cutoffs reproduce reject-all and all-ID operating points on
  custom confidence grids. Single-point base-rate comparisons return their
  value without attempting crossover interpolation. ROC origins carry a separate marker; z-ROC retains
  genuine zero-count criteria and reports the requested confidence level and
  the lure/target SD-ratio convention.
* Grouped calibration and ANRI resolve innocent-suspect design within groups.
  Explicit estimated-mode inputs containing TA suspect IDs are rejected.
  Confidence/time bins must cover observations rather than silently omit them.
* Named table conversion retains member labels; one-column matrices retain all
  observations; singleton empirical samples no longer generate unseen choices.
  Fractional/duplicate position labels and invalid lineup sizes are rejected.
* Corrected effective-size estimates retain legitimate infinity and undefined
  N=1 results. Bootstrap distributions preserve infinite draws; plots explain
  the finite subset displayed. Nonfinite generic proportion summaries fail
  explicitly. Application formatting preserves infinity.
* Normal and bootstrap effective-size intervals are explicitly qualified near
  uniformity and at small samples; percentile/BCa intervals do not provide a
  test of exact fairness. Functional-size percentile and sparse-cell SDT Wald
  intervals carry corresponding approximation cautions. Estimated fractional
  Bayesian counts are described as an approximation. Bayesian calibration
  states its conditional Bernoulli model and fixed-arm design limitation.
* mSDT log CDF/density and extreme log-tail quantiles avoid premature underflow.
* Winter 2-HT fitting validates counts, removes the arbitrary 2% rejection gate,
  uses an independently derived feasible inverse solution or constrained multiple starts,
  and avoids regular Hessian SEs on boundaries. TP/TA bootstrap totals remain
  fixed; BCa influences match that design. Unsuccessful attempts are retained.
* The pinned pyWitness engine reverses rejection-cell denominators in its
  configured chi-squared statistic. A packaged per-instance adapter repairs
  that objective, tightens optimization, and retains the returned minimum.
  `control = list(objective = "pywitness")` turns the correction off to
  reproduce pyWitness itself. The installed Python environment is unchanged. Observed-denominator fits
  reject zero observed cells. Engine provenance records the correction.
* Diagnosticity and z-ROC retain failed bootstrap attempts and report failure
  counts; MAX warns about any failed bootstrap fit. ANRI and power outputs
  report failed/attempted counts and conditional inference explicitly.
* Binned RAC mean times now use the same correct/incorrect IDs as accuracy,
  including 1/k weights for estimated TA IDs. Examples and application bins
  include their full response-time range.
* pAUC confidence labels follow the selected level; its SE-standardized
  difference is no longer labelled Cohen d. Log diagnosticity variance,
  prior-dependent EIG ceilings, fairness sensitivity bands, and embedding
  similarity claims are described explicitly. EIG response plots distinguish
  neutral likelihood ratios from evidence in either direction. Vignettes, help and manuscript
  assurance claims are aligned with the statistical QA evidence and its gaps.

# r4lineups 2.2.0 (2026-10-28)

## Breaking changes

* The first argument of `esize_T()`, `esize_m()`, `i_esize_T()`,
  `gen_linevec()`, `lineup_prop_tab()`, `allfoilbias()`, `datacheck3()`,
  `esize_T_boot()`, `esize_T_bayes()` and `esize_T_ci_n()` is renamed from
  `lineup_table` to `lineup_tab`, so that it no longer collides with the new
  `lineup_table()` function (see below). Code that calls these functions
  positionally is unaffected; code that passes the argument by name (e.g.
  `esize_T(lineup_table = x)`) must be updated to `esize_T(lineup_tab = x)`.

* `simulate_power_analysis()` now performs an actual power analysis and its
  arguments have changed accordingly. It takes `d_prime_null` (the comparison
  condition), `n_bootstrap`, `lineup_size`, `seed` and `verbose`; the former
  `stat_function` argument is replaced by `test_function`, which takes two
  dataframes and returns a p-value. The returned columns are now
  `sample_size`, `power`, `power_se`, `mean_effect`, `sd_effect` and `n_valid`.
  See the bug-fix entry below for why this was necessary.

* `lineup_example` has been regenerated and now has 1400 rows rather than 200,
  with `lineup_member` and `suspect_pos` columns. Numeric results computed from
  it will differ from previous versions.

## Bug fixes

* `simulate_power_analysis()` computed power as the proportion of simulations
  in which the pAUC exceeded zero. A partial area under a curve is always
  positive, so that proportion was **always exactly 1**, for every sample size
  and every effect size, and the resulting power curve was a flat line at the
  top of the plot. Power is now the proportion of simulated studies in which a
  test of the difference between two conditions reaches significance, using
  `compare_pauc()` by default. The single test covering this function was
  skipped and additionally asserted a field the function never returned, so the
  fault could not have been caught; it has been replaced with tests that run.

* `make_roc()`, `make_cac()`, `make_rac()`, `make_fullroc()`, `make_calibration()`
  and `compute_dpp()` set `$plot` to `NULL` when called with `show_plot = FALSE`,
  so suppressing display also destroyed the plot object. `result$plot` was then
  `NULL` and nothing could be drawn from it — the cause of several "the plot does
  not show" reports. The plot is now always built, and `show_plot` controls only
  whether it is displayed immediately, which is what its documentation always
  said it did.

* `compare_pauc()` chose its default region of integration as the **larger** of
  the two conditions' maximum false-ID rates. Since neither curve extends past
  its own maximum and no extrapolation is performed, the curve that stopped
  sooner was integrated over a smaller interval than the other, so the two
  "partial areas" covered different regions and were not comparable. In testing,
  this understated a real difference by around 14%. The default is now the
  largest region **both** curves attain, a message reports it when the two
  differ, and an explicit `max_false_id_rate` beyond either curve's reach now
  warns.

* `make_fullroc()` and `make_fullroc_data()` now report that the default
  `order = "diagnosticity"` is optimistically biased. Because the ordering of
  evidence categories is estimated from the same data used to compute the area,
  data containing no signal at all return a mean AUC of roughly 0.65 at 100
  lineups per arm, 0.60 at 200 and 0.55 at 1000, against a true value of 0.5.
  A-priori ordering is unbiased at every sample size tested. The default is
  unchanged, since it is the ordering Smith et al. describe, but a message now
  points to `order = "apriori"` for inferential use.

* `fit_winter_2ht()` failed with `non-finite finite-difference value` when the
  data contained almost no lineup rejections. The detect-absence parameter `dA`
  is identified by rejections, so the likelihood is nearly flat without them. It
  now stops with a message naming the actual problem and the likely cause — a
  decision criterion so liberal that witnesses never reject. `compare_models()`
  had silently omitted the 2-HT model in this situation, so every downstream
  section that referred to it produced no output.

* `format_comparison_table(format = "markdown")` produced a table that rendered
  as a single run-on paragraph. It returns a `knitr_kable` object, which is a
  character vector with one element per line, and the vignette printed it with
  `cat(x)` — joining the lines with spaces. The vignette now uses
  `cat(x, sep = "\n")` and the return value is documented as line-per-element.
  The markdown and latex forms also called `kable()` without
  `row.names = FALSE`, so row names inherited from the named parameter vectors
  ("dP", "dA", ...) appeared as a spurious unlabelled first column.

* `plot_2ht_parameters()` rejected `winter_2ht_boot` objects, although its
  documentation offered confidence intervals "if bootstrapped". It now accepts
  them and draws the bootstrap percentile intervals. For a plain fit it still
  draws normal approximations, but clamped to [0, 1]: all four parameters are
  probabilities, and on `lineup_example` the interval for `b` previously ran to
  -0.023.

* `plot.esize_T_bayes()` packed the credible interval into the plot title using
  an embedded newline, producing a two-line 74-character title laid out as a
  single text grob — the usual cause of a heading running off the top of a
  rendered figure. The interval is now a subtitle. The other five Bayesian plot
  methods were checked and none had the same fault.

* Effective size is no longer referred to eponymously as "Tredoux effective
  size" in documentation, printed output or plot labels; it is now "effective
  size" with the citation (Tredoux, 1998). Affects `esize_T()`, `i_esize_T()`,
  `esize_T_ci_n()` and the `esize_T_bayes()` family.

* `fit_max_sdt()` left bootstrap replicates that failed to converge as `NA`
  rows in `boot_params` without recording how many, so `sd(fit$boot_params[, 1])`
  returned `NA` unexplained. It now reports `boot_n_used` and `boot_n_failed`,
  warns when more than 10% fail, and errors rather than returning meaningless
  intervals if all of them do. Its documentation also gained a "Provenance"
  section crediting the R scripts of A. M. Smith from which the implementation
  follows, and a "Relation to Smith's scripts" section explaining why estimates
  differ: this function uses five mutually exclusive response cells, whereas
  those scripts fit total target-present choices alongside culprit
  identifications, counting the latter twice.

* `install_r4lineups_python()` failed with reticulate's opaque `Suitable Python
  installation for creating a venv not found` when `python_version` named a
  version not present on the machine, without saying how to obtain one. It now
  checks first, reports which versions it did find, and gives the
  `reticulate::install_python()` call needed.

* `Vignette.Rmd` drew random numbers -- simulated ages and every bootstrap --
  without a seed, so its output changed on every build. Seeded once in setup.

* `make_bayes_curves()` now warns when a response category has no observations
  in one arm. The likelihood ratio is then unbounded and the prior-posterior
  curve is pinned flat at 0 or 1, which looks like a plotting fault rather than
  a data problem.

* The member-level branch of the effective-size calculation in
  `ppv_by_confidence()` counted rejections as a lineup position and did not
  tabulate over all positions, so unchosen members were dropped. This branch was
  previously unreachable, as no dataset supplied a `lineup_member` column.

* `plot_lineup_bias_distribution()` and `plot_esize_distribution()` no longer
  draw ragged histograms with structural gaps. A bootstrapped lineup proportion
  is a count over a fixed denominator, so the replicates fall on a lattice with
  spacing `1/n`; binning them at a fixed 30 bins left bins that contained no
  attainable value empty regardless of the data, and made bins that straddled
  two attainable values twice as tall as their neighbours. The bin width is now
  chosen to match the lattice, giving one bar per attainable value. Estimates
  that are effectively continuous still use 30 bins. Both functions gained
  `bins` and `binwidth` arguments for explicit control.
* `plot_fullroc()` labelled the plot `"Ordering method: "` with nothing after it
  when given the output of `make_fullroc()`, because the ordering method is
  recorded at `$order_used` on a `make_fullroc_data()` object but at
  `$summary$order_method` on a `make_fullroc()` object. Both shapes are now
  accepted, and an object carrying neither raises an informative error.
* `plot()` now works on the ROC and CAC objects. `make_rocdata()`,
  `make_cacdata()` and `make_fullroc_data()` returned unclassed lists, so
  `plot(x)` dispatched to `plot.default()` and failed; the package registered
  `print` methods for these objects but no `plot` methods. Added
  `plot.lineup_roc()`, `plot.lineup_cac()`, `plot.lineup_fullroc()` and methods
  for the corresponding `*_data` classes, all reusing the existing
  `make_roc_gg()`, `make_cac_gg()` and `plot_fullroc()` builders.
* `install_r4lineups_python()` pinned no Python version, so reticulate could
  select an interpreter newer than any for which TensorFlow publishes wheels,
  and the install failed partway through with `No matching distribution found
  for tensorflow`. It now checks the interpreter before downloading anything and
  stops with an actionable message, and gained a `python_version` argument for
  building the virtual environment from a supported interpreter.

## New features

* New `make_multid()` implements Lee and Penrod's (2019) Multi-d' model. From
  target-present and target-absent lineup data (trial-level or counts) it
  computes d'(GI), d'(GFp), d'(IFa), d'(FaFp) and d'(RaRp), the TP and TA
  lineup-bias indices, the criterion and the diagnosticity ratio, with optional
  bootstrap intervals and a cumulative breakdown by confidence. Because the
  model uses total filler rates, a fair lineup does not give d'(IFa) = 0, so the
  result also reports the d'(IFa) a perfectly fair lineup would give at the
  observed rejection rate. `compare_multid()` compares two conditions, and
  `plot.multid()` draws the decomposition of d'(GI), the indices, or the
  indices by confidence. The tests reproduce Lee and Penrod's Tables 3, 4 and
  9. See `vignette("multid_model")`.

* `esize_T()` gained an `unbiased` argument. The default (`FALSE`) is the
  plug-in estimator as before, so no existing result changes. `unbiased = TRUE`
  applies the small-sample correction `N(N-1) / sum(o(o-1))`. The plug-in form is
  biased **downward**, by approximately `k(k-1)/(N + k - 1)`: a perfectly fair
  six-person lineup assessed with 50 choices returns about 5.47 rather than 6,
  and with 20 choices about 4.87. The correction removes essentially all of it in
  simulation, at a variance cost that disappears by about 100 choices. The bias,
  the evidence and the choice between the two forms are documented in
  `?esize_T`, with cross-references from `?esize_m` and `?esize_T_ci_n`.

* New `classify_lineup_choices()` derives identification outcomes (suspect ID,
  foil ID, rejection) from the raw record of which lineup member each witness
  chose. Lineup data should record the member chosen, with the collapsed label
  derived from it rather than stored in its place, because foils are not
  interchangeable and a foil nobody ever chooses is what makes a lineup unfair.

* `simulate_lineup_data()` gained a `foil_bias` argument, giving each foil its
  own mean memory strength so that foils differ in plausibility rather than
  being drawn from a common distribution. The default (`NULL`) preserves the
  previous behaviour exactly.

* `simulate_lineup_data()` now returns `lineup_member` (the position chosen, 0
  for a rejection) and `suspect_pos` alongside `identification`. The chosen
  position was already computed internally and discarded.

* `plot.power_analysis()` shows a Monte Carlo interval around the power curve,
  so that apparent dips between adjacent points can be recognised as simulation
  noise, and returns the ggplot object invisibly for further modification.

* `homog_diag()` and `homog_diag_boot()` now report the diagnosticity ratio and
  a confidence interval for each lineup pair, not just the pooled mean and the
  chi-square. The chi-square is Cochran's Q, which measures how far the k ratios
  lie from their pooled value; that is a different question from whether the
  pooled ratio differs from 1, and the two can disagree sharply. Ratios of 6.0,
  1.0 and 0.44 give Q = 45.6 (p < .0001) while pooling to 1.06, because they
  cancel on the log scale — an output that reads as "highly significant, but no
  diagnosticity" unless the per-pair values are visible. Both functions now
  print a note stating whether the ratios are heterogeneous and, when they are,
  that the pooled estimate averages quantities that are not estimating a common
  parameter and should not be reported on its own. The returned list gained a
  `heterogeneous` flag, and `homog_diag_boot()` gained `df` and `p_value`; the
  `parameters` element now carries `d`, `ln_d`, `var_lnd`, `wi`, `lower` and
  `upper` alongside the existing cell counts. Both functions gained a
  `conf.level` argument, which also sets the level for `homog_diag_boot()`'s
  percentile intervals (default 0.95, unchanged from the previous hard-coded
  value).
* Added `lineup_table()`, which tabulates lineup choices across all `k`
  positions so that null foils — members no mock witness chose — appear as
  explicit zeroes rather than being silently dropped by `table()`. Its print
  method names the null foils and marks the nominated suspect. The result
  inherits from `table`, so it can be passed to `esize_m()`, `esize_T()` and the
  other table-taking functions unchanged.

## Shiny app

* The app bundled with `run_r4lineups_app()` has been rebuilt on `bslib`, with
  one module per analysis. Every number it shows now comes from an exported
  r4lineups function; the app's own copy of the Malpass effective-size formula
  is gone.
* The ROC, CAC, full ROC, pAUC, EIG and PPV analyses previously assumed a
  six-person lineup whatever the data. Lineup size is now set on a new Data
  tab, which loads one confidence-based data set (built-in or uploaded) for all
  of these analyses and lets any column define two groups to compare.
* New tabs cover the diagnosticity ratio and its homogeneity test, Lee and
  Penrod's Multi-d' model, and calibration and decision analyses (ANRI, DPP,
  expected utility, Bayesian and BREE curves, RAC). The fairness tab adds the
  binomial test against chance, bootstrap distributions, functional size and
  the corrected E'.
* Bootstrap and posterior analyses now run when *Run analysis* is clicked
  rather than on every keystroke, take a seed, and are computed once, so a
  table's interval and the plotted distribution come from the same draws.
  Messages and warnings from the package are shown beside the results.
* Every table downloads as CSV and every plot as PNG. "Download ROC/CAC data"
  previously wrote an `.rds` file.
* The app no longer counts sessions on disk or fetches download totals from
  CRAN and GitHub at start-up.
* The face similarity tab now includes the Python setup that used to be the
  first tab, and is hidden when `reticulate` is unavailable.
* `run_r4lineups_app()` checks that `shiny`, `bslib` and `DT` are installed.
  `bslib` and `shinytest2` join Suggests; `shinyWidgets` and `jsonlite` leave.

## Output and documentation

* `make_racdata()` labels response-time bins in plain notation, e.g.
  `[2390,4810]` rather than `[2.39e+03,4.81e+03]`.

* ROC and CAC print methods now say `"Plot: use plot(x), or save the object and
  extract x$plot"` rather than the cryptic `"Plot available in $plot"`.
* `print.lineup_roc()` and `print.lineup_fullroc()` now report
  `"Target-present trials (N)"` rather than `"Target-present lineups"`, which
  read as a count of lineups when it is a count of witnesses.
* `make_roc_gg()` now names the ROC variant in the title, so a partial ROC is
  not mistaken for a full ROC, and records the data source in the caption. It
  gained `title` and `data_source` arguments.
* `eff_size_per_foils()` had no `@return` documentation at all. Documented that
  it returns a count of lineup members, so it changes only in whole steps as
  `conf` varies and is flat across ranges of `conf` in which no member's
  interval crosses `1/k`.
* `eff_size_per_foils()` and `lineup_boot_allprop()` both described `conf` as
  "Desired level of alpha"; it is a confidence level. Documented also that
  `lineup_boot_allprop()` applies no multiplicity correction across the `k`
  intervals it returns.
* The main vignette has been substantially revised: corrected a typo
  (`line_df$linep_vec`), a reference to a nonexistent `sample_data` dataset, an
  inverted functional size formula (`D/N` for `N/D`), a misstated proportion
  ("0.14%" for 14%), a misattributed sentence ("He instead proposed" for
  "Tredoux instead proposed"), and inline code spans that reflowed into a
  paragraph instead of forming a code chunk. Added sections on null foils,
  normal-theory versus bootstrap intervals, multiplicity across lineup members,
  functional size exceeding nominal size, the `boot.ci()` warnings, the
  `*_data()`/wrapper distinction in the ROC and CAC functions, and raw data
  format for ROC analysis. The homogeneity section now separates the two
  questions the output answers — whether the k ratios agree with one another
  (chi-square) and whether the average ratio differs from 1 (interval on the
  pooled ratio) — with a worked heterogeneous example in which every ratio is
  diagnostic, followed by a cautionary example in which ratios pointing in
  opposite directions give a very large chi-square while pooling to roughly 1.
  The unsupported version 1
  `face_sim()` section has been retired in favour of a pointer to the face
  similarity vignette, and the vignette closes with a linked index of the other
  vignettes.

# r4lineups 2.1.0 (2026-08-10)

* Added an explicit authorship and acknowledgements statement recognizing
  Colin Tredoux and Tamsyn Naylor as authors of the original package; the use
  of Codex and Claude for code-review, drafting, and debugging support in the
  latest development cycle; and the published work on which the package's
  computational methods rely. Suspected implementation errors should be
  reported through the GitHub issue tracker.
* Relicensed the package under the permissive MIT license and added canonical
  citation metadata for the CRAN and JOSS release.

## Wixted lineup-memory model interface

* Added `install_pywitness()` and `check_pywitness_deps()` for an explicit,
  isolated `r4lineups-pywitness` virtual environment. Installation uses the
  immutable archive for audited pyWitness revision
  `e726dcfc09423d0e0ff7f46c8e3a711040293eba`; package loading and ordinary CRAN
  checks never install or initialize it.
* Added `fit_lineup_models()` for fair simultaneous lineups, with Independent
  Observations, Ensemble, Integration, and optional BEST-Rest fits; equal- and
  unequal-variance paths; zero or estimated shared variance where identified;
  starting values; optimizer controls; and explicit convergence diagnostics.
* Results are durable R-native `lineup_model_comparison` objects with extracted
  parameters, observed and expected response cells, Pearson statistics,
  degrees of freedom, p-values, confidence binning, engine metadata, and
  comparison/fit/ROC plots. No live Python object is retained.
* The audited engine minimizes Pearson chi-squared. The reported multinomial
  log likelihood is evaluated at that estimate rather than maximized, so AIC
  and BIC are deliberately `NA` rather than invalidly treating it as an MLE.
* BEST-Rest and Ensemble are reported as scale-equivalent parameterizations and
  are never counted as independent model-selection evidence. Integration is a
  historical comparator rather than a recommended default.
* Added opt-in, three-platform Python tests for wrapper/direct-engine parity,
  parameter and generating-model recovery, probability partitions,
  BEST-Rest/Ensemble rescaling, equal/unequal variance, zero/estimated shared
  variance, sparse confidence cells, and optimizer failure.

# r4lineups 2.0.0 (2026-08-08)

Major release: a substantial expansion of the package from the 0.1.x
fairness-measure toolkit into a full analysis suite for eyewitness
identification research, plus a package-wide statistical audit.

## CRAN-readiness and statistical corrections

* Corrected the legacy fairness estimators and bootstrap helpers: unchosen
  lineup positions are retained, named count tables are matched by member
  label, Malpass and Tredoux effective sizes use the documented definitions,
  Tredoux bootstrap resampling occurs at the witness level, and bootstrap
  standard errors are the standard deviation of the replicate statistics.
* Corrected diagnosticity pairing, log-ratio variance, inverse-variance
  weighting, homogeneity degrees of freedom, and independent TP/TA resampling.
  `pos_list` now denotes the actual suspect position for each TP/TA pair (or a
  two-element TP/TA position pair). Homogeneity inference requires positive
  suspect-ID cells, as in Tredoux's uncorrected log-risk procedure.
* Confidence-based methods now distinguish observed designated innocent-suspect
  IDs from estimates based on target-absent filler IDs. ROC/CAC/RAC,
  calibration, PPV, utility, full ROC, EIG, and Bayesian curves no longer add
  both quantities. With no designated suspect, full ROC/EIG/Bayesian response
  probabilities split target-absent filler choices into `1 / lineup_size`
  suspect and `(lineup_size - 1) / lineup_size` filler components.
* Full-ROC documentation now identifies empirical diagnosticity ordering as an
  optimistic, sample-dependent ordering. PPV/effective-size, utility
  false-alarm, fractional calibration/ANRI, saturated single-condition 2-HT,
  and method-of-moments mSDT assumptions are stated explicitly.
* Corrected pAUC sorting/interpolation and stratified its bootstrap; corrected
  unequal-variance z-ROC discriminability and raw-data resampling; fixed binary
  SDT-GLM validation and the covariance term in summary-level log-beta
  comparisons.
* Seed-taking simulation and bootstrap functions now restore the caller's RNG
  state. Bootstrap count and confidence arguments are validated and honored,
  optimizer/bootstrap failures are checked, and numerical/vector edge cases
  receive explicit errors.
* Optional Python dependency checks no longer initialize or download a managed
  Python installation. Face-embedding distances reject missing, non-finite,
  unequal-length, or zero-norm vectors as appropriate.
* Added package-wide export-contract and hand-calculation regression tests.
  All 156 exported functions are referenced by the test suite; functions that
  require Python, Shiny, `lme4`, images, or other optional environments have
  explicit dependency/error-path coverage and opt-in smoke-test exemptions.
* Reduced evaluated vignette bootstrap/simulation sizes while retaining
  publication-grade settings in unevaluated examples. Source builds no longer
  include the audit article PDFs.
* Corrected the EIG reference to Starns, Cohen, and Rotello (2023).
* Corrected the lineup-memory simulator: the supplied lowest criterion now
  controls identification versus rejection, subsequent ordered criteria
  control confidence, and explicit criteria are preserved. The simulator now
  implements Independent Observations/MAX, Ensemble, and Integration exactly
  under its documented independent equal-variance assumptions. `"best_rest"`
  remains accepted for compatibility but is identified as a scale-equivalent
  parameterization of Ensemble.
* Reframed `fit_max_sdt()` as a restricted, aggregate, single-criterion,
  equal-variance Independent-Observations fit estimated by minimum Pearson
  chi-squared. It is not a maximum-likelihood fit and is not the complete
  confidence-based Wixted et al. (2018) model.

## Major New Features (pyWitness-inspired)

This release implements high-priority features from pyWitness (Mickes et al., 2024), significantly expanding r4lineups' analytical capabilities for eyewitness identification research.

### Model Comparison Framework

* `compare_models()`: Unified interface for fitting and comparing multiple models
  * 2-HT (Winter et al., 2022): Multinomial processing tree model
  * EIG (Starns et al., 2023): Expected Information Gain
  * Full ROC (Smith & Yang, 2020): Complete ROC using all responses
* S3 methods: `print()`, `summary()`, `plot()` for model comparison objects
* `format_comparison_table()`: Publication-ready comparison tables
* Automatic handling of different data requirements across models
* Side-by-side reporting of non-comparable estimands; AIC/BIC are reported only
  for the 2-HT likelihood fit and are not used to rank EIG or full-ROC AUC
* Side-by-side visualizations of model results

### pAUC Statistical Comparison

* `compare_pauc()`: Rigorous statistical testing of ROC curve differences
* Z-test framework with bootstrap-based standard errors
* Automatic false ID rate cutoff handling with interpolation
* Confidence intervals and effect size calculation (Cohen's d)
* Multiple comparison support with adjustment examples
* Publication-ready side-by-side ROC plots
* Customizable bootstrap samples and confidence levels

### Data Simulation and Power Analysis

* `simulate_lineup_data()`: Generate lineup identification data
  * Signal Detection Theory (SDT) model with three distinct decision families:
    * **MAX** (Independent Observations Model) - Default
    * **Ensemble** - Best match relative to the lineup mean
    * **Integration** - Sum memory strengths across all members
  * **BEST-REST** remains a compatibility parameterization. Its decision
    variable is `k / (k - 1)` times Ensemble, so the two fit identically when
    criteria are rescaled and must not be treated as independent model evidence
  * Based on Wixted et al. (2018) and pyWitness implementations
  * Flexible parameters: d', criterion, lineup size, confidence levels
  * Optional response time simulation
  * S3 class with print method
* `simulate_power_analysis()`: Determine required sample sizes
  * Multiple sample size evaluation
  * Customizable effect measures and comparison types
  * Power curve visualization
* Full integration with all r4lineups analyses

### z-ROC and SDT Parameter Estimation

* `fit_sdt_roc()`: Extract d', criteria, and variance ratio from ROC data
  * Equal and unequal variance SDT models
  * z-transformed ROC analysis following Macmillan & Creelman (2005)
  * Log-linear correction for extreme hit/false alarm rates
  * Bootstrap confidence intervals for parameter estimates
  * Model fit diagnostics (R-squared, residuals)
  * S3 class with `print()`, `plot()`, `summary()` methods
* Integration with `make_rocdata()` for seamless workflow
* Comprehensive examples and documentation
* 24 unit tests ensuring reliability

### Standardized Data Format and Validation

* `validate_lineup_data()`: Comprehensive data validation
  * Checks required columns (target_present, identification, confidence)
  * Validates data types and value ranges
  * Detects missing values and invalid identification categories
  * Warns about common data quality issues
  * Strict mode option for error-on-failure behavior
* `standardize_lineup_data()`: Convert data from various formats
  * Auto-detects common column names (tp, culprit_present, choice, etc.)
  * Recodes identification values to standard terms
  * Handles messy real-world data gracefully
  * Generates participant IDs if missing
  * Returns "lineup_data" S3 class with print method
* `create_example_lineup_data()`: Generate example data for testing
  * Configurable sample size and trial distribution
  * Optional confidence ratings and response times
  * Produces realistic identification patterns
  * Useful for demonstrations and method validation
* 76 unit tests covering validation, standardization, and integration
* Comprehensive example script with 10 real-world scenarios

### Response Time-Accuracy (RAC) Analysis

* `make_rac()`: Response time-accuracy characteristic analysis
* `make_racdata()`: Compute RAC data with time bins
* `make_rac_gg()`: Publication-ready RAC plots
* `print.lineup_rac()`: Print method for RAC objects
* Complementary to CAC analysis for objective memory assessment
* Follows Seale-Carlisle et al. (2019) methodology

### Full ROC, EIG, and PPV-Range Analysis

* `make_fullroc()` / `make_fullroc_data()` / `plot_fullroc()`: full ROC analysis
  using all lineup responses (Smith & Yang, 2020), with diagnosticity-ratio or
  a-priori ordering
* `compute_eig()` / `make_eig()` / `make_eig_data()` / `plot_eig()`: Expected
  Information Gain analysis (Starns et al., 2023)
* `ppv_by_confidence()` / `ppv_range_by_confidence()` / `make_ppv_range()` /
  `plot_ppv_range()`: positive predictive value across confidence levels, with
  nominal- and effective-size corrections

### Winter 2-HT Multinomial Processing Tree Model

* `fit_winter_2ht()`: two-high-threshold MPT model for the full 2x3 outcome
  table (Winter et al., 2022), with `print()`, `summary()`, and `plot()` methods
* `boot_winter_2ht()`: bootstrap confidence intervals for 2-HT parameters

### Calibration, ANRI, and Decision Analysis

* `make_calibration()` / `make_calibration_by_condition()` /
  `make_calibration_data()`: confidence-accuracy calibration (C, O/U, NRI)
* `compute_anri()` / `bootstrap_anri()` / `compare_anri()`: Adjusted Normalized
  Resolution Index with bootstrap inference
* `make_bayes_curves()`, `make_bree_curve()`: Bayesian prior-posterior curves
* `make_utility_curves()`, `make_utility_difference()`, `compare_utility()`:
  expected-utility analysis (Lampinen et al., 2019)
* `make_dpp()`, `compare_dpp()`: Deviation from Perfect Performance
  (Smith et al., 2018)

### Bayesian Beta-Binomial Inference

* Posterior inference for core fairness measures: `esize_T_bayes()`,
  `func_size_bayes()`, `diag_ratio_T_bayes()`, `calibration_bayes()`
* `sdt_compare()`: Bayesian comparison of SDT parameters between conditions

### mSDT and MAX SDT Compound-Decision Models

* Multi-item signal detection (mSDT) core functions: `pmax_filler()`,
  `dmax_filler()`, `qmax_filler()`, `rmax_filler()`, `max_filler_moments()`,
  `estimate_msdt_params()` (method-of-moments from rejection rates)
* `fit_max_sdt()` / `compare_max_sdt()`: restricted aggregate
  Independent-Observations/MAX fitting by minimum Pearson chi-squared, with
  bootstrap CIs and nested-constraint comparisons

### SDT Summary-Level Comparisons and GLM Estimation

* `sdt_summary_from_counts()`, `sdt_summary_variance()`, `compare_sdt_summary()`:
  SDT comparisons when only summary counts are available (Miller/Gourevitch or
  bootstrap variance)
* `fit_sdt_glm()` / `fit_sdt_glmm()` / `extract_sdt_metrics()`: SDT parameter
  estimation via probit/logit (mixed) models

### Face Similarity via Deep Learning

* `face_similarity()`, `lineup_similarity()`, `batch_embeddings()`, and related
  helpers compute facial similarity with deep embeddings (ArcFace, FaceNet, ...)
  through Python's deepface library (optional; requires `reticulate`)
* `install_r4lineups_python()` / `check_python_deps()` for setup

### Shiny App

* `run_r4lineups_app()`: interactive Shiny interface to the main analyses

### Core Fairness Measures

* Bootstrap distributions for lineup bias and effective size
  (`lineup_boot_allprop()`, `esize_boot_dist()` and friends)
* Data standardization is now the recommended entry point for all analyses
  (`standardize_lineup_data()`)

## Statistical Audit (June 2026)

A component-by-component numerical audit of the statistical routines was
completed for this release. Fixes:

* **MAX SDT fitter** (`fit_max_sdt()`): replaced the single boundary-prone
  optimizer start with a deterministic multistart grid plus Nelder-Mead polish;
  corrected the chi-square objective to use a proper outcome partition
  (correct-ID / filler-ID / reject); floored expected counts instead of dropping
  cells; corrected goodness-of-fit degrees of freedom (df = 3 x n_conditions -
  n_params); convergence is now reported honestly
* **Full ROC**: fixed `order = "apriori"` with `conf_bins` (labels were coerced
  to `NA`, silently breaking within-decision ordering)
* **PPV**: `ppv_by_confidence(correction = "effective")` computed the wrong
  effective size in the no-member-data fallback; now uses the pseudo-table
  counts directly
* **Calibration**: added a `confidence_scale = c("auto", "0-1", "0-100")`
  argument to `make_calibration_data()` and all callers; `auto` now warns when
  confidence looks like a Likert/0-10 scale instead of silently dividing by 100
* **Winter 2-HT**: removed the invalid single-condition goodness-of-fit
  chi-square test (the model is saturated, df = 0); `summary()` now reports a
  saturation note and the observed-vs-expected discrepancy as a convergence
  check
* **Diagnosticity ratio**: fixed diagnosticity-ratio bugs and documented the
  Haldane correction in `ln_diag_ratio()`
* Removed a duplicate internal `entropy()` definition that relied on file
  collation order

## Documentation

### New Vignettes

* `simulation_power_analysis.Rmd`: Complete guide to data simulation and power analysis
* `model_comparison.Rmd`: Comprehensive model comparison workflows
* `pauc_statistical_comparison.Rmd`: Statistical testing of ROC curves
* `rac_analysis.Rmd`: Response time-accuracy analysis
* `fullroc_analysis.Rmd`: Full ROC methodology
* `information_error_rate_analysis.Rmd`: EIG and PPV-range analysis
* `calibration_decision_analysis.Rmd`: Calibration, utility, DPP, and ANRI
* `winter_2ht_model.Rmd`: Winter 2-HT model
* `bayesian_inference.Rmd`: Bayesian posterior inference for lineup measures
* `msdt_model.Rmd`: Multi-item signal detection theory
* `sdt_summary_comparisons.Rmd`: SDT comparisons from summary counts
* `sdt_glm_analysis.Rmd`: SDT via GLM/GLMM
* `face_similarity.Rmd`: Deep-learning face similarity

All vignettes include:
* Complete workflows from start to finish
* Real-world examples and use cases
* Best practices and common pitfalls
* Publication-ready guidance
* References to original literature

### Example Scripts

* `model_comparison_example.R`: 8 comprehensive examples
* `pauc_comparison_example.R`: 8 comprehensive examples with interpretation
* `simulation_power_analysis_example.R`: Complete tutorial
* `rac_example.R`: RAC analysis demonstration

## Bug Fixes

* Fixed `.extract_counts_from_df()` in `winter_2ht.R` to properly handle dataframe inputs
  * Issue: Names were being combined incorrectly (e.g., "n_tp_suspect.suspect")
  * Fix: Added `unname()` to ensure clean names
  * Result: 2-HT model now works correctly with dataframe inputs

## Testing

* Added comprehensive testthat unit tests
  * `test-model-comparison.R`: Model comparison framework tests
  * `test-pauc-comparison.R`: pAUC statistical comparison tests
  * `test-simulation.R`: Data simulation and power analysis tests
* Tests cover basic functionality, edge cases, and integration

## Dependencies

* Added `gridExtra` to Imports (used by `plot.model_comparison()`)
* Added `testthat (>= 3.0.0)` to Suggests for unit testing

## References

New features based on:

* Mickes, L., Seale-Carlisle, T. M., Chen, X., & Boogert, S. (2024). pyWitness 1.0: A python eyewitness identification analysis toolkit. *Behavior Research Methods, 56*, 1533-1550.

* Winter, K., Menne, N. M., Bell, R., & Buchner, A. (2022). Experimental validation of a multinomial processing tree model for analyzing eyewitness identification decisions. *Scientific Reports, 12*, 15571.

* Starns, J. J., Cohen, A. L., & Rotello, C. M. (2023). A complete method for assessing the effectiveness of eyewitness identification procedures: Expected information gain. *Psychological Review, 130*(3), 677–719.

* Smith, A. M., Yang, Y., & Wells, G. L. (2020). Distinguishing between investigator discriminability and eyewitness discriminability. *Perspectives on Psychological Science, 15*(3), 589-607.

* Seale-Carlisle, T. M., Wetmore, S. A., Flowe, H. D., & Mickes, L. (2019). Designing police lineups to maximize memory performance. *Journal of Applied Research in Memory and Cognition, 8*(4), 420-428.

---

# r4lineups 0.1.2

Previous release. See earlier documentation for details.
