# Code review of `tidysdm`: branch `codereview`

- **Base:** `8d77b5a` ("Remove lints")
- **Branch:** [`codereview`](https://github.com/EvolEcolGroup/tidysdm/tree/codereview), 36 commits
- **Date:** 2026-10-22

This review covered the whole package (all files in `R/`). Each problem was
first reproduced with a script, then fixed in its own commit. Every bug-fix
commit includes:

- a regression test that fails without the fix and passes with it
- a NEWS entry for user-facing changes
- regenerated `man/` pages if the roxygen docs changed

Tick each box once the commit has been reviewed. Any commit can be reverted or
cherry-picked on its own; the only exception is #26, which supersedes the code
change in #25 (#25 still adds the `verbose` test).

**Severity:**

- **High:** silently wrong results
- **Medium:** errors or wrong results in plausible use
- **Low:** minor or edge cases
- **Docs:** documentation, messages or packaging only

## How to review

```bash
git fetch origin codereview
git log --reverse --oneline 8d77b5a..origin/codereview
git show <hash>                       # review one commit
```

```r
devtools::test()                      # full suite
testthat::test_file("tests/testthat/test_<name>.R")
```

## Verification

- [ ] Full test suite: 634 expectations, 0 failures, 0 warnings, 0 skips
  (all optional packages installed).
- [ ] `R CMD check --no-manual --ignore-vignettes`: 0 errors and 0 warnings.
  There was 1 NOTE ("unable to verify current time"), which came from the
  check environment, not the package.
- [ ] `lintr::lint_package()`: no new lints. The only lints are the
  `object_usage_linter` warnings for `%>%`, which are also present on the
  base commit.
- [ ] `spelling::spell_check_package()`: no errors.
- [ ] Vignettes were **not** built during the review. They need the data
  downloads, so they should be checked separately. Output from DALEX and
  `autoplot()` will change (see below).

## Behaviour changes users may notice

- [ ] **DALEX results change** (#1). Variable importance from
  `model_parts()` and the measures from `model_performance()` are now
  computed correctly. Expect different numbers and plots in the vignettes.
- [ ] **`autoplot(<simple_ensemble>)` rank order is reversed** (#12). Rank 1
  is now the best model, as documented.
- [ ] **Sensitivity-based thresholds change** (#14). This affects
  `class_thresh = c("sens", x)` in `calib_class_thresh()` and `predict()`.
- [ ] **`weighted_median` predictions change slightly** (#15). This happens
  with an even number of equally weighted models, or with tied cumulative
  weights.
- [ ] **`check_sdm_presence()` no longer accepts an unquoted
  `presence_level`** (#18). It is now evaluated as a normal string
  argument, as documented.
- [ ] **`filter_collinear()` errors if any `to_keep` variable is missing**
  (#7). Previously the error only happened if all of them were missing.

---

## Commits

### Model explanation (DALEX)

- [X] MC TODO **#1 High: [`ccb8bf4`](https://github.com/EvolEcolGroup/tidysdm/commit/ccb8bf4fbadf83fa615dc966f517d785d13a27ba) Fix response coding in `explain_tidysdm()`**
  - *Problem:* the response was passed to DALEX as `as.numeric(factor)`,
    giving 1 for presence and 2 for absence. DALEX expects 0/1. As a
    result, `model_performance()` reported an AUC of 0.57 for
    `lacerta_ensemble`, whose true AUC is 0.975. Variable importance used
    the same broken loss.
  - *Fix:* the response is now coded 1 for presence (the first level) and
    0 otherwise, both for the whole ensemble and with `by_workflow = TRUE`.
  - *Test:* `test_explain_tidysdm.R` checks that `y` is 0/1 and that the
    DALEX loss matches `yardstick::roc_auc`.
  - *Comment:* if DALEX expects 0/1 then accept the fix. For model_parts() it says: 
    “By default it is 1-AUC for classification, cross entropy for multilabel classification and RMSE for regression.”
    In explain_tidymodels() the argument predict_function_target_column says 
    "Character or numeric containing either column name or column number in the 
    model prediction object of the class that should be considered as positive 
    (i.e. the class that is associated with probability 1)."
- [X] MC **#2 Medium: [`c26bb06`](https://github.com/EvolEcolGroup/tidysdm/commit/c26bb06db69d3a0e7e96bd9c750350fe7c863810) Fix workflow labels when explaining repeat ensembles**
  - *Problem:* the method assigned to a new column `workflow_id` instead
    of `wflow_id`, so with `by_workflow = TRUE` the explainers for
    different repeats had identical labels.
  - *Test:* labels are checked to be `rep_id.wflow_id`.
  - *Comment:* It seems that only the original $wflow_id is used, 
    so the new variable that was created was actually never used
- [X] MC **#3 Medium: [`f6164a9`](https://github.com/EvolEcolGroup/tidysdm/commit/f6164a92cdc751afe24573ae9d0f3775d76568bf) Detect recipe steps when explaining by workflow**
  - *Problem:* the check for recipe steps looked at `model$pre`, which does
    not exist on an ensemble. It never fired and raised an "Unknown column"
    warning.
  - *Test:* with `by_workflow = TRUE`, a recipe with steps gives an error,
    and passing `data` works.
  - *Comment:* The function tried to access `model$pre$actions$recipe$recipe$steps` 
    but `model$pre` is NULL so it fails without error. 
    This fix also avoids using pre-processed data for recipes - this bug might have had some impact on results

### Data splitting

- [X] MC TODO **#4 High: [`3afee81`](https://github.com/EvolEcolGroup/tidysdm/commit/3afee815291d4f2b9208e464d6a2f2d6196ab640) Fix swapped counts in `check_splits_balance()`**
  - *Problem:* for an `rset`, the analysis counts were labelled
    `_assessment` and vice versa. For an initial split, the training counts
    were in the `_test` columns.
  - *Fix:* column names and their order are unchanged; the values now
    match their labels.
  - *Test:* each cell is compared to `table()` of
    `analysis()`/`assessment()` (or `training()`/`testing()`).
  - *Comment:* not a labelling issue but it actually inverts results. 
    Could add an additional check for labels, as the new test checks only counts observations in each class,
    an additional check on levels to be sure it is comparing presence_test to presence and not just to the first level available
  
### Variable selection (`filter_collinear()`)

- [ ] **#5 Medium: [`7ea1097`](https://github.com/EvolEcolGroup/tidysdm/commit/7ea1097b59f768a41ab9c6035e012b7013a06b4e) Fix `to_remove` indices with `names = FALSE`**
  - *Problem:* the `to_remove` attribute listed every variable.
  - *Test:* the indices of kept and removed variables are checked against
    the variable names.
- [ ] **#6 Medium: [`ae9ef6d`](https://github.com/EvolEcolGroup/tidysdm/commit/ae9ef6dee616ea4fa0967bdc4994f6e83caf7d21) Fix `filter_collinear()` when a single variable is left**
  - *Problem:* matrices were subset without `drop = FALSE`, which caused
    three different crashes:
    - `vif_step` when removing one of two collinear variables
    - `vif_fast()` and the verbose output with one variable
    - `cor_caret` when `to_keep` left fewer than two other variables
  - *Test:* each of the 3 methods is run with 2 collinear variables, with
    `to_keep` leaving 1 variable, and with all variables in `to_keep`.
- [ ] **#7 Medium: [`624386b`](https://github.com/EvolEcolGroup/tidysdm/commit/624386b63a9aa959e02afd0af71e528079af35c9) Validate `to_keep` properly**
  - *Problem:* there were two errors in the validation:
    - `any()` was used instead of `all()`, so a typo in one of several
      names was not caught
    - the correlation among `to_keep` variables was signed, so strongly
      negative correlations passed the check
  - *Test:* covers a partially missing `to_keep` for all methods, and
    negatively correlated `to_keep` variables.

### Extrapolation

- [ ] MC TODO **#8 High: [`d8d3211`](https://github.com/EvolEcolGroup/tidysdm/commit/d8d32115754b6d771f7a4ee5a7caebfb8bf6d76d) Match training variables to layers by name in `clamp_predictors()`**
  - *Problem:* `terra::clamp()` applies its limits by layer order, but the
    limits were computed in the column order of `training`. Columns in a
    different order, or extra layers in the raster, were clamped with the
    wrong limits. The `SpatRasterDataset` method errored for datasets not
    in `training`.
  - *Fix:* limits are matched by name, and layers or datasets that are not
    in `training` are left unchanged.
  - *Test:* uses a toy raster with swapped training columns and an extra
    layer, with `use_na = TRUE/FALSE`, plus a `SpatRasterDataset`.
  - *Comment:* Accept, but double check range of acceptable values. The error is 
    not informative (either prints `"invalid metric"` or 
    `Error in if (metric == "tss_max") { : the condition has length > 1`), but 
    the issue might arise in analysis downstream if the error "invalid metrics"" is taken as an output.
    Now the tests ensure that there is an error.
    Do line 7&8 of `test_optim_thresh.R` need also to check values (do we have a range of acceptable values)?
- [ ] **#9 High: [`eb005ab`](https://github.com/EvolEcolGroup/tidysdm/commit/eb005abb00754fc3e9935b6e0d7d397c10d17a3e) Match training variables by name in `extrapol_mess()`**
  - *Problem:* the method only checked `setequal()` on the names, then
    paired columns by position, so a different column order gave wrong
    MESS scores. The `data.frame` method also failed on tibbles.
  - *Test:* a known MESS result is reproduced with both column orders, for
    a `SpatRaster`, a `data.frame` and a tibble.

### Model specification and fitting

- [ ] **#10 Medium: [`e388971`](https://github.com/EvolEcolGroup/tidysdm/commit/e388971ef9c0043a93953ae832fac837d392fec4) Fix `maxnet_fit()` for plain data.frames**
  - *Problem:* `data[, col] %>% pull()` fails on a `data.frame`, which
    broke `maxent() %>% fit(..., data = <data.frame>)`.
  - *Test:* fitting on a `data.frame` gives the same result as fitting on
    a tibble, and `parsnip::fit()` works.
- [ ] **#11 Medium: [`e25d440`](https://github.com/EvolEcolGroup/tidysdm/commit/e25d4402d33149aa0c9e302ccecd42d023753b79) Fix `gam_formula()` with factors or a single numeric predictor**
  - *Problem:* three bugs:
    - factors were wrapped in `s()`
    - the factor terms were pasted into a malformed formula
    - exactly one numeric predictor was rejected as "no numeric
      predictors"
  - *Test:* covers numeric-only, single numeric, numeric plus factors, and
    no numeric predictors.

### Ensembles: plotting, thresholds and prediction

- [ ] **#12 Medium: [`66d2153`](https://github.com/EvolEcolGroup/tidysdm/commit/66d215312b5f959596c58bab4f49ed18cf949c85) Rank the best workflow first in `autoplot.simple_ensemble()`**
  - *Problem:* the metric was sorted in increasing order, so the *worst*
    model got rank 1 for AUC, Boyce and TSS. The docs also said 95% where
    the bounds are 90% (`qnorm(0.95)`).
  - *Fix:* the sort direction is taken from the yardstick/tidysdm metric
    function.
  - *Test:* rank order is checked for a maximised metric (`roc_auc`) and a
    minimised one (`mn_log_loss`).
- [ ] **#13 Medium: [`5a095f1`](https://github.com/EvolEcolGroup/tidysdm/commit/5a095f19bc16c1bf1df1778b11c3a6a8ca1ce887) Error on invalid metrics in `optim_thresh()`**
  - *Problem:* the function returned the *string* `"invalid metric"` as
    the threshold.
  - *Test:* new `test_optim_thresh.R` covers valid metrics and invalid
    ones.
- [ ] **#14 High: [`1a0e339`](https://github.com/EvolEcolGroup/tidysdm/commit/1a0e339b843e5f01cdede6f1c8a6139b087ef5e2) Fix threshold for a target sensitivity**
  - *Problem:* `which.min(sens > target)` gave a sensitivity below the
    target (e.g. 0.8 for a target of 0.85). If every threshold exceeded the
    target, it returned the *lowest* threshold, giving a sensitivity of 1.
  - *Fix:* the threshold is now the highest one that achieves at least the
    target. A target outside 0 to 1 gives an error.
  - *Test:* for a range of targets, the target is met and no higher
    threshold meets it.
- [ ] **#15 High: [`225aa02`](https://github.com/EvolEcolGroup/tidysdm/commit/225aa02e0a64cd2b8813296c4a50ebbcb992039d) Fix weighted median for ensemble predictions**
  - *Problem:* tied cumulative weights (e.g. 1, 2, 1) returned two values,
    so `predict()` returned one column per location. With equal weights it
    did not match the median.
  - *Fix:* the weighted median is the first value whose cumulative weight
    reaches 0.5, averaged with the next value when it is exactly 0.5. It
    now lives in the internal helper `weighted_median_vec()`.
  - *Test:* covers the helper directly, and `predict()` with tied weights
    and with equal weights.
- [X] MC **#16 Medium: [`bb2d6eb`](https://github.com/EvolEcolGroup/tidysdm/commit/bb2d6eba095c8354d5f82a5855fcd4cdc7be0265) Compare metric sets properly in `add_repeat()`**
  - *Problem:* `all(x != y)` let partially different metrics through and
    rejected the same metrics in a different order.
  - *Fix:* uses `setequal()`.
  - *Test:* covers a partial difference, an extra metric and a different
    order.
  - *Comment:* before fix, it seems that the script compared the two metric 
    vectors element by element (with `!=` and `all() `) this returns `TRUE` only 
    when every corresponding element is different, which is wrong. 
    This would impact downstream analysis, but it might be an edge case.
    Could add check that the metrics are still correct?
- [ ] **#17 Medium: [`77634af`](https://github.com/EvolEcolGroup/tidysdm/commit/77634af638b658d282264f8f1233a43502da839a) Use the first level as presence in repeat ensemble class predictions**
  - *Problem:* combining classes across repeats required a level literally
    called `"presence"`. `predict.simple_ensemble()` uses the first level
    instead.
  - *Test:* a repeat ensemble built from `two_class_res` (levels
    `Class1`/`Class2`), with `majority` and `prop`.

### Data preparation and sampling

- [ ] **#18 Medium: [`5b699dc`](https://github.com/EvolEcolGroup/tidysdm/commit/5b699dcd8669d5e3395f28c9c7e3c245a02f513e) Evaluate `presence_level` normally in `check_sdm_presence()`**
  - *Problem:* `presence_level = my_var` checked for a level called
    `"my_var"`. A stray `browser` was also left in the function body.
  - *Test:* covers `presence_level` passed as a variable, and a non-string
    `presence_level`.
- [ ] **#19 Medium: [`00d211a`](https://github.com/EvolEcolGroup/tidysdm/commit/00d211a5f97dac62020b46b550e74674094e5651) Support any geometry column name in the time samplers**
  - *Problem:* with `return_pres = TRUE`, the code used `data$geometry`,
    which is `NULL` when the geometry column is named e.g. `geom` (as
    after `sf::st_read()` on a GeoPackage).
  - *Test:* `sample_background_time()` and `sample_pseudoabs_time()` on an
    `sf` with a `geom` column.
- [ ] **#20 Medium: [`651f388`](https://github.com/EvolEcolGroup/tidysdm/commit/651f38840e882749e34279c9de3e3c0279505fa1) Handle the time axis of `stars` objects consistently**
  - *Problem:* the three functions converted `stars` objects differently:
    - `sample_background_time()` lost "years" time axes
    - `sample_pseudoabs_time()` and `thin_by_cell_time()` failed with
      POSIXct or Date times
  - *Fix:* all three use a shared helper, `stars_to_spatraster_time()` in
    `R/misc.R`.
  - *Test:* `stars` with a "years" time axis for
    `sample_background_time()`, and with POSIXct times for all three.
- [ ] **#21 Medium: [`1c26776`](https://github.com/EvolEcolGroup/tidysdm/commit/1c2677689232cd150e928d648685051958d5e174) Fix `out_of_range_warning()` for a single time step**
  - *Problem:* the range had an NA bound, so every `*_time()` function
    failed on single-layer rasters.
  - *Test:* covers the unit, and `sample_pseudoabs_time()` with a single
    time step.
- [ ] **#22 Medium: [`3feb609`](https://github.com/EvolEcolGroup/tidysdm/commit/3feb609c6fb93117c3ad956429853c5e97855eba) Compute time intervals in days in `thin_by_dist_time()`**
  - *Problem:* `difftime` chooses its units automatically, giving seconds
    near 1970-01-01, while `interval_min` is in days.
  - *Test:* points half a day apart near the origin are thinned.
- [ ] **#23 Low: [`df0d7d5`](https://github.com/EvolEcolGroup/tidysdm/commit/df0d7d57da09bbfcf499a52007b2f088af2e985c) Accept namespaced strategies in `spatial_initial_split()`**
  - *Problem:* `spatialsample::spatial_block_cv` was rejected as "not a
    function in spatialsample".
  - *Fix:* checks that the function belongs to the spatialsample
    namespace.
  - *Test:* a namespaced strategy gives the same split; a non-spatialsample
    function still gives an error.
- [ ] **#24 Low: [`ea21b72`](https://github.com/EvolEcolGroup/tidysdm/commit/ea21b7213564223fc21f602aabefff20ef271627) Only warn about too few cells when there are fewer than `n`**
  - *Problem:* `sample_background()` and `sample_pseudoabs()` warned even
    when exactly `n` cells were available.
  - *Test:* no warning when `n` equals the number of available cells.
- [ ] **#34 Medium: [`a80e55b`](https://github.com/EvolEcolGroup/tidysdm/commit/a80e55b843d5c2ba30a9cf963e3f37989106e7ce) Only use numeric variables in `dist_pres_vs_bg()` and `plot_pres_vs_bg()`**
  - *Problem:* the code comment said it subset to numeric columns, but it
    used every column, so factor or id columns made `dist_pres_vs_bg()`
    fail.
  - *Test:* a dataset with factor and character columns.

### Recipes

- [ ] **#25 Low: [`f7280b3`](https://github.com/EvolEcolGroup/tidysdm/commit/f7280b301772d0d13ba5e2fc0c57b0ffec26546b) Pass `verbose` on in `prep.spatial_recipe()`**
  - *Problem:* `verbose` was hard-coded to `FALSE`.
  - *Test:* `expect_output()` with `verbose = TRUE`.
- [ ] **#26 High: [`d6bf76b`](https://github.com/EvolEcolGroup/tidysdm/commit/d6bf76be84b7059793f4be71e47166c0c04307c0) Fix positional arguments in `prep()` and `bake()` for spatial recipes**
  - *Problem:* `NextMethod()` was called with arguments re-named
    explicitly. That *appends* to the original call rather than replacing
    positional arguments, which caused three problems:
    - `bake(rec, data)` failed with "Can't select columns with
      `new_data`"
    - the recipe object ended up in `...`
    - `strings_as_factors` was always passed, triggering the recipes 1.3.0
      deprecation warning
  - *Fix:* the data are modified in place and `NextMethod()` is called
    without arguments. This also makes the change in #25 redundant.
  - *Test:* positional `training` and `new_data`, column selection in
    `bake()`, and no deprecation warning. The old `expect_warning()`
    "@TODO" checks are now `expect_no_warning()`.

### Packaging

- [ ] **#27 Medium: [`ddc47ab`](https://github.com/EvolEcolGroup/tidysdm/commit/ddc47ab59e40e55bd3fe8421836d8f87ccbde6dc) Require `dplyr (>= 1.2.0)`**
  - `dplyr::recode_values()`, which `maxnet_fit()`, `autoplot()` and the
    examples use, was introduced in dplyr 1.2.0.
- [ ] **#35 Docs: [`d7d77f7`](https://github.com/EvolEcolGroup/tidysdm/commit/d7d77f71e4dd4a8ca0569e3797ed8460f92826e5) Format code in a NEWS entry**
  - Adds backticks, which also avoids a spelling note.
- [ ] **#36 Docs: [`31e7b86`](https://github.com/EvolEcolGroup/tidysdm/commit/31e7b867917b1609ea809e0940ebc2d32ee01575) Add `WGS` to the spelling wordlist**
  - Pre-existing spelling note for "WGS84".

### Documentation and messages

- [ ] **#28 Docs: [`8ae49f4`](https://github.com/EvolEcolGroup/tidysdm/commit/8ae49f491b1445ec2447ece73d542aa0be59a653) Fix the TSS formula in the documentation**
  - The formula read "sensitivity + specificity + 1"; it is "- 1".
- [ ] **#29 Docs: [`f7fd9e6`](https://github.com/EvolEcolGroup/tidysdm/commit/f7fd9e69356f208236810436ca40643cd532c37e) Fix the number of rows in the dataset documentation**
  - All three datasets were documented as having 1,297 rows. The actual
    counts are:
    - `lacerta`: 1,268
    - `horses`: 788
    - `lacertidae_background`: 34,677
  - Adds `test_datasets.R`.
- [ ] **#30 Docs: [`971b594`](https://github.com/EvolEcolGroup/tidysdm/commit/971b594919023c187157c4ee3324fc45989f5ddb) Fix formatting of several error and warning messages**
  - Missing spaces ("numberof", "combinationof").
  - The `blockcv2rsample()` error did not list all supported classes.
  - `clamp_predictors()`:
    - variable names were not separated by commas
    - the `SpatRasterDataset` method passed `sep = ","` to `stop()`, and
      used `varnames()` rather than `names()`
  - Dates in `out_of_range_warning()` were not separated by commas.
  - Tests check each message.
- [ ] **#31 Docs: [`85622e8`](https://github.com/EvolEcolGroup/tidysdm/commit/85622e8bb7be3b4da77849dad2cdc135fdc88867) Fix documentation of the time sampling functions**
  - A literal `#' @details` appeared in `sample_pseudoabs_time()`.
  - `time_buffer` referred to method `'max_dist'` (it is `'dist_max'`).
  - There were unbalanced backticks.
- [ ] **#32 Docs: [`9f8cd6a`](https://github.com/EvolEcolGroup/tidysdm/commit/9f8cd6a9b344992a4268f6e81c92fb7be6f92786) Clarify the meaning of `prop` in `spatial_initial_split()`**
  - The first sentence said `prop` was the training proportion, but the
    code, the rest of the docs and the tests all use it as the testing
    proportion (see the open questions).
- [ ] **#33 Docs: [`1653f66`](https://github.com/EvolEcolGroup/tidysdm/commit/1653f667b76fa11cc97adc0b38bf8daef8c45e0e) Refer to existing functions in the ensemble documentation**
  - `add_best_candidates()`, `add_members()` and `add_repeats()` do not
    exist; they are now `add_member()` and `add_repeat()`.
  - Adds a test on the print messages.

---

## Open questions (not changed; decision needed)

- [ ] **Weights when `metric_thresh` is set.** When `metric_thresh` is set,
  `predict.simple_ensemble()` computes the `weighted_mean` and
  `weighted_median` weights from the `metric_thresh` metric. The code
  comment says this is intended. However, the docs of
  `predict.simple_ensemble()` and `calib_class_thresh()` say the weights
  are based on the metric used to tune the ensemble. Which behaviour is
  intended? Either the code or the docs should change.
- [ ] **`prop` in `spatial_initial_split()`** is the testing proportion.
  This is the opposite of `rsample::initial_split()`, where it is the
  training proportion. Only the docs were clarified (#32); changing the
  behaviour would break existing code.
- [ ] **`tests/testthat/_test_predict_repeat_ensemble.R`** never runs
  (because of the leading underscore) and looks superseded by
  `test_predict_repeated_ensemble.R`. Delete it?
- [ ] **`filter_caret_algorithm()`** keeps caret's quirk of comparing
  `mean(x2[i, ])` with `mean(x2[-j, ])` (all rows except `j`) rather than
  with row `j`. This was kept for fidelity with `caret::findCorrelation()`.
  Should it be kept?
- [ ] **`predict.simple_ensemble()` with a partially calibrated ensemble.**
  If a requested `fun` has no calibrated threshold, every prediction
  silently becomes the absence class. This cannot happen through
  `calib_class_thresh()`, which always calibrates all four functions; it
  only happens if the attribute is edited by hand. Add a defensive error?
