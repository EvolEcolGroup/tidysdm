# `tidysdm` dev
* fix bug in predict.simple_ensemble where weights were ignored 
  for "weighted mean"
* implement better prediction logic for `repeat_ensembles`, grouping
  by repeat
* implement `calib_class_thresh` method for `repeat_ensembles`
* `add_repeat()` now removes any class threshold calibration from the
  `repeat_ensemble` (with a warning), as it might be inconsistent with the
  added repeats; previously, the new repeats were left uncalibrated, and
  a reused `rep_id` could inherit a stale calibration
* fix `predict.repeat_ensemble` for class predictions: a numeric
  `class_thresh` (or the default of 0.5) no longer requires calibration;
  a metric `class_thresh` gives a clear error if the ensemble is not
  calibrated for that combination of `class_thresh` and `metric_thresh`,
  rather than silently skipping repeats; the message about the default
  threshold is given only once
* implement `collect_calib_thresh` method for `repeat_ensembles`
* resolve conflicting coords names in `thin_by_cell()`
* make sure that coords name is passed on by `thin_by_cell_time()`
* extend support for all methods from `blockCV`, and allow the creation of
  an initial split from a `blockCV` object
* fix `explain_tidysdm()` passing the response to DALEX as 1/2 rather than
  1 (presence) / 0 (absence); this corrupted the default loss used by
  `DALEX::model_parts()` (variable importance) and `DALEX::model_performance()`
* fix labels of explainers created by `explain_tidysdm()` with
  `by_workflow = TRUE` for `repeat_ensemble` objects, which now include the
  repeat id
* `explain_tidysdm()` with `by_workflow = TRUE` now correctly detects recipes
  with steps (and asks for `data` to be provided), as it already did for the
  whole ensemble
* fix `check_splits_balance()`, which swapped the counts for the analysis and
  assessment sets (and for the training and testing sets of an initial split)
* fix the `to_remove` attribute returned by `filter_collinear()` with
  `names = FALSE`, which listed the indices of all variables
* fix errors in `filter_collinear()` when only one variable is left to
  evaluate (e.g. when removing one of two collinear variables, or after
  removing the variables correlated with those in `to_keep`), or when all
  variables are in `to_keep`
* `filter_collinear()` now gives an informative error if any of the variables
  in `to_keep` is missing (previously only if all of them were), and checks
  the absolute correlation among the variables in `to_keep`, so that strong
  negative correlations are also caught
* fix `clamp_predictors()` for `SpatRaster` objects, which applied the limits
  from `training` by position rather than by name, giving wrong results if
  the columns of `training` were in a different order from the layers of `x`,
  or if `x` had additional layers; layers (or datasets of a
  `SpatRasterDataset`) that are not in `training` are now left unchanged
* fix `extrapol_mess()`, which matched the columns of `training` to the layers
  of `x` (or the columns of a `data.frame`) by position rather than by name;
  the `data.frame` method now also works with tibbles
* fix `maxnet_fit()` (and thus fitting a `maxent()` model) when `data` is a
  plain `data.frame` rather than a tibble
* fix `gam_formula()`, which added smooths for all predictors (including
  factors), malformed the formula when factors were present, and refused
  recipes with a single numeric predictor
* `autoplot()` for `simple_ensemble` now gives rank 1 to the best workflow,
  as documented (previously the workflow with the lowest value of the
  metric was ranked first, i.e. the worst one for metrics such as AUC)
* `optim_thresh()` (and thus `calib_class_thresh()`) now gives an error for an
  invalid `metric`, rather than returning the string "invalid metric"
* fix the threshold for a target sensitivity (e.g. `class_thresh =
  c("sensitivity", 0.9)`), which could give a sensitivity below the target,
  and returned the lowest possible threshold (i.e. a sensitivity of 1) for
  targets below the minimum achievable sensitivity; the highest threshold
  that achieves at least the target sensitivity is now used
* fix the `weighted_median` used to combine predictions in ensembles, which
  returned multiple values (and thus a malformed prediction) when two values
  were equally close to half of the cumulative weight, and differed from the
  median when all weights were equal
* `add_repeat()` now checks that the repeat being added has the same set of
  metrics as the `repeat_ensemble` (previously, only metrics that were all
  different were caught, and the same metrics in a different order gave an
  error)
* class predictions from `predict.repeat_ensemble()` now treat the first level
  of the response as the presence class (as for `simple_ensemble`), rather
  than requiring it to be called "presence"
* `check_sdm_presence()` now evaluates `presence_level` as a normal argument,
  so that it can be passed as a variable (previously the name of the variable
  was used as the level)
* fix `sample_background_time()` and `sample_pseudoabs_time()` with
  `return_pres = TRUE` for `sf` objects whose geometry column is not called
  "geometry"
* consistent handling of the time axis of `stars` objects in
  `sample_background_time()` (which previously lost the time axis unless
  times were POSIXct), `sample_pseudoabs_time()` and `thin_by_cell_time()`
  (which previously failed if times were POSIXct or Date)
* fix the `*_time()` functions (e.g. `sample_pseudoabs_time()` and
  `thin_by_cell_time()`) for rasters with a single time step
* fix `thin_by_dist_time()` when times are within a day of 1970-01-01, for
  which time intervals were computed in seconds rather than days
* `spatial_initial_split()` now accepts a `strategy` given with its namespace
  (e.g. `spatialsample::spatial_block_cv`)
* `sample_background()` and `sample_pseudoabs()` no longer warn that there are
  fewer available cells than requested when the number of available cells is
  exactly `n`
* the `verbose` argument of `prep()` is now passed on for `spatial_recipe`
  objects (it was always set to `FALSE`)

# `tidysdm` 1.0.4
* improve documentation of `calib_class_threshold` and implement 
  `collect_calib_thresh`
* update saved objects to the new version of `xgboost`

# `tidysdm` 1.0.3
* add `mgcv` to suggested packages to deal with upcoming changes in `ggplot2`
  4.0.0

# `tidysdm` 1.0.2
* when using `predict_raster()`, process the raster in chunks if it is too
  large to fit in memory.
* run tests relying on suggested packages on if those packages are already
  installed

# `tidysdm` 1.0.0
* add interoperability with `stars` (thanks to @btupper)
* fixed inverted plots for `explain_tidysdm` when plotting for all workflows.
* when using `explain_tidysdm` with a recipe that contains steps, data have to be
  passed explicitly to the `data` argument
* implement `make_mask_from_presence` to define the area of interest
* check time units of rasters for `sample_background_time()` and 
  `sample_pseudo_absences_time()` (thanks to @zpmdal)
* show the use of map projections in the overview vignette, and make sure that
  projections are properly handled in functions such as `thin_by_cell`
  and `thin_by_dist`

# `tidysdm` 0.9.5
* implement clamping and MESS to manage extrapolation
* clearly separate sampling of background vs pseudo-absences
* update vignettes

# `tidysdm` 0.9.4
* fix a but in the `predict*` functions that prevented a fixed threshold to be used
  to assign classes
* ensure compatibility with upcoming changes in `tune`


# `tidysdm` 0.9.3
* fix bug in `filter_high_cor` due to changes in `terra` 1.6.75
* implement `collect_metrics` for ensembles.

# `tidysdm` 0.9.2
* Release on CRAN

# `tidysdm` 0.9.1
* Add a `spatial_recipe` class. This is a BREAKING change that makes previously
saved objects unusable, but old code will work as expected.
* Additional articles showing how to use additional `tidymodels` features, and
how to debug errors.
* Integration of DALEX to explain models.
* More functions to select variables.

# `tidysdm` 0.9.0
* Initial release on GitHub.
