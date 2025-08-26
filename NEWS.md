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
