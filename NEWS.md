# `tidysdm` development version

* implement `make_mask_from_presence` to define the area of interest
* add interoperability with `stars` (thanks to @btupper)
* check time units of rasters for `sample_background_time()` and 
  `sample_pseudo_absences_time()` (thanks to @zpmdal)

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
