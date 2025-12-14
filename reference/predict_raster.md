# Make predictions for a whole raster

This function allows to use a raster as data to make predictions from a
variety of `tidymodels` objects, such as
[`simple_ensemble`](https://evolecolgroup.github.io/tidysdm/reference/simple_ensemble.md)
or
[`stacks::stacks`](https://stacks.tidymodels.org/reference/stacks.html)

## Usage

``` r
predict_raster(object, raster, ...)

# Default S3 method
predict_raster(object, raster, filename = "", n = 4, ...)
```

## Arguments

- object:

  the `tidymodels` object of interest

- raster:

  the
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or `stars` with the input data. It has to include levels with the same
  names as the variables used in `object`

- ...:

  parameters to be passed to the standard
  [`predict()`](https://rdrr.io/r/stats/predict.html) function for the
  appropriate object type (e.g. `metric_thresh`, `class_thresh` or
  `fun`). See the documentation of the relevant
  [`predict()`](https://rdrr.io/r/stats/predict.html) method for
  possible parameters (e.g.
  [`predict.simple_ensemble()`](https://evolecolgroup.github.io/tidysdm/reference/predict.simple_ensemble.md)).

- filename:

  the name of the output file raster file; this is only needed to save
  large rasters, and can be left blank for rasters that can be kept in
  memory.

- n:

  positive integer indicating how many copies the data may be in memory
  at any point in time (it defaults to 4). This is used to determine
  whether rasters can be processed in one go, or in chunks. If you get
  an out of memory error, increase `n`. See
  [`terra::writeStart()`](https://rspatial.github.io/terra/reference/readwrite.html)
  for more details.

## Value

a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
(or `stars` if that is the input) with the predictions
