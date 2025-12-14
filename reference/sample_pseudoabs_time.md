# Sample pseudo-absence points for SDM analysis for points with a time point.

This function samples pseudo-absence points from a raster given a set of
presences. The locations returned as the center points of the sampled
cells, which can not overlap with the presences (in contrast to
background points, see
[sample_background_time](https://evolecolgroup.github.io/tidysdm/reference/sample_background_time.md)).
The following methods are implemented:

- 'random': pseudo-absences randomly sampled from the region covered by
  the raster (i.e. not NAs).

- 'dist_min': pseudo-absences randomly sampled from the region excluding
  a buffer of 'dist_min' from presences (distances in 'm' for lonlat
  rasters, and in map units for projected rasters).

- 'dist_max': pseudo-absences randomly sampled from the unioned buffers
  of 'dist_max' from presences (distances in 'm' for lonlat rasters, and
  in map units for projected rasters). Using the union of buffers means
  that areas that are in multiple buffers are not oversampled. This is
  also referred to as "thickening".

- 'dist_disc': pseudo-absences randomly sampled from the unioned discs
  around presences with the two values of 'dist_disc' defining the
  minimum and maximum distance from presences.

## Usage

``` r
sample_pseudoabs_time(
  data,
  raster,
  n_per_presence,
  coords = NULL,
  time_col = "time",
  lubridate_fun = c,
  method = "random",
  class_label = "pseudoabs",
  return_pres = TRUE,
  time_buffer = 0
)
```

## Arguments

- data:

  An [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) data
  frame, or a data frame with coordinate variables. These can be defined
  in `coords`, unless they have standard names (see details below).

- raster:

  the
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  `stars` or
  [terra::SpatRasterDataset](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  from which cells will be sampled. If a
  [terra::SpatRasterDataset](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  the first dataset will be used to define which cells are valid, and
  which are NAs.

- n_per_presence:

  number of pseudoabsence points to sample for each presence

- coords:

  a vector of length two giving the names of the "x" and "y"
  coordinates, as found in `data`. If left to NULL, the function will
  try to guess the columns based on standard names `c("x", "y")`,
  `c("X","Y")`, `c("longitude", "latitude")`, or `c("lon", "lat")`

- time_col:

  The name of the column with time; if time is not a lubridate object,
  use `lubridate_fun` to provide a function that can be used to convert
  appropriately

- lubridate_fun:

  function to convert the time column into a lubridate object

- method:

  sampling method. One of 'random', 'dist_min', 'dist_max', or
  'dist_disc'.

- class_label:

  the label given to the sampled points. Defaults to `pseudoabs`

- return_pres:

  return presences together with pseudoabsences in a single tibble

- time_buffer:

  the buffer on the time axis around presences that defines their effect
  when sampling pseudoabsences. If set to zero, presences have an effect
  only on the time step to which they are assigned in `raster`; if a
  positive value, it defines the number of days before and after the
  date provided in the `time` column for which the presence should be
  considered (e.g. 20 days means that a presence is considered in all
  time steps equivalent to plus and minus twenty days from its date).

## Value

An object of class
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html). If
presences are returned, the presence level is set as the reference (to
match the expectations in the `yardstick` package that considers the
first level to be the event)

## Details

\#' @details Note that the time axis of the raster should be in
`POSIXct` or `Date` format, or use \`tstep="years"'. See
[`terra::time()`](https://rspatial.github.io/terra/reference/time.html)
for details on how to set the time axis.
