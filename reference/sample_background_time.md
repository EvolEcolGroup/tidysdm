# Sample background points for SDM analysis for points with a time point.

This function samples background points from a raster given a set of
presences. The locations returned as the center points of the sampled
cells, which can overlap with the presences (in contrast to
pseudo-absences, see
[sample_pseudoabs_time](https://evolecolgroup.github.io/tidysdm/reference/sample_pseudoabs_time.md)).
The following methods are implemented:

- 'random': background points randomly sampled from the region covered
  by the raster (i.e. not NAs).

- 'dist_max': background points randomly sampled from the unioned
  buffers of 'dist_max' from presences (distances in 'm' for lonlat
  rasters, and in map units for projected rasters). Using the union of
  buffers means that areas that are in multiple buffers are not
  oversampled. This is also referred to as "thickening".

- 'bias': background points are sampled according to a surface
  representing the biased sampling effort. Note that the surface for
  each time step is normalised to sum to 1;use `n_per_time_step` to
  affect sampling effort within each time step.

## Usage

``` r
sample_background_time(
  data,
  raster,
  n_per_time_step,
  coords = NULL,
  time_col = "time",
  lubridate_fun = c,
  method = "random",
  class_label = "background",
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

- n_per_time_step:

  number of background points to sample for each time step (i.e. a
  vector of length equal to the number of time steps in raster)

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

  sampling method. One of 'random', 'dist_max', or 'bias'.

- class_label:

  the label given to the sampled points. Defaults to `background`

- return_pres:

  return presences together with background in a single tibble

- time_buffer:

  the buffer on the time axis around presences that defines their effect
  when sampling background with method 'max_dist'. If set to zero,
  presences have an effect only on the time step to which they are
  assigned in `raster`; if a positive value, it defines the number of
  days before and after the date provided in the `time` column for which
  the presence should be considered (e.g. 20 days means that a presence
  is considered in all time steps equivalent to plus and minus twenty
  days from its date).

## Value

An object of class
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html). If
presences are returned, the presence level is set as the reference (to
match the expectations in the `yardstick` package that considers the
first level to be the event)

## Details

Note that the time axis of the raster should be in `POSIXct` or `Date`
format, or use \`tstep="years"'. See
[`terra::time()`](https://rspatial.github.io/terra/reference/time.html)
for details on how to set the time axis.
