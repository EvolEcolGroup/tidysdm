# Sample pseudo-absence points for SDM analysis

This function samples pseudo-absence points from a raster given a set of
presences. The locations returned as the center points of the sampled
cells, which can not overlap with the presences (in contrast to
background points, see
[sample_background](https://evolecolgroup.github.io/tidysdm/dev/reference/sample_background.md)).
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
sample_pseudoabs(
  data,
  raster,
  n,
  coords = NULL,
  method = "random",
  class_label = "pseudoabs",
  return_pres = TRUE
)
```

## Arguments

- data:

  An [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) data
  frame, or a data frame with coordinate variables. These can be defined
  in `coords`, unless they have standard names (see details below).

- raster:

  the
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or `stars` from which cells will be sampled

- n:

  number of pseudoabsence points to sample

- coords:

  a vector of length two giving the names of the "x" and "y"
  coordinates, as found in `data`. If left to NULL, the function will
  try to guess the columns based on standard names `c("x", "y")`,
  `c("X","Y")`, `c("longitude", "latitude")`, or `c("lon", "lat")`

- method:

  sampling method. One of 'random', 'dist_min', 'dist_max', or
  'dist_disc'. Threshold distances are set as additional elements of a
  vector, e.g c('dist_min',70000) or c('dist_disc',50000,200000).

- class_label:

  the label given to the sampled points. Defaults to `pseudoabs`

- return_pres:

  return presences together with pseudoabsences in a single tibble

## Value

An object of class
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html). If
presences are returned, the presence level is set as the reference (to
match the expectations in the `yardstick` package that considers the
first level to be the event)
