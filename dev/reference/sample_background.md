# Sample background points for SDM analysis

This function samples background points from a raster given a set of
presences. The locations returned as the center points of the sampled
cells, which can overlap with the presences (in contrast to
pseudo-absences, see
[sample_pseudoabs](https://evolecolgroup.github.io/tidysdm/dev/reference/sample_pseudoabs.md)).
The following methods are implemented:

- 'random': background randomly sampled from the region covered by the
  raster (i.e. not NAs).

- 'dist_max': background randomly sampled from the unioned buffers of
  'dist_max' from presences (distances in 'm' for lonlat rasters, and in
  map units for projected rasters). Using the union of buffers means
  that areas that are in multiple buffers are not oversampled. This is
  also referred to as "thickening".

- 'bias': background points are sampled according to a surface
  representing the biased sampling effort.

## Usage

``` r
sample_background(
  data,
  raster,
  n,
  coords = NULL,
  method = "random",
  class_label = "background",
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
  or `stars` from which cells will be sampled (the first layer will be
  used to determine which cells are NAs, and thus can not be sampled).
  If sampling is "bias", then the sampling probability will be
  proportional to the values on the first layer (i.e. band) of the
  raster.

- n:

  number of background points to sample.

- coords:

  a vector of length two giving the names of the "x" and "y"
  coordinates, as found in `data`. If left to NULL, the function will
  try to guess the columns based on standard names `c("x", "y")`,
  `c("X","Y")`, `c("longitude", "latitude")`, or `c("lon", "lat")`.

- method:

  sampling method. One of 'random', 'dist_max', and 'bias'. For
  dist_max, the maximum distance is set as an additional element of a
  vector, e.g c('dist_max',70000).

- class_label:

  the label given to the sampled points. Defaults to `background`

- return_pres:

  return presences together with background in a single tibble.

## Value

An object of class
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html). If
presences are returned, the presence level is set as the reference (to
match the expectations in the `yardstick` package that considers the
first level to be the event).

## Details

Note that the units of distance depend on the projection of the raster.
