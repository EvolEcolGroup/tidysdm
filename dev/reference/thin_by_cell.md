# Thin point dataset to have 1 observation per raster cell

This function thins a dataset so that only one observation per cell is
retained.

## Usage

``` r
thin_by_cell(data, raster, coords = NULL, drop_na = TRUE, agg_fact = NULL)
```

## Arguments

- data:

  An [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) data
  frame, or a data frame with coordinate variables. These can be defined
  in `coords`, unless they have standard names (see details below).

- raster:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or `stars` object that defined the grid

- coords:

  a vector of length two giving the names of the "x" and "y"
  coordinates, as found in `data`. If left to NULL, the function will
  try to guess the columns based on standard names `c("x", "y")`,
  `c("X","Y")`, `c("longitude", "latitude")`, or `c("lon", "lat")`

- drop_na:

  boolean on whether locations that are NA in the raster should be
  dropped.

- agg_fact:

  positive integer. Aggregation factor expressed as number of cells in
  each direction (horizontally and vertically). Or two integers
  (horizontal and vertical aggregation factor) or three integers (when
  also aggregating over layers). Defaults to NULL, which implies no
  aggregation (i.e. thinning is done on the grid of `raster`)

## Value

An object of class
[`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) or
[`data.frame`](https://rdrr.io/r/base/data.frame.html), the same as
"data".

## Details

Further thinning can be achieved by aggregating cells in the raster
before thinning, as achieved by setting `agg_fact` \> 1 (aggregation
works in a manner equivalent to
[`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)).
Note that if `data` is an `sf` object, the function will transform the
coordinates to the same projection as the `raster` (recommended); if
`data` is a data.frame, it is up to the user to ensure that the
coordinates are in the correct units.
