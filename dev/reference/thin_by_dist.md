# Thin points dataset based on geographic distance

This function thins a dataset so that only observations that have a
distance from each other greater than "dist_min" are retained.

## Usage

``` r
thin_by_dist(
  data,
  dist_min,
  coords = NULL,
  dist_method = c("great_circle", "euclidean")
)
```

## Arguments

- data:

  An [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) data
  frame, or a data frame with coordinate variables. These can be defined
  in `coords`, unless they have standard names (see details below).

- dist_min:

  Minimum distance between points (in units appropriate for the
  projection, or meters for lonlat data).

- coords:

  A vector of length two giving the names of the "x" and "y"
  coordinates, as found in `data`. If left to NULL, the function will
  try to guess the columns based on standard names `c("x", "y")`,
  `c("X","Y")`, `c("longitude", "latitude")`, or `c("lon", "lat")`

- dist_method:

  method to compute distance, either "euclidean" or "great_circle".
  Defaults to "great_circle", which is more accurate but takes slightly
  longer.

## Value

An object of class
[`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) or
[`data.frame`](https://rdrr.io/r/base/data.frame.html), the same as
"data".

## Details

Distances are measured in the appropriate units for the projection used.
In case of raw latitude and longitude (e.g. as provided in a
data.frame), the crs is set to WGS84, and units are set to meters.

This function is a modified version of the algorithm in `spThin`,
adapted to work on `sf` objects.
