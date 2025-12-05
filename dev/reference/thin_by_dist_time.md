# Thin points dataset based on geographic and temporal distance

This function thins a dataset so that only observations that have a
distance from each other greater than "dist_min" in space and
"interval_min" in time are retained.

## Usage

``` r
thin_by_dist_time(
  data,
  dist_min,
  interval_min,
  coords = NULL,
  time_col = "time",
  lubridate_fun = c,
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

- interval_min:

  Minimum time interval between points, in days.

- coords:

  A vector of length two giving the names of the "x" and "y"
  coordinates, as found in `data`. If left to NULL, the function will
  try to guess the columns based on standard names `c("x", "y")`,
  `c("X","Y")`, `c("longitude", "latitude")`, or `c("lon", "lat")`

- time_col:

  The name of the column with time; if time is not a lubridate object,
  use `lubridate_fun` to provide a function that can be used to convert
  appropriately

- lubridate_fun:

  function to convert the time column into a lubridate object

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

Geographic distances are measured in the appropriate units for the
projection used. In case of raw latitude and longitude (e.g. as provided
in a data.frame), the crs is set to WGS84, and units are set to meters.
Time interval are estimated in days. Note that for very long time
period, the simple conversion x years = 365 \* x days might lead to
slightly shorter intervals than expected, as it ignores leap years. The
function
[`y2d()`](https://evolecolgroup.github.io/tidysdm/dev/reference/y2d.md)
provides a closer approximation.

This function an algorithm analogous to `spThin`, with the exception
that neighbours are defined in terms of both space and time.
