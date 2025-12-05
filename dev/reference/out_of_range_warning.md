# Warn if some times are outside the range of time steps from a raster

This function helps making sure that, when we assign times to time_step
layers of a raster, we do not have values which are badly out of range

## Usage

``` r
out_of_range_warning(times, time_steps)
```

## Arguments

- times:

  the times of the locations

- time_steps:

  the time steps from the raster

## Value

NULL return
