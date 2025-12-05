# Create a ggplot for a spatial initial rsplit.

This method provides a good visualization method for a spatial initial
rsplit.

## Usage

``` r
# S3 method for class 'spatial_initial_split'
autoplot(object, ..., alpha = 0.6)
```

## Arguments

- object:

  A `spatial_initial_rsplit` object. Note that only resamples made from
  `sf` objects create `spatial_initial_rsplit` objects; this function
  will not work for resamples made with non-spatial tibbles or
  data.frames.

- ...:

  Options passed to
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

- alpha:

  Opacity, passed to
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).
  Values of alpha range from 0 to 1, with lower values corresponding to
  more transparent colors.

## Value

A ggplot object with each fold assigned a color, made using
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

## Details

This plot method is a wrapper around the standard `spatial_rsplit`
method, but it re-labels the folds as *Testing* and *Training* following
the convention for a standard `initial_split` object

## Examples

``` r
set.seed(123)
block_initial <- spatial_initial_split(boston_canopy,
  prop = 1 / 5, spatial_block_cv
)
autoplot(block_initial)
```
