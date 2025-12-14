# Clamp the predictors to match values in training set

This function clamps the environmental variables in a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
or
[`terra::SpatRasterDataset`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
so that their minimum and maximum values do not exceed the range in the
training dataset.

## Usage

``` r
clamp_predictors(x, training, .col, use_na)

# Default S3 method
clamp_predictors(x, training, .col, use_na)

# S3 method for class 'stars'
clamp_predictors(x, ...)

# S3 method for class 'SpatRaster'
clamp_predictors(x, training, .col, use_na = FALSE)

# S3 method for class 'SpatRasterDataset'
clamp_predictors(x, training, .col, use_na = FALSE)
```

## Arguments

- x:

  a
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  `stars` or
  [`terra::SpatRasterDataset`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  to clamp.

- training:

  the training dataset (a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html) or a
  [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) object.

- .col:

  the column containing the presences (optional). If specified, it is
  excluded from the clamping.

- use_na:

  a boolean determining whether values outside the range of the training
  dataset are removed (set to NA). If FALSE (the default), values
  outside the training range are replaced with the extremes of the
  training range.

- ...:

  additional arguments specific to a given object type

## Value

a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
or
[`terra::SpatRasterDataset`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
clamped to the ranges in `training`
