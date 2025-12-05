# Deprecated: Filter to retain only variables below a given correlation threshold

THIS FUNCTION IS DEPRECATED. USE `filter_collinear` with
`method=cor_caret` instead

## Usage

``` r
filter_high_cor(x, cutoff = 0.7, verbose = FALSE, names = TRUE, to_keep = NULL)

# Default S3 method
filter_high_cor(x, cutoff = 0.7, verbose = FALSE, names = TRUE, to_keep = NULL)

# S3 method for class 'SpatRaster'
filter_high_cor(x, cutoff = 0.7, verbose = FALSE, names = TRUE, to_keep = NULL)

# S3 method for class 'data.frame'
filter_high_cor(x, cutoff = 0.7, verbose = FALSE, names = TRUE, to_keep = NULL)

# S3 method for class 'matrix'
filter_high_cor(x, cutoff = 0.7, verbose = FALSE, names = TRUE, to_keep = NULL)
```

## Arguments

- x:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object, a data.frame (with only numeric variables), or a correlation
  matrix

- cutoff:

  A numeric value for the pair-wise absolute correlation cutoff

- verbose:

  A boolean for printing the details

- names:

  a logical; should the column names be returned `TRUE` or the column
  index `FALSE`)?

- to_keep:

  A vector of variable names that we want to force in the set (note that
  the function will return an error if the correlation among any of
  those variables is higher than the cutoff).

## Value

A vector of names of columns that are below the correlation threshold
(when `names = TRUE`), otherwise a vector of indices. Note that the
indices are only for numeric variables (i.e. if factors are present, the
indices do not take them into account).

## Details

This method finds a subset of variable such that all have a correlation
below a certain cutoff. There are methods for
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
[`data.frame`](https://rdrr.io/r/base/data.frame.html), and to work
directly on a correlation matrix that was previously estimated. For
`data.frame`, only numeric variables will be considered. The algorithm
is based on `caret::findCorrelation`, using the `exact` option. The
absolute values of pair-wise correlations are considered. If two
variables have a high correlation, the function looks at the mean
absolute correlation of each variable and removes the variable with the
largest mean absolute correlation.

There are several function in the package `subselect` that can also be
used to accomplish the same goal but tend to retain more predictors.
