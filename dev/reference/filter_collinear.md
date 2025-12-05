# Filter to retain only variables that have low collinearity

This method finds a subset of variables that have low collinearity. It
provides three methods: `cor_caret`, a stepwise approach to remove
variables with a pairwise correlation above a given cutoff, choosing the
variable with the greatest mean correlation (based on the algorithm in
`caret::findCorrelation`); `vif_step`, a stepwise approach to remove
variables with an variance inflation factor above a given cutoff (based
on the algorithm in `usdm::vifstep`), and `vif_cor`, a stepwise approach
that, at each step, find the pair of variables with the highest
correlation above the cutoff and removes the one with the largest vif.
such that all have a correlation below a certain cutoff. There are
methods for
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
[`data.frame`](https://rdrr.io/r/base/data.frame.html) and
[`matrix`](https://rdrr.io/r/base/matrix.html). For
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
and `data.frame`, only numeric variables will be considered.

## Usage

``` r
filter_collinear(
  x,
  cutoff = NULL,
  verbose = FALSE,
  names = TRUE,
  to_keep = NULL,
  method = "cor_caret",
  cor_type = "pearson",
  max_cells = Inf,
  ...
)

# Default S3 method
filter_collinear(
  x,
  cutoff = NULL,
  verbose = FALSE,
  names = TRUE,
  to_keep = NULL,
  method = "cor_caret",
  cor_type = "pearson",
  max_cells = Inf,
  ...
)

# S3 method for class 'stars'
filter_collinear(
  x,
  cutoff = NULL,
  verbose = FALSE,
  names = TRUE,
  to_keep = NULL,
  method = "cor_caret",
  cor_type = "pearson",
  max_cells = Inf,
  exhaustive = FALSE,
  ...
)

# S3 method for class 'SpatRaster'
filter_collinear(
  x,
  cutoff = NULL,
  verbose = FALSE,
  names = TRUE,
  to_keep = NULL,
  method = "cor_caret",
  cor_type = "pearson",
  max_cells = Inf,
  exhaustive = FALSE,
  ...
)

# S3 method for class 'data.frame'
filter_collinear(
  x,
  cutoff = NULL,
  verbose = FALSE,
  names = TRUE,
  to_keep = NULL,
  method = "cor_caret",
  cor_type = "pearson",
  max_cells = Inf,
  ...
)

# S3 method for class 'matrix'
filter_collinear(
  x,
  cutoff = NULL,
  verbose = FALSE,
  names = TRUE,
  to_keep = NULL,
  method = "cor_caret",
  cor_type = "pearson",
  max_cells = Inf,
  ...
)
```

## Arguments

- x:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or `stars` object, a data.frame (with only numeric variables)

- cutoff:

  A numeric value used as a threshold to remove variables. For,
  "cor_caret" and "vif_cor", it is the pair-wise absolute correlation
  cutoff, which defaults to 0.7. For "vif_step", it is the variable
  inflation factor, which defaults to 10

- verbose:

  A boolean whether additional information should be provided on the
  screen

- names:

  a logical; should the column names be returned `TRUE` or the column
  index `FALSE`)?

- to_keep:

  A vector of variable names that we want to force in the set (note that
  the function will return an error if the correlation among any of
  those variables is higher than the cutoff).

- method:

  character. One of "cor_caret", "vif_cor" or "vif_step".

- cor_type:

  character. For methods that use correlation, which type of
  correlation: "pearson", "kendall", or "spearman". Defaults to
  "pearson"

- max_cells:

  positive integer. The maximum number of cells to be used. If this is
  smaller than ncell(x), a regular sample of x is used

- ...:

  additional arguments specific to a given object type

- exhaustive:

  boolean. Used only for
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  when downsampling to `max_cells`, if we require the `exhaustive`
  approach in
  [`terra::spatSample()`](https://rspatial.github.io/terra/reference/sample.html).
  This is only needed for rasters that are very sparse and not too
  large, see the help page of
  [`terra::spatSample()`](https://rspatial.github.io/terra/reference/sample.html)
  for details.

## Value

A vector of names of columns that are below the correlation threshold
(when `names = TRUE`), otherwise a vector of indices. Note that the
indices are only for numeric variables (i.e. if factors are present, the
indices do not take them into account).

## References

Naimi, B., Hamm, N.A.S., Groen, T.A., Skidmore, A.K., and Toxopeus, A.G.
2014. Where is positional uncertainty a problem for species distribution
modelling?, Ecography 37 (2): 191-203.

## Author

for `cor_caret`: Original R code by Dong Li, modified by Max Kuhn and
Andrea Manica; for `vif_step` and `vif_cor`, original algorithm by Babak
Naimi, rewritten by Andrea Manica for `tidysdm`
