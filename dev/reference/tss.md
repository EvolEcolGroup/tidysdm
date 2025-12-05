# TSS - True Skill Statistics

The True Skills Statistic, which is defined as

## Usage

``` r
tss(data, ...)

# S3 method for class 'data.frame'
tss(
  data,
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = "first",
  ...
)
```

## Arguments

- data:

  Either a data.frame containing the columns specified by the truth and
  estimate arguments, or a table/matrix where the true class results
  should be in the columns of the table.

- ...:

  Not currently used.

- truth:

  The column identifier for the true class results (that is a factor).
  This should be an unquoted column name although this argument is
  passed by expression and supports quasiquotation (you can unquote
  column names). For \_vec() functions, a factor vector.

- estimate:

  The column identifier for the predicted class results (that is also
  factor). As with truth this can be specified different ways but the
  primary method is to use an unquoted variable name. For \_vec()
  functions, a factor vector.

- estimator:

  One of: "binary", "macro", "macro_weighted", or "micro" to specify the
  type of averaging to be done. "binary" is only relevant for the two
  class case. The other three are general methods for calculating
  multiclass metrics. The default will automatically choose "binary" or
  "macro" based on estimate.

- na_rm:

  A logical value indicating whether NA values should be stripped before
  the computation proceeds.

- case_weights:

  The optional column identifier for case weights. This should be an
  unquoted column name that evaluates to a numeric column in data. For
  \_vec() functions, a numeric vector.

- event_level:

  A single string. Either "first" or "second" to specify which level of
  truth to consider as the "event". This argument is only applicable
  when estimator = "binary". The default is "first".

## Value

A tibble with columns .metric, .estimator, and .estimate and 1 row of
values. For grouped data frames, the number of rows returned will be the
same as the number of groups.

## Details

*sensitivity*+*specificity* +1

This function is a wrapper around
[`yardstick::j_index()`](https://yardstick.tidymodels.org/reference/j_index.html),
another name for the same quantity. Note that this function takes the
classes as predicted by the model without any calibration (i.e. making a
split at 0.5 probability). This is usually not the metric used for
Species Distribution Models, where the threshold is recalibrated to
maximise TSS; for that purpose, use
[`tss_max()`](https://evolecolgroup.github.io/tidysdm/dev/reference/tss_max.md).

## Examples

``` r
# Two class
data("two_class_example")
tss(two_class_example, truth, predicted)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 tss     binary         0.673
# Multiclass
library(dplyr)
data(hpc_cv)
# Groups are respected
hpc_cv %>%
  group_by(Resample) %>%
  tss(obs, pred)
#> # A tibble: 10 × 4
#>    Resample .metric .estimator .estimate
#>    <chr>    <chr>   <chr>          <dbl>
#>  1 Fold01   tss     macro          0.434
#>  2 Fold02   tss     macro          0.422
#>  3 Fold03   tss     macro          0.533
#>  4 Fold04   tss     macro          0.449
#>  5 Fold05   tss     macro          0.431
#>  6 Fold06   tss     macro          0.413
#>  7 Fold07   tss     macro          0.398
#>  8 Fold08   tss     macro          0.468
#>  9 Fold09   tss     macro          0.435
#> 10 Fold10   tss     macro          0.412
```
