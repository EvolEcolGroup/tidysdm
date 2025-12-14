# Maximum Cohen's Kappa

Cohen's Kappa
([`yardstick::kap()`](https://yardstick.tidymodels.org/reference/kap.html))
is a measure similar to
[`yardstick::accuracy()`](https://yardstick.tidymodels.org/reference/accuracy.html),
but it normalises the observed accuracy by the value that would be
expected by chance (this helps for unbalanced cases when one class is
predominant).

## Usage

``` r
kap_max(data, ...)

# S3 method for class 'data.frame'
kap_max(
  data,
  truth,
  ...,
  estimator = NULL,
  na_rm = TRUE,
  event_level = "first",
  case_weights = NULL
)

# S3 method for class 'sf'
kap_max(data, ...)

kap_max_vec(
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  event_level = "first",
  case_weights = NULL,
  ...
)
```

## Arguments

- data:

  Either a data.frame containing the columns specified by the truth and
  estimate arguments, or a table/matrix where the true class results
  should be in the columns of the table.

- ...:

  A set of unquoted column names or one or more dplyr selector functions
  to choose which variables contain the class probabilities. If truth is
  binary, only 1 column should be selected, and it should correspond to
  the value of event_level. Otherwise, there should be as many columns
  as factor levels of truth and the ordering of the columns should be
  the same as the factor levels of truth.

- truth:

  The column identifier for the true class results (that is a factor).
  This should be an unquoted column name although this argument is
  passed by expression and supports quasiquotation (you can unquote
  column names). For \_vec() functions, a factor vector.

- estimator:

  One of "binary", "hand_till", "macro", or "macro_weighted" to specify
  the type of averaging to be done. "binary" is only relevant for the
  two class case. The others are general methods for calculating
  multiclass metrics. The default will automatically choose "binary" if
  truth is binary, "hand_till" if truth has \>2 levels and case_weights
  isn't specified, or "macro" if truth has \>2 levels and case_weights
  is specified (in which case "hand_till" isn't well-defined).

- na_rm:

  A logical value indicating whether NA values should be stripped before
  the computation proceeds.

- event_level:

  A single string. Either "first" or "second" to specify which level of
  truth to consider as the "event". This argument is only applicable
  when estimator = "binary". The default uses an internal helper that
  generally defaults to "first"

- case_weights:

  The optional column identifier for case weights. This should be an
  unquoted column name that evaluates to a numeric column in data. For
  \_vec() functions, a numeric vector.

- estimate:

  If truth is binary, a numeric vector of class probabilities
  corresponding to the "relevant" class. Otherwise, a matrix with as
  many columns as factor levels of truth. It is assumed that these are
  in the same order as the levels of truth.

## Value

A tibble with columns .metric, .estimator, and .estimate and 1 row of
values. For grouped data frames, the number of rows returned will be the
same as the number of groups.

## Details

This function calibrates the probability threshold to classify presences
to maximises kappa.

There is no multiclass version of this function, it only operates on
binary predictions (e.g. presences and absences in SDMs).

## References

Cohen, J. (1960). "A coefficient of agreement for nominal scales".
*Educational and Psychological Measurement*. 20 (1): 37-46.

Cohen, J. (1968). "Weighted kappa: Nominal scale agreement provision for
scaled disagreement or partial credit". *Psychological Bulletin*. 70
(4): 213-220.

## See also

Other class probability metrics:
[`boyce_cont()`](https://evolecolgroup.github.io/tidysdm/reference/boyce_cont.md),
[`tss_max()`](https://evolecolgroup.github.io/tidysdm/reference/tss_max.md)

## Examples

``` r
kap_max(two_class_example, truth, Class1)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 kap_max binary         0.725
```
