# Find threshold that optimises a given metric

This function returns the threshold to turn probabilities into binary
classes whilst optimising a given metric. Currently available for
[`tss_max`](https://evolecolgroup.github.io/tidysdm/dev/reference/tss_max.md),
[`kap_max`](https://evolecolgroup.github.io/tidysdm/dev/reference/kap_max.md)
and `sensitivity` (for which a target sensitivity is required).

## Usage

``` r
optim_thresh(truth, estimate, metric, event_level = "first")
```

## Arguments

- truth:

  The column identifier for the true class results (that is a factor).
  This should be an unquoted column name although this argument is
  passed by expression and supports quasiquotation (you can unquote
  column names). For \_vec() functions, a factor vector.

- estimate:

  the predicted probability for the event

- metric:

  character of metric to be optimised. Currently only "tss_max",
  "kap_max", and "sensitivity" with a given target (e.g.
  c("sensitivity",0.8))

- event_level:

  A single string. Either "first" or "second" to specify which level of
  truth to consider as the "event". This argument is only applicable
  when estimator = "binary". The default uses an internal helper that
  generally defaults to "first"

## Value

the probability threshold for the event

## Examples

``` r
optim_thresh(two_class_example$truth, two_class_example$Class1,
  metric = c("tss_max")
)
#> [1] 0.7544818
optim_thresh(two_class_example$truth, two_class_example$Class1,
  metric = c("sens", 0.9)
)
#> [1] 0.3710924
```
