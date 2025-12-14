# Probability metrics for `sf` objects

`tidysdm` provides specialised metrics for SDMs, which have their own
help
pages([`boyce_cont()`](https://evolecolgroup.github.io/tidysdm/reference/boyce_cont.md),
[`kap_max()`](https://evolecolgroup.github.io/tidysdm/reference/kap_max.md),
and
[`tss_max()`](https://evolecolgroup.github.io/tidysdm/reference/tss_max.md)).
Additionally, it also provides methods to handle
[sf::sf](https://r-spatial.github.io/sf/reference/sf.html) objects for
the following standard `yardstick` metrics:

[`yardstick::average_precision()`](https://yardstick.tidymodels.org/reference/average_precision.html)

[`yardstick::brier_class()`](https://yardstick.tidymodels.org/reference/brier_class.html)

[`yardstick::classification_cost()`](https://yardstick.tidymodels.org/reference/classification_cost.html)

[`yardstick::gain_capture()`](https://yardstick.tidymodels.org/reference/gain_capture.html)

[`yardstick::mn_log_loss()`](https://yardstick.tidymodels.org/reference/mn_log_loss.html)

[`yardstick::pr_auc()`](https://yardstick.tidymodels.org/reference/pr_auc.html)

[`yardstick::roc_auc()`](https://yardstick.tidymodels.org/reference/roc_auc.html)

[`yardstick::roc_aunp()`](https://yardstick.tidymodels.org/reference/roc_aunp.html)

[`yardstick::roc_aunu()`](https://yardstick.tidymodels.org/reference/roc_aunu.html)

## Usage

``` r
# S3 method for class 'sf'
average_precision(data, ...)

# S3 method for class 'sf'
brier_class(data, ...)

# S3 method for class 'sf'
classification_cost(data, ...)

# S3 method for class 'sf'
gain_capture(data, ...)

# S3 method for class 'sf'
mn_log_loss(data, ...)

# S3 method for class 'sf'
pr_auc(data, ...)

# S3 method for class 'sf'
roc_auc(data, ...)

# S3 method for class 'sf'
roc_aunp(data, ...)

# S3 method for class 'sf'
roc_aunu(data, ...)
```

## Arguments

- data:

  an [sf::sf](https://r-spatial.github.io/sf/reference/sf.html) object

- ...:

  any other parameters to pass to the `data.frame` version of the
  metric. See the specific man page for the metric of interest.

## Value

A tibble with columns `.metric`, `.estimator`, and `.estimate` and 1 row
of values.

## Details

Note that `roc_aunp` and `roc_aunu` are multiclass metrics, and as such
are are not relevant for SDMs (which work on a binary response). They
are included for completeness, so that all class probability metrics
from `yardstick` have an `sf` method, for applications other than SDMs.
