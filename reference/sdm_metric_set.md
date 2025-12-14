# Metric set for SDM

This function returns a
[yardstick::metric_set](https://yardstick.tidymodels.org/reference/metric_set.html)
that includes
[`boyce_cont()`](https://evolecolgroup.github.io/tidysdm/reference/boyce_cont.md),
[`yardstick::roc_auc()`](https://yardstick.tidymodels.org/reference/roc_auc.html)
and
[`tss_max()`](https://evolecolgroup.github.io/tidysdm/reference/tss_max.md),
the most commonly used metrics for SDM.

## Usage

``` r
sdm_metric_set(...)
```

## Arguments

- ...:

  additional metrics to be added to the
  [`yardstick::metric_set`](https://yardstick.tidymodels.org/reference/metric_set.html).
  See the help to
  [`yardstick::metric_set()`](https://yardstick.tidymodels.org/reference/metric_set.html)
  for constraints on the type of metrics that can be mixed.

## Value

a
[`yardstick::metric_set`](https://yardstick.tidymodels.org/reference/metric_set.html)
object.

## Examples

``` r
sdm_metric_set()
#> A metric set, consisting of:
#> - `boyce_cont()`, a probability metric | direction: maximize
#> - `roc_auc()`, a probability metric    | direction: maximize
#> - `tss_max()`, a probability metric    | direction: maximize
sdm_metric_set(accuracy)
#> A metric set, consisting of:
#> - `boyce_cont()`, a probability metric | direction: maximize
#> - `roc_auc()`, a probability metric    | direction: maximize
#> - `tss_max()`, a probability metric    | direction: maximize
#> - `accuracy()`, a class metric         | direction: maximize
```
