# Obtain and format results produced by tuning functions for ensemble objects

Return a tibble of performance metrics for all models.

## Usage

``` r
# S3 method for class 'simple_ensemble'
collect_metrics(x, ...)

# S3 method for class 'repeat_ensemble'
collect_metrics(x, ...)
```

## Arguments

- x:

  A
  [`simple_ensemble`](https://evolecolgroup.github.io/tidysdm/dev/reference/simple_ensemble.md)
  or
  [`repeat_ensemble`](https://evolecolgroup.github.io/tidysdm/dev/reference/repeat_ensemble.md)
  object

- ...:

  Not currently used.

## Value

A tibble.

## Details

When applied to a ensemble, the metrics that are returned do not contain
the actual tuning parameter columns and values (unlike when these
collect functions are run on other objects). The reason is that
ensembles contain different types of models or models with different
tuning parameters.

## See also

[`tune::collect_metrics()`](https://tune.tidymodels.org/reference/collect_predictions.html)

## Examples

``` r
collect_metrics(lacerta_ensemble)
#> # A tibble: 12 × 5
#>    wflow_id       .metric     mean std_err     n
#>    <chr>          <chr>      <dbl>   <dbl> <int>
#>  1 default_glm    boyce_cont 0.443  0.0899     5
#>  2 default_glm    roc_auc    0.785  0.0391     5
#>  3 default_glm    tss_max    0.556  0.0745     5
#>  4 default_rf     boyce_cont 0.600  0.0567     5
#>  5 default_rf     roc_auc    0.805  0.0449     5
#>  6 default_rf     tss_max    0.556  0.0856     5
#>  7 default_gbm    boyce_cont 0.516  0.0500     5
#>  8 default_gbm    roc_auc    0.800  0.0422     5
#>  9 default_gbm    tss_max    0.539  0.0844     5
#> 10 default_maxent boyce_cont 0.427  0.149      5
#> 11 default_maxent roc_auc    0.784  0.0402     5
#> 12 default_maxent tss_max    0.557  0.0785     5
collect_metrics(lacerta_rep_ens)
#> # A tibble: 18 × 6
#>    rep_id wflow_id       .metric     mean std_err     n
#>    <chr>  <chr>          <chr>      <dbl>   <dbl> <int>
#>  1 rep_01 default_glm    boyce_cont 0.699 0.113       5
#>  2 rep_01 default_glm    roc_auc    0.960 0.0123      5
#>  3 rep_01 default_glm    tss_max    0.859 0.0429      5
#>  4 rep_01 default_maxent boyce_cont 0.875 0.0342      5
#>  5 rep_01 default_maxent roc_auc    0.977 0.00606     5
#>  6 rep_01 default_maxent tss_max    0.881 0.0350      5
#>  7 rep_02 default_glm    boyce_cont 0.475 0.154       5
#>  8 rep_02 default_glm    roc_auc    0.906 0.0487      5
#>  9 rep_02 default_glm    tss_max    0.799 0.0688      5
#> 10 rep_02 default_maxent boyce_cont 0.825 0.0635      5
#> 11 rep_02 default_maxent roc_auc    0.960 0.0222      5
#> 12 rep_02 default_maxent tss_max    0.853 0.0667      5
#> 13 rep_03 default_glm    boyce_cont 0.840 0.0395      5
#> 14 rep_03 default_glm    roc_auc    0.938 0.0248      5
#> 15 rep_03 default_glm    tss_max    0.815 0.0565      5
#> 16 rep_03 default_maxent boyce_cont 0.765 0.101       5
#> 17 rep_03 default_maxent roc_auc    0.954 0.0148      5
#> 18 rep_03 default_maxent tss_max    0.826 0.0356      5
```
