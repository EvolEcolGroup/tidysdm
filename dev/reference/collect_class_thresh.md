# Obtain and format the class thresholds for ensemble objects

Return a tibble of class thresholds, as computed by
[`calib_class_thresh()`](https://evolecolgroup.github.io/tidysdm/dev/reference/calib_class_thresh.md).

## Usage

``` r
collect_class_thresh(x, ...)
```

## Arguments

- x:

  A
  [`simple_ensemble`](https://evolecolgroup.github.io/tidysdm/dev/reference/simple_ensemble.md)

- ...:

  Not currently used.

## Value

A tibble.

## Examples

``` r
test_ens <- simple_ensemble() %>%
  add_member(two_class_res[1:3, ], metric = "roc_auc")
test_ens <- calib_class_thresh(test_ens, class_thresh = "tss_max")
test_ens <- calib_class_thresh(test_ens, class_thresh = "kap_max")
collect_class_thresh(test_ens)
#> # A tibble: 8 × 4
#>   class_thresh metric_thresh fun             optim_value
#>   <list>       <list>        <chr>                 <dbl>
#> 1 <chr [1]>    <NULL>        mean                  0.544
#> 2 <chr [1]>    <NULL>        median                0.545
#> 3 <chr [1]>    <NULL>        weighted_mean         0.544
#> 4 <chr [1]>    <NULL>        weighted_median       0.447
#> 5 <chr [1]>    <NULL>        mean                  0.544
#> 6 <chr [1]>    <NULL>        median                0.451
#> 7 <chr [1]>    <NULL>        weighted_mean         0.544
#> 8 <chr [1]>    <NULL>        weighted_median       0.447
```
