# Plot the results of a repeat ensemble

This `autoplot()` method plots performance metrics for each repeat,
ranked using one of the metrics.

## Usage

``` r
# S3 method for class 'repeat_ensemble'
autoplot(object, ...)
```

## Arguments

- object:

  A
  [`repeat_ensemble`](https://evolecolgroup.github.io/tidysdm/dev/reference/repeat_ensemble.md)
  whose elements have results.

- ...:

  Other options to pass to `autoplot()`. Currently unused.

## Value

A ggplot object.

## Examples

``` r
autoplot(lacerta_rep_ens)

```
