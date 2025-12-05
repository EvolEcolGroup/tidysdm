# Repeat ensemble

An ensemble based multiple sets of pseudoabsences/background. This
object is a collection (list) of
[`simple_ensemble`](https://evolecolgroup.github.io/tidysdm/dev/reference/simple_ensemble.md)
objects for which predictions will be combined in a simple way (e.g. by
taking either the mean or median). Each
[`simple_ensemble`](https://evolecolgroup.github.io/tidysdm/dev/reference/simple_ensemble.md)
contains the best version of a each given model type following turning;
all simple ensembles will need to have the same metric estimated during
the cross-validation process.

## Usage

``` r
repeat_ensemble(...)
```

## Arguments

- ...:

  not used, this function just creates an empty `repeat_ensemble`
  object. Members are added with `add_best_candidates()`

## Value

an empty `repeat_ensemble`
