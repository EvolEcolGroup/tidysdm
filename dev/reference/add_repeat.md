# Add repeat(s) to a repeated ensemble

This function adds repeat(s) to a
[`repeat_ensemble`](https://evolecolgroup.github.io/tidysdm/dev/reference/repeat_ensemble.md)
object, where each repeat is a
[`simple_ensemble`](https://evolecolgroup.github.io/tidysdm/dev/reference/simple_ensemble.md).
All repeats must contain the same members, selected using the same
metric.

## Usage

``` r
add_repeat(x, rep, ...)

# Default S3 method
add_repeat(x, rep, ...)

# S3 method for class 'simple_ensemble'
add_repeat(x, rep, ...)

# S3 method for class 'list'
add_repeat(x, rep, ...)
```

## Arguments

- x:

  a
  [repeat_ensemble](https://evolecolgroup.github.io/tidysdm/dev/reference/repeat_ensemble.md)
  to which repeat(s) will be added

- rep:

  a repeat, as a single
  [`simple_ensemble`](https://evolecolgroup.github.io/tidysdm/dev/reference/simple_ensemble.md),
  or a list of
  [`simple_ensemble`](https://evolecolgroup.github.io/tidysdm/dev/reference/simple_ensemble.md)
  objects

- ...:

  not used at the moment.

## Value

a
[repeat_ensemble](https://evolecolgroup.github.io/tidysdm/dev/reference/repeat_ensemble.md)
with additional repeat(s)
