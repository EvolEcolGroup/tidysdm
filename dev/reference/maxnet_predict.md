# Wrapper to predict maxnet models

This function is a wrapper around the `predict` method for
[maxnet::maxnet](https://rdrr.io/pkg/maxnet/man/maxnet.html), making the
function compatible with `parsnip`. Users are unlikely to use this
function directly. For the `parsnip` model specification for MaxEnt, see
[`maxent()`](https://evolecolgroup.github.io/tidysdm/dev/reference/maxent.md).

## Usage

``` r
maxnet_predict(
  object,
  newdata,
  type = c("class", "prob"),
  maxnet_type = c("cloglog", "link", "exponential", "logistic"),
  clamp = TRUE
)
```

## Arguments

- object:

  the [maxnet::maxnet](https://rdrr.io/pkg/maxnet/man/maxnet.html)
  object

- newdata:

  the dataframe of new data

- type:

  either "prob" or "class"

- maxnet_type:

  the transformation used for the prediction

- clamp:

  logical, defining whether clamping to observed ranges should be used

## Value

a tibble of predictions
