# Parameters for maxent models

These parameters are auxiliary to MaxEnt models using the "maxnet"
engine. These functions are used by the tuning functions, and the user
will rarely access them directly.

## Usage

``` r
regularization_multiplier(range = c(0.5, 3), trans = NULL)

feature_classes(values = c("l", "lq", "lqp", "lqph", "lqpht"))
```

## Arguments

- range:

  A two-element vector holding the defaults for the smallest and largest
  possible values, respectively. If a transformation is specified, these
  values should be in the transformed units.

- trans:

  A trans object from the scales package, such as scales::log10_trans()
  or scales::reciprocal_trans(). If not provided, the default is used
  which matches the units used in range. If no transformation, NULL.

- values:

  For `feature_classes()`, a character string of any subset of "lqpht"
  (for example, "lh")

## Value

a `param` object that can be used for tuning.

## Examples

``` r
regularization_multiplier()
#> Reg. multiplier (quantitative)
#> Range: [0.5, 3]
feature_classes()
#> Feature classes (qualitative)
#> 5 possible values include:
#> 'l', 'lq', 'lqp', 'lqph', and 'lqpht'
```
