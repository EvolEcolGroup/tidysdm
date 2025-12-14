# Model specification for a MaxEnt for SDM

This function returns a
[parsnip::model_spec](https://parsnip.tidymodels.org/reference/model_spec.html)
for a MaxEnt model to be used in Species Distribution Models.

## Usage

``` r
sdm_spec_maxent(..., tune = c("sdm", "all", "custom", "none"))
```

## Arguments

- ...:

  parameters to be passed to
  [`maxent()`](https://evolecolgroup.github.io/tidysdm/reference/maxent.md)
  to customise the model. See the help of that function for details.

- tune:

  character defining the tuning strategy. Valid strategies are:

  - "sdm" chooses hyper-parameters that are most important to tune for
    an sdm (for *maxent*, 'feature_classes' and
    'regularization_multiplier')

  - "all" tunes all hyperparameters (for *maxent*, 'feature_classes' and
    'regularization_multiplier', the same as with tune = "sdm")

  - "custom" passes the options from '...'

  - "none" does not tune any hyperparameter

## Value

a
[parsnip::model_spec](https://parsnip.tidymodels.org/reference/model_spec.html)
of the model.

## See also

Other "sdm model specifications":
[`sdm_spec_boost_tree()`](https://evolecolgroup.github.io/tidysdm/reference/sdm_spec_boost_tree.md),
[`sdm_spec_gam()`](https://evolecolgroup.github.io/tidysdm/reference/sdm_spec_gam.md),
[`sdm_spec_glm()`](https://evolecolgroup.github.io/tidysdm/reference/sdm_spec_glm.md),
[`sdm_spec_rand_forest()`](https://evolecolgroup.github.io/tidysdm/reference/sdm_spec_rand_forest.md)

## Examples

``` r
test_maxent_spec <- sdm_spec_maxent(tune = "sdm")
test_maxent_spec
#> maxent Model Specification (classification)
#> 
#> Main Arguments:
#>   feature_classes = tune::tune()
#>   regularization_multiplier = tune()
#> 
#> Computational engine: maxnet 
#> 
# setting specific values
sdm_spec_maxent(tune = "custom", feature_classes = "lq")
#> maxent Model Specification (classification)
#> 
#> Main Arguments:
#>   feature_classes = lq
#> 
#> Computational engine: maxnet 
#> 
```
