# Model specification for a Random Forest for SDM

This function returns a
[parsnip::model_spec](https://parsnip.tidymodels.org/reference/model_spec.html)
for a Random Forest to be used as a classifier of presences and absences
in Species Distribution Models. It uses the library `ranger` to fit
boosted trees; to use another library, simply build the
[parsnip::model_spec](https://parsnip.tidymodels.org/reference/model_spec.html)
directly.

## Usage

``` r
sdm_spec_rand_forest(..., tune = c("sdm", "all", "custom", "none"))

sdm_spec_rf(..., tune = c("sdm", "all", "custom", "none"))
```

## Arguments

- ...:

  parameters to be passed to
  [`parsnip::rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
  to customise the model. See the help of that function for details.

- tune:

  character defining the tuning strategy. Valid strategies are:

  - "sdm" chooses hyperparameters that are most important to tune for an
    sdm (for *rf*, 'mtry')

  - "all" tunes all hyperparameters (for *rf*, 'mtry', 'trees' and
    'min')

  - "custom" passes the options from '...'

  - "none" does not tune any hyperparameter

## Value

a
[parsnip::model_spec](https://parsnip.tidymodels.org/reference/model_spec.html)
of the model.

## Details

`sdm_spec_rf()` is simply a short form for `sm_spec_rand_forest()`.

## See also

Other "sdm model specifications":
[`sdm_spec_boost_tree()`](https://evolecolgroup.github.io/tidysdm/reference/sdm_spec_boost_tree.md),
[`sdm_spec_gam()`](https://evolecolgroup.github.io/tidysdm/reference/sdm_spec_gam.md),
[`sdm_spec_glm()`](https://evolecolgroup.github.io/tidysdm/reference/sdm_spec_glm.md),
[`sdm_spec_maxent()`](https://evolecolgroup.github.io/tidysdm/reference/sdm_spec_maxent.md)

## Examples

``` r
test_rf_spec <- sdm_spec_rf(tune = "sdm")
test_rf_spec
#> Random Forest Model Specification (classification)
#> 
#> Main Arguments:
#>   mtry = tune::tune()
#> 
#> Computational engine: ranger 
#> 
# combining tuning with specific values for other hyperparameters
sdm_spec_rf(tune = "sdm", trees = 100)
#> Random Forest Model Specification (classification)
#> 
#> Main Arguments:
#>   mtry = tune::tune()
#>   trees = 100
#> 
#> Computational engine: ranger 
#> 
```
