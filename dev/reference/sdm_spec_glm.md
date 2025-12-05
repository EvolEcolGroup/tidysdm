# Model specification for a GLM for SDM

This function returns a
[parsnip::model_spec](https://parsnip.tidymodels.org/reference/model_spec.html)
for a Generalised Linear Model to be used as a classifier of presences
and absences in Species Distribution Model.

## Usage

``` r
sdm_spec_glm(..., tune = "none")
```

## Arguments

- ...:

  parameters to be passed to
  [`parsnip::logistic_reg()`](https://parsnip.tidymodels.org/reference/logistic_reg.html)
  to customise the model. See the help of that function for details.

- tune:

  character defining the tuning strategy. As there are no
  hyperparameters to tune in a *glm*, the only valid option is "none".
  This parameter is present for consistency with other `sdm_spec_*`
  functions, but it does nothing in this case.

## Value

a
[parsnip::model_spec](https://parsnip.tidymodels.org/reference/model_spec.html)
of the model.

## See also

Other "sdm model specifications":
[`sdm_spec_boost_tree()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_boost_tree.md),
[`sdm_spec_gam()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_gam.md),
[`sdm_spec_maxent()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_maxent.md),
[`sdm_spec_rand_forest()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_rand_forest.md)

## Examples

``` r
my_spec_glm <- sdm_spec_glm()
```
