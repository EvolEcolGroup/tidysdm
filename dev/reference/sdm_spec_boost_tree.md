# Model specification for a Boosted Trees model for SDM

This function returns a
[parsnip::model_spec](https://parsnip.tidymodels.org/reference/model_spec.html)
for a Boosted Trees model to be used as a classifier of presences and
absences in Species Distribution Model. It uses the library `xgboost` to
fit boosted trees; to use another library, simply build the
[parsnip::model_spec](https://parsnip.tidymodels.org/reference/model_spec.html)
directly.

## Usage

``` r
sdm_spec_boost_tree(..., tune = c("sdm", "all", "custom", "none"))
```

## Arguments

- ...:

  parameters to be passed to
  [`parsnip::boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html)
  to customise the model. See the help of that function for details.

- tune:

  character defining the tuning strategy. Valid strategies are:

  - "sdm" chooses hyperparameters that are most important to tune for an
    sdm (for *boost_tree*: 'mtry', 'trees', 'tree_depth', 'learn_rate',
    'loss_reduction', and 'stop_iter')

  - "all" tunes all hyperparameters (for *boost_tree*: 'mtry', 'trees',
    'tree_depth', 'learn_rate', 'loss_reduction', 'stop_iter','min_n'
    and 'sample_size')

  - "custom" passes the options from '...'

  - "none" does not tune any hyperparameter

## Value

a
[parsnip::model_spec](https://parsnip.tidymodels.org/reference/model_spec.html)
of the model.

## See also

Other "sdm model specifications":
[`sdm_spec_gam()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_gam.md),
[`sdm_spec_glm()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_glm.md),
[`sdm_spec_maxent()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_maxent.md),
[`sdm_spec_rand_forest()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_rand_forest.md)

## Examples

``` r
standard_bt_spec <- sdm_spec_boost_tree()
full_bt_spec <- sdm_spec_boost_tree(tune = "all")
custom_bt_spec <- sdm_spec_boost_tree(tune = "custom", mtry = tune::tune())
```
