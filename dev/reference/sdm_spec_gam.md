# Model specification for a GAM for SDM

This function returns a
[parsnip::model_spec](https://parsnip.tidymodels.org/reference/model_spec.html)
for a General Additive Model to be used as a classifier of presences and
absences in Species Distribution Model.

## Usage

``` r
sdm_spec_gam(..., tune = "none")
```

## Arguments

- ...:

  parameters to be passed to
  [`parsnip::gen_additive_mod()`](https://parsnip.tidymodels.org/reference/gen_additive_mod.html)
  to customise the model. See the help of that function for details.

- tune:

  character defining the tuning strategy. As there are no
  hyperparameters to tune in a *gam*, the only valid option is "none".
  This parameter is present for consistency with other `sdm_spec_*`
  functions, but it does nothing in this case.

## Value

a
[parsnip::model_spec](https://parsnip.tidymodels.org/reference/model_spec.html)
of the model.

## Details

Note that, when using GAMs in a `workflow_set()`, it is necessary to
update the model with
[`gam_formula()`](https://evolecolgroup.github.io/tidysdm/dev/reference/gam_formula.md)
(see
[`parsnip::model_formula`](https://parsnip.tidymodels.org/reference/model_formula.html)
for a discussion of formulas with special terms in `tidymodels`):

    workflow_set(
      preproc = list(default = my_recipe),
      models = list(gam = sdm_spec_gam()),
      cross = TRUE
    ) %>% update_workflow_model("default_gam",
                                spec = sdm_spec_gam(),
                                formula = gam_formula(my_recipe))

## See also

[`parsnip::gen_additive_mod()`](https://parsnip.tidymodels.org/reference/gen_additive_mod.html)
[`gam_formula()`](https://evolecolgroup.github.io/tidysdm/dev/reference/gam_formula.md)

Other "sdm model specifications":
[`sdm_spec_boost_tree()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_boost_tree.md),
[`sdm_spec_glm()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_glm.md),
[`sdm_spec_maxent()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_maxent.md),
[`sdm_spec_rand_forest()`](https://evolecolgroup.github.io/tidysdm/dev/reference/sdm_spec_rand_forest.md)

## Examples

``` r
my_gam_spec <- sdm_spec_gam()
```
