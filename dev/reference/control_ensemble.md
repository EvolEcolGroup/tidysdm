# Control wrappers

Supply these light wrappers as the `control` argument in a
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html),
[`tune::tune_bayes()`](https://tune.tidymodels.org/reference/tune_bayes.html),
or
[`tune::fit_resamples()`](https://tune.tidymodels.org/reference/fit_resamples.html)
call to return the needed elements for use in an ensemble. These
functions will return the appropriate control grid to ensure that
assessment set predictions and information on model specifications and
preprocessors, is supplied in the resampling results object!

To integrate ensemble settings with your existing control settings, note
that these functions just call the appropriate `tune::control_*`
function with the arguments `save_pred = TRUE, save_workflow = TRUE`.

These wrappers are equivalent to the ones used in the `stacks` package.

## Usage

``` r
control_ensemble_grid()

control_ensemble_resamples()

control_ensemble_bayes()
```

## Value

A
[tune::control_grid](https://tune.tidymodels.org/reference/control_grid.html),
[tune::control_bayes](https://tune.tidymodels.org/reference/control_bayes.html),
or
[tune::control_resamples](https://tune.tidymodels.org/reference/control_grid.html)
object.

## See also

See the vignettes for examples of these functions used in context.
