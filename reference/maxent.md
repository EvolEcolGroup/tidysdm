# MaxEnt model

`maxent` defines the MaxEnt model as used in Species Distribution
Models. A good guide to how options of a MaxEnt model work can be found
in
https://onlinelibrary.wiley.com/doi/full/10.1111/j.1600-0587.2013.07872.x

## Usage

``` r
maxent(
  mode = "classification",
  engine = "maxnet",
  feature_classes = NULL,
  regularization_multiplier = NULL
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "classification".

- engine:

  A single character string specifying what computational engine to use
  for fitting. Currently only "maxnet" is available.

- feature_classes:

  character, continuous feature classes desired, either "default" or any
  subset of "lqpht" (for example, "lh")

- regularization_multiplier:

  numeric, a constant to adjust regularization

## Value

a
[`parsnip::model_spec`](https://parsnip.tidymodels.org/reference/model_spec.html)
for a `maxent` model

## Examples

``` r
# format the data
data("bradypus", package = "maxnet")
bradypus_tb <- tibble::as_tibble(bradypus) %>%
  dplyr::mutate(presence = relevel(
    factor(
      dplyr::case_match(presence, 1 ~ "presence", 0 ~ "absence")
    ),
    ref = "presence"
  )) %>%
  select(-ecoreg)

# fit the model, and make some predictions
maxent_spec <- maxent(feature_classes = "lq")
maxent_fitted <- maxent_spec %>%
  fit(presence ~ ., data = bradypus_tb)
pred_prob <- predict(maxent_fitted,
  new_data = bradypus[, -1],
  type = "prob"
)
pred_class <- predict(maxent_fitted,
  new_data = bradypus[, -1],
  type = "class"
)

# Now with tuning
maxent_spec <- maxent(
  regularization_multiplier = tune::tune(),
  feature_classes = tune::tune()
)
set.seed(452)
cv <- vfold_cv(bradypus_tb, v = 2)
maxent_tune_res <- maxent_spec %>%
  tune_grid(presence ~ ., cv, grid = 3)
show_best(maxent_tune_res, metric = "roc_auc")
#> # A tibble: 3 × 8
#>   feature_classes regularization_multip…¹ .metric .estimator  mean     n std_err
#>   <chr>                             <dbl> <chr>   <chr>      <dbl> <int>   <dbl>
#> 1 lq                                 0.5  roc_auc binary     0.855     2  0.0105
#> 2 l                                  3    roc_auc binary     0.851     2  0.0250
#> 3 lqp                                1.75 roc_auc binary     0.842     2  0.0127
#> # ℹ abbreviated name: ¹​regularization_multiplier
#> # ℹ 1 more variable: .config <chr>
```
