# Predict for a repeat ensemble set

Predict for a new dataset by using a repeat ensemble. Predictions from
individual models are combined according to `fun`: if a weighted
function is used (`weighted_mean` or `weighted_median`), weights are
based on the metric used to tune models in the ensemble (see
[`repeat_ensemble`](https://evolecolgroup.github.io/tidysdm/dev/reference/repeat_ensemble.md)).

## Usage

``` r
# S3 method for class 'repeat_ensemble'
predict(
  object,
  new_data,
  type = "prob",
  fun = "mean",
  class_fun = c("majority", "prop"),
  metric_thresh = NULL,
  class_thresh = NULL,
  by_repeat = FALSE,
  ...
)
```

## Arguments

- object:

  an repeat_ensemble object

- new_data:

  a data frame in which to look for variables with which to predict.

- type:

  the type of prediction, "prob" or "class".

- fun:

  string defining the aggregating function. It can take values `mean`,
  `median`, `weighted_mean`, `weighted_median` and `none`. It is
  possible to combine multiple functions, except for "none". If it is
  set to "none", only the individual member predictions are returned.

- class_fun:

  the function to use to combine class predictions across repeats. It
  can be "majority" (the class with the highest proportion across
  repeats is predicted) or "prop" (the proportion of the "presence"
  class across repeats is returned). This argument is only used if
  `type` is "class".

- metric_thresh:

  a vector of length 2 giving a metric and its threshold, which will be
  used to prune which models in the ensemble will be used for the
  prediction. The 'metrics' need to have been computed when the workflow
  was tuned. Examples are c("accuracy",0.8) or c("boyce_cont",0.7)

- class_thresh:

  probability threshold used to convert probabilities into classes. It
  can be a number (between 0 and 1), or a character metric (currently
  "tss_max" or "sensitivity"). For sensitivity, an additional target
  value is passed along as a second element of a vector, e.g.
  c("sensitivity",0.8).

- by_repeat:

  boolean defining whether individual predictions for each repeat should
  be returned (no aggregating function will be applied at the repeat
  level). The columns for individual members have the name of the
  workflow as a prefix, separated by "." from the usual column names of
  the predictions.

- ...:

  not used in this method.

## Value

a tibble of predictions

## Examples

``` r
# we need a dataset to predict, we extract it from one of the models
new_data_ex <- workflowsets::extract_mold(
  lacerta_rep_ens$workflow[[1]]
)$predictors
ens_pred <- predict(lacerta_rep_ens,
  new_data = new_data_ex,
  fun = c("mean", "weighted_mean", "median")
)
head(ens_pred)
#>        mean weighted_mean    median
#> 1 0.1706767     0.1831235 0.1799623
#> 2 0.8780982     0.8733243 0.8951955
#> 3 0.1935688     0.1967814 0.1869704
#> 4 0.8106598     0.8108360 0.8280066
#> 5 0.9200148     0.9165388 0.9262732
#> 6 0.8988133     0.8857264 0.8855383
# set class thresholds for binary prediction
lacerta_rep_ens_calib <- calib_class_thresh(lacerta_rep_ens,
  class_thresh = c("tss_max")
)
ens_class_pred <- predict(lacerta_rep_ens_calib,
  new_data = new_data_ex,
  fun = c("mean", "median"), type = "class", class_thresh = c("tss_max")
)
head(ens_class_pred)
#>   mean.majority median.majority
#> 1     pseudoabs       pseudoabs
#> 2      presence        presence
#> 3     pseudoabs       pseudoabs
#> 4      presence        presence
#> 5      presence        presence
#> 6      presence        presence
```
