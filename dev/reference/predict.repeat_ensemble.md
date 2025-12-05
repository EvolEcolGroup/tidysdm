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
  metric_thresh = NULL,
  class_thresh = NULL,
  members = FALSE,
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
  set to "none", only the individual member predictions are returned
  (this automatically sets `member` to TRUE)

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

- members:

  boolean defining whether individual predictions for each member should
  be added to the ensemble prediction. The columns for individual
  members have the name of the workflow a a prefix, separated by "."
  from the usual column names of the predictions.

- ...:

  not used in this method.

## Value

a tibble of predictions
