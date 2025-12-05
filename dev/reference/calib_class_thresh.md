# Calibrate class thresholds

Calibrate the probability thresholds that convert probabilities into
classes for a simple ensemble object. This is done by generating
predictions for the training data, and then optimizing the threshold
according to the metric given in `class_thresh`. The calibration depends
on how the ensemble is pruned, which is defined by `metric_thresh`, and
how predictions are combined. `calib_class_threshold` considers the four
possible combining options available via the parameter `fun` in
[predict.simple_ensemble](https://evolecolgroup.github.io/tidysdm/dev/reference/predict.simple_ensemble.md);
note that the weighted functions `weighted_mean` and `weighted_median`
use weights based on the metric used to tune the ensemble, and so they
might make little sense if used in conjunction with a different metric.
The updated simple ensemble contains information on the optimal
thresholds for the given combination of `class_thresh`, `metric_thresh`
and `fun`, and these will be used when predicting classes with
[predict.simple_ensemble](https://evolecolgroup.github.io/tidysdm/dev/reference/predict.simple_ensemble.md).

## Usage

``` r
calib_class_thresh(object, class_thresh, metric_thresh = NULL)
```

## Arguments

- object:

  an simple_ensemble object

- class_thresh:

  probability threshold used to convert probabilities into classes. It
  can be a number (between 0 and 1), or a character metric (currently
  "tss_max", "kap_max" or "sensitivity"). For sensitivity, an additional
  target value is passed along as a second element of a vector, e.g.
  c("sensitivity",0.8).

- metric_thresh:

  a vector of length 2 giving a metric and its threshold, which will be
  used to prune which models in the ensemble will be used for the
  prediction. The 'metrics' need to have been computed when the workflow
  was tuned. The metric's threshold needs to match the value used during
  prediction. Examples are c("accuracy",0.8) or c("boyce_cont",0.7).

## Value

a
[simple_ensemble](https://evolecolgroup.github.io/tidysdm/dev/reference/simple_ensemble.md)
object with an additional attribute `class_thresholds`, which is a
tibble with columns:

- `class_thresh`: the value passed to `class_thresh`

- `metric_thresh`: the value passed to `metric_thresh`

- `fun`: the aggregating function used to combine predictions

- `optim_value`: the optimal threshold for the given combination of
  `class_thresh`, `metric_thresh` and `fun`

## Examples

``` r
test_ens <- simple_ensemble() %>%
  add_member(two_class_res[1:3, ], metric = "roc_auc")
test_ens <- calib_class_thresh(test_ens, class_thresh = "tss_max")
test_ens <- calib_class_thresh(test_ens, class_thresh = "kap_max")
test_ens <- calib_class_thresh(test_ens, class_thresh = c("sens", 0.9))
```
