#' Predict for a repeat ensemble set
#'
#' Predict for a new dataset by using a repeat ensemble. Predictions from individual
#' models are combined according to `fun`
#' @param object an repeat_ensemble object
#' @param new_data a data frame in which to look for variables with which to predict.
#' @param type the type of prediction, "prob" or "class".
#' @param fun string defining the aggregating function. It can take values
#' `mean`, `median`, `weighted_mean`, `weighted_median` and `none`. It is possible
#' to combine multiple functions, except for "none". If it
#' is set to "none", only the individual member predictions are returned (this
#' automatically sets `member` to TRUE)
#' @param metric_thresh a vector of length 2 giving a metric and its threshold,
#' which will be used to prune
#' which models in the ensemble will be used for the prediction. The 'metrics'
#' need to have been computed when the workflow was tuned. Examples are
#' c("accuracy",0.8) or c("boyce_cont",0.7)
#' @param class_thresh probability threshold used to convert probabilities into
#' classes. It can be a number (between 0 and 1), or a character metric (currently
#' "tss_max" or "sensitivity"). For sensitivity, an additional target value is passed
#' along as a second element of a vector, e.g. c("sensitivity",0.8).
#' @param members boolean defining whether individual predictions for each member
#' should be added to the ensemble prediction. The columns for individual members
#' have the name of the workflow a a prefix, separated by "." from the usual
#' column names of the predictions.
#' @param ... not used in this method.
#' @returns a tibble of predictions
#' @method predict repeat_ensemble
#' @export
predict.repeat_ensemble <-
  function (object,
            new_data,
            type = "prob",
            fun = "mean",
            metric_thresh = NULL,
            class_thresh = NULL,
            members = FALSE,
            ...) {
  # we change the names of the workflows to combine with the repeat ids
  object$workflow_id <- paste(object$rep_id,object$wflow_id, sep=".")
  class(object)[1] <- "simple_ensemble"
  # now predict the object as if it was a simple ensemble
  stats::predict(object = object,
          new_data = new_data,
          type = type,
          fun = fun,
          metric_thresh = metric_thresh,
          class_thresh = class_thresh,
          members = members)
  }
