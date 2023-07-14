#' Simple ensemble
#'
#' A simple ensemble is a collection of workflows for which predictions will
#' be combined in a simple way (e.g. by taking either the mean or median). Usually
#' these workflows will consists each of the best version of a given model type
#' following turning. The workflows are fitted to the full training dataset
#' before making predictions.
#'
#' @param ... not used, this function just creates an empty `simple_ensemble`
#' object. Members are added with `add_best_candidates()`
#' @return an empty `simple_ensemble`. This is a tibble with columns:
#' {/itemize:
#' /item: `wflow_id`: the name of the workflows for which the best model was
#' chosen
#' /item: `workflow`: the trained workflow objects
#' /item: `metrics`: metrics based on the crossvalidation resampling used
#' to tune the models
#' }
#' @export

simple_ensemble <- function(...) {
  parsnip::check_empty_ellipse(...)
  # a tibble with columns: name, workflow
  base_ensemble <- tibble::tibble(
    wflow_id = character(),
    workflow = list(),
    # tibble of metrics from the CV on the training dataset (coming from when
    # the workflow was originally fit, potentially as part of a workflow_set)
    metrics = list()
  )

  base_ensemble <- structure(base_ensemble, class = c("simple_ensemble",
                                                      class(base_ensemble)))
}

#' @export
print.simple_ensemble <- function(x, ...) {
  rlang::inform("A simple_ensemble of models")
  if (nrow(x)>0) {
    rlang::inform(c("\nMembers:",x$wflow_id))
    rlang::inform(c("\nAvailable metrics:",attr(x,"metrics")))
    rlang::inform(c("\nMetric used to tune workflows:",attr(x,"best_metric")))
  } else {
    rlang::inform("\nThis object is empty; add models with `add_members()`")
  }
}

#' @export
summary.simple_ensemble <- function(object, ...) {
  print(object)
}

