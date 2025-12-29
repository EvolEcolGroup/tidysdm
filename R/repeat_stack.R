#' Repeat stack ensemble
#'
#' An ensemble of linear stacks based multiple sets of
#' pseudoabsences/background. This object is a collection (list) of
#' `linear_stack` objects from package `stacks` for which predictions will be
#' combined in a simple way (e.g. by taking either the mean or median). Each
#' `linear_stack` is a weighted combination of models following turning; all
#' stacks will need to have the same metric estimated during the tuning process.
#'
#' @param ... not used, this function just creates an empty `repeat_stack`
#'   object. Members are added with `add_stack()`
#' @returns an empty `repeat_stack`
#' @export

repeat_stack <- function(...) {
  parsnip::check_empty_ellipse(...)
  # a tibble with columns: name, workflow
  base_ensemble <- tibble::tibble(
    rep_id = character(),
    stack = list(),
    # tibble of metrics from the CV on the training dataset (coming from when
    # the workflow was originally fit, potentially as part of a workflow_set)
    metrics = list()
  )
  base_ensemble <- structure(base_ensemble, class = c(
    "repeat_stack",
    class(base_ensemble)
  ))
}

#' @export
print.repeat_stack <- function(x, ...) {
  rlang::inform("A repeat_stack of models")

  if (nrow(x) > 0) {
    rlang::inform(c("\nNumber of repeats:", length(unique(x$rep_id))))
##    rlang::inform(c("\nMembers:", unique(x$wflow_id)))
    # all simple_ensembles need to have the same metrics
    rlang::inform(c("\nAvailable metrics:", attr(x, "metrics")))
    rlang::inform(c("\nMetric used to tune workflows:", attr(x, "best_metric")))
  } else {
    rlang::inform("\nThis object is empty; add models with `add_stacks()`")
  }
}

#' @export
summary.repeat_stack <- function(object, ...) {
  print(object)
}


## TODO we need to add a function to add stacks to the repeat_stack object