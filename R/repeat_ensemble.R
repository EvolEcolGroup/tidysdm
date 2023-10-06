#' Repeat ensemble
#'
#' An ensemble based multiple sets of pseudoabsences/background. This object
#' is a collection (list) of [`simple_ensemble`] objects for which predictions will
#' be combined in a simple way (e.g. by taking either the mean or median). Each
#'  [`simple_ensemble`] contains the best version of a each given model type
#' following turning; all simple ensembles will need to have the same metric
#' estimated during the cv process.
#'
#' @param ... not used, this function just creates an empty `repeat_ensemble`
#' object. Members are added with `add_best_candidates()`
#' @returns an empty `repeat_ensemble`
#' @export

repeat_ensemble <- function(...) {
  parsnip::check_empty_ellipse(...)
  # a tibble with columns: name, workflow
  base_ensemble <- tibble::tibble(
    rep_id = character(),
    # the three slots below are the same as a simple ensemble
    wflow_id = character(),
    workflow = list(),
    # tibble of metrics from the CV on the training dataset (coming from when
    # the workflow was originally fit, potentially as part of a workflow_set)
    metrics = list()
  )
  base_ensemble <- structure(base_ensemble, class = c(
    "repeat_ensemble",
    class(base_ensemble)
  ))
}

#' @export
print.repeat_ensemble <- function(x, ...) {
  rlang::inform("A repeat_ensemble of models")

  if (nrow(x) > 0) {
    rlang::inform(c("\nNumber of repeats:", length(unique(x$rep_id))))
    rlang::inform(c("\nMembers:", unique(x$wflow_id)))
    # all simple_ensembles need to have the same metrics
    rlang::inform(c("\nAvailable metrics:", attr(x, "metrics")))
    rlang::inform(c("\nMetric used to tune workflows:", attr(x, "best_metric")))
  } else {
    rlang::inform("\nThis object is empty; add models with `add_repeats()`")
  }
}

#' @export
summary.repeat_ensemble <- function(object, ...) {
  print(object)
}
