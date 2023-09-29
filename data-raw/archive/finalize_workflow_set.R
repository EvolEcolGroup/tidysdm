#' Finalize a workflow set to create an ensemble set
#'
#' This function selects the best model within each workflow based on the
#' metric provided, and then creates an `ensemble_set`, which can be later used
#' to make ensemble predictions. The `ensemble set` needs then to be fitted to
#' the training data, and can be later used to make predictions on the testing
#' set.
#' @param x a [workflowsets::workflow_set] object, fully trained
#' @param metric a string with the name of the metric to be used
#' @export

# TODO add a threshold used to select whether a workflow should be excluded
# finalise a workflowset, extracting the best fits for each workflow
finalize_workflow_set <- function(x, metric) {
  wkflow_id <- x$wflow_id
  # create a list of workflows
  wkflow_list <- list()
  for (i_wkflow in wkflow_id) {
    wkflow_list[[i_wkflow]] <- finalize_workflow(
      extract_workflow(x, i_wkflow),
      select_best(extract_workflow_set_result(x, i_wkflow), metric)
    )
  }
  # rank results
  ranked_results <- rank_results(x, rank_metric = metric, select_best = TRUE)
  ranked_results <- ranked_results[match(
    names(wkflow_list),
    ranked_results$wflow_id
  ), ]
  # TODO special case for gam, bring over the formula
  ensemble_set <- list(
    workflow = wkflow_list,
    # we need to sort this in the same order as above
    metrics_cv = ranked_results,
    trained = FALSE
  )
  class(ensemble_set) <- c("ensemble_set", class(ensemble_set))
  ensemble_set
}

#' Fit an ensemble set
#'
#' Once an ensemble set has been created with [finalise()] applied to
#' a [workflowsets::workflow_set] object, the workflows should be fitted to the
#' full training dataset.
#' @param object an ensemble_set object
#' @param data the data to fit to (usually the full training dataset)
#' @export

fit.ensemble_set <- function(object, data, ...) {
  object$workflow <- lapply(object$workflow, fit, data = data, ...)
  # TODO figure out how to set that worksets are fitted
  object$trained <- TRUE
  object
}

#' Predict for an ensemble set
#'
#' Predict to a new dataset for each workflow in the ensemble set.
#' @param object an ensemble_set object
#' @param new_data the data to fit to (usually the full training dataset)
#' @export
predict.ensemble_set <- function(object, new_data, type = NULL, fun = NULL, metric = NULL, ...) {
  if (!object$trained) {
    stop("the ensemble_set needs to train first. Use fit() with the training dataset")
  }
  predictions <- lapply(object$workflow, predict, new_data = new_data, type = type, ...)
  predictions
}

# ensemble_set <- function(model_list){
#   if (!all(unlist(lapply(model_list, inherits,"workflow")))){
#     stop("all models in model_lists should be workflows")
#   }
#   # TODO check that models are not trained (we will train them later)
#   class(model_list)<-c("ensemble_set", class(model_list))
# }
