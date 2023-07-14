#' Add best member of workflow to a simple ensemble
#'
#' This function adds member(s) to a [simple_ensemble()] object, taking the
#' best member from each workflow provided. It is possible to pass individual
#' `tune_results` objects from a tuned `workflow`, or a [workflowsets::workflow_set()].
#'
#' @param x a [simple_ensemble] to which member(s) will be added
#' @param member a  `tune_results`, or a [`workflowsets::workflow_set`]
#' @param metric A character string (or NULL) for which metric to optimize.
#' If NULL, the first metric is used.
#' @param ... not used at the moment.
#' @returns a [simple_ensemble] with additional member(s)
#' @export

# Note that `add_member` is tested in `test_simple_ensemble.R`

add_member <- function (x, member, ...) {
  UseMethod("add_member", object = member)
}

#' @rdname add_member
#' @export
add_member.default <- function(x, member, ...){
  stop("no method available for this object type")
}

#' @param id the name to be given to this workflow in the `wflow_id` column.
#' @rdname add_member
#' @export
add_member.tune_results <- function(x, member, metric = NULL, id=NULL, ...){
  # set the metric if the simple ensemble is empty (and thus has no metric)
  if (is.null(attr(x,"best_metric"))){
    attr(x,"best_metric") <- utils::getFromNamespace("choose_metric", "tune")(metric, member)
  }

  # if metric is NULL
  if (is.null(metric)){
    # use the metric from the simple ensemble
    metric <- attr(x,"best_metric")
  } else {
    if (metric!=attr(x,"best_metric")){
      stop("the requested metric is not the same as the one previously used in x")
    }
  }

  # use name of object as id if we don't have one
  if (is.null(id)){
    id <-deparse(substitute(member))
  }
  # check that id is unique
  if (id %in% x$wflow_id){
    stop("x already has a member with the same name, provide an alternative name")
  }

  # get the best config, metric, etc.
  best_config <- tune::select_best(member,metric)$.config
  best_metrics <- tune::collect_metrics(member) %>%
    dplyr::filter(.data$.config ==best_config) %>%
    dplyr::select(dplyr::all_of(c(".metric", "mean", "std_err", "n"))) %>%
    dplyr::mutate(wflow_id=id, .before=dplyr::all_of(c(".metric")))
  # now check that metrics are the same as in best_metrics
  if (!is.null(attr(x,"metrics"))){
    if(!all(attr(x,"metrics")%in%best_metrics$.metric)){
      stop("x and member were not evaluated for the same metrics")
    }
  } else {
    attr(x,"metrics")<-best_metrics$.metric
  }
  # subset best metrics to just the metrics used in the ensemble
  best_metrics <- best_metrics %>% dplyr::filter(.data$.metric %in% attr(x,"metrics"))

  best_fit <- tune::fit_best(member, metric = metric)

  x %>% dplyr::bind_rows(tibble::tibble(
    wflow_id = id,
    workflow = list(best_fit),
    # tibble of metrics from the CV on the training dataset (coming from when
    # the workflow was originally fit, potentially as part of a workflow_set)
    metrics = list(best_metrics)
  ))
}

#' @rdname add_member
#' @export
add_member.workflow_set <- function(x, member, metric = NULL, ...) {
  for (i_wflow in member$wflow_id){
    this_res <- workflowsets::extract_workflow_set_result(member, id = i_wflow)
    x <- x %>% add_member(this_res, metric=metric, id = i_wflow)
  }
  x
}
