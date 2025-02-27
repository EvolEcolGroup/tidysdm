#' Obtain and format results produced by tuning functions for ensemble objects
#'
#' Return a tibble of performance metrics for all models.
#'
#' @param x A [`simple_ensemble`] or [`repeat_ensemble`] object
#' @param ... Not currently used.
#' @return A tibble.
#' @details
#'
#' When applied to a ensemble, the metrics that are returned do not contain the
#' actual tuning parameter columns and values (unlike when these collect
#' functions are run on other objects). The reason is that ensembles contain
#' different types of models or models with different tuning parameters.
#'
#' @seealso [tune::collect_metrics()]
#'
#' @examples
#' collect_metrics(lacerta_ensemble)
#' collect_metrics(lacerta_rep_ens)
#' @importFrom tune collect_metrics
#' @export

collect_metrics.simple_ensemble <- function(x, ...) {
  dplyr::bind_rows(x$metrics)
}

#' @export
#' @rdname collect_metrics.simple_ensemble
collect_metrics.repeat_ensemble <- function(x, ...) {
  metric_table <- dplyr::bind_rows(x$metrics, .id = "rep_id")
  metric_table$rep_id <- x$rep_id[as.numeric(metric_table$rep_id)]
  return(metric_table)
}
