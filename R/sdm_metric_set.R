#' Metric set for SDM
#'
#' This function returns a [yardstick::metric_set] that includes [boyce_cont()],
#' [yardstick::roc_auc()] and [tss_max()], the most commonly used metrics for
#' SDM.
#'
#' @param ... additional metrics to be added to the
#' [`yardstick::metric_set`]. See the help to [yardstick::metric_set()] for
#' constraints on the type of metrics that can be mixed.
#' @return a [`yardstick::metric_set`] object.
#' @examples
#' sdm_metric_set()
#' sdm_metric_set(accuracy)
#' @export

sdm_metric_set <- function(...){
  metric_set(roc_auc, boyce_cont, tss_max, ...)
}
