#' Plot the results of a repeat ensemble
#'
#' This `autoplot()` method plots performance metrics for each repeat,
#'  ranked using one of the metrics.
#'
#' @param object A [`repeat_ensemble`] whose elements have results.
#' @param ... Other options to pass to `autoplot()`. Currently unused.
#' @returns A ggplot object.
#' @export
#' @keywords ensemble
#' @examples
#' autoplot(lacerta_rep_ens)
#'
# TODO we should add metric and rank metric in a manner similar to what we have
# in autoplot.simple_ensemble, but for now we just plot all metrics
autoplot.repeat_ensemble <- function(object, ...) {
  # check that ellipse is empty
  if (length(list(...)) > 0) {
    warning("additional arguments ... are currently not used")
  }
  metrics <- collect_metrics(object)

  metrics$rep_index <- as.numeric(sub("rep_", "", metrics$rep_id))
  metrics$.metric <- as.factor(metrics$.metric)

  ggplot2::ggplot(
    metrics,
    ggplot2::aes(
      x = .data$rep_index,
      y = .data$mean,
      group = .data$wflow_id,
      color = .data$wflow_id
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ .data$.metric, nrow = 1, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = "Repeated ensemble autoplot",
      x = "Repeat id",
      y = "Mean",
      color = "Model"
    ) +
    ggplot2::theme(
      legend.position = "right"
    )
}
