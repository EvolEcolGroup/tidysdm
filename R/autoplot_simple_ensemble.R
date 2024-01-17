#' Plot the results of a simple ensemble
#'
#' This `autoplot()` method plots performance metrics that have been
#'  ranked using a metric.
#'
#' @param object A [`simple_ensemble`] whose elements have results.
#' @param rank_metric A character string for which metric should be used to rank
#' the results. If none is given, the first metric in the metric set is used
#' (after filtering by the `metric` option).
#' @param metric A character vector for which metrics (apart from `rank_metric`)
#' to be included in the visualization. If NULL (the default), all available 
#' metrics will be plotted
#' @param std_errs The number of standard errors to plot (if the standard error
#' exists).
#' @param ... Other options to pass to `autoplot()`. Currently unused.
#' @details
#' This function is intended to produce a default plot to visualize helpful
#'  information across all possible applications of a [`simple_ensemble`]. More
#'  sophisticated plots can be produced using standard `ggplot2` code for
#'  plotting.
#'
#' The x-axis is the workflow rank in the set (a value of one being the best)
#' versus the performance metric(s) on the y-axis. With multiple metrics, there
#' will be facets for each metric, with the `rank_metric` first (if any was
#' provided; otherwise the metric used to create the [`simple_ensemble`] will
#' be used).
#'
#' If multiple resamples are used, confidence bounds are shown for each result
#' (95% confidence, by default).
#' @returns A ggplot object.
#' @examples
#' \donttest{
#' #' # we use the two_class_example from `workflowsets`
#' two_class_ens <- simple_ensemble() %>%
#'   add_member(two_class_res, metric = "roc_auc")
#' autoplot(two_class_ens)
#' }
#' @export
autoplot.simple_ensemble <- function(object, rank_metric = NULL, metric = NULL,
                                     std_errs = stats::qnorm(0.95), ...) {
  # get metrics table
  res <- object$metrics %>% dplyr::bind_rows()
  # vector of available metrics
  metric_avail <- attr(object, "metrics")
  # if we requested metrics, make sure they are all available
  if (!is.null(metric)) {
    # all necessary metrics (rank_metric plus all in metric)
    keep_metrics <- unique(c(rank_metric, metric))
    if (!all(keep_metrics %in% metric_avail)) {
      stop("some metrics are not part of the ensemble")
    }
    res <- dplyr::filter(res, .data$.metric %in% keep_metrics)
    # reset available metrics to what is left behind
    metric_avail <- unique(res$.metric)
  }

  # check we have the rank_metric, or, if we were not given one, take the
  # metric that was to choose models in the workflows when simple_ensemble was
  # created
  if (!is.null(rank_metric)) {
    if (!rank_metric %in% metric_avail) {
      stop("rank metric ", rank_metric, " is not part of the ensemble")
    }
  } else {
    rank_metric <- attr(object, "best_metric")
  }
  # rank models by the metric of choice
  res_rank_metric <- res %>%
    dplyr::filter(.data$.metric == rank_metric) %>%
    dplyr::arrange(mean)

  res$rank <- (1:nrow(res_rank_metric))[match(
    res$wflow_id,
    res_rank_metric$wflow_id
  )]

  # relevel the .metric factor to show first the rank metric
  res <- res %>%
    dplyr::mutate(.metric = stats::relevel(factor(res$.metric),
      ref = rank_metric
    ))

  num_metrics <- length(unique(res$.metric))
  has_std_error <- !all(is.na(res$std_err))

  p <-
    ggplot2::ggplot(res, ggplot2::aes(
      x = .data$rank,
      y = .data$mean,
      col = .data$wflow_id
    )) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$wflow_id))

  if (num_metrics > 1) {
    p <-
      p +
      ggplot2::facet_wrap(~ .data$.metric, scales = "free_y", as.table = FALSE) +
      ggplot2::labs(x = "Workflow Rank", y = "Metric")
  } else {
    p <- p + ggplot2::labs(x = "Workflow Rank", y = unique(res$.metric))
  }

  if (has_std_error) {
    p <-
      p +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = .data$mean - .data$std_err * std_errs,
          ymax = .data$mean + .data$std_err * std_errs
        ),
        width = diff(range(res$rank)) / 75
      )
  }

  p
}
