autoplot_repeat <- function(repeat_ensemble) {
  metrics <- collect_metrics(repeat_ensemble)
  
  metrics$rep_index <- as.numeric(sub("rep_", "", metrics$rep_id))
  metrics$.metric <- as.factor(metrics$.metric)
  
  ggplot2::ggplot(
    metrics,
    aes(x = rep_index, y = mean, group = wflow_id, color = wflow_id)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    facet_wrap(~ .metric, nrow = 1, scales = "free_y") +
    theme_bw() +
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