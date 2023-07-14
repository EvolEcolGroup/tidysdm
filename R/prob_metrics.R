#' Probability metrics for `sf` objects
#'
#' `tidysdm` provides methods to handle [sf::sf] objects for the following
#' [yardstick] metrics:
#' @param data an [sf::sf] object
#' @param ... any other parameters to pass to the `data.frame` version of
#' the metric.
#' @import yardstick
#' @name prob_metrics
#'
NULL

#' @description [yardstick::average_precision()]
#' @export
#' @rdname prob_metrics
average_precision.sf <- function(data,...){
  data %>% dplyr::as_tibble() %>% yardstick::average_precision(...)
}

#' @description [yardstick::brier_class()]
#' @export
#' @rdname prob_metrics
brier_class.sf <- function(data,...){
  data %>% dplyr::as_tibble() %>% yardstick::brier_class(...)
}

#' @description [yardstick::classification_cost()]
#' @export
#' @rdname prob_metrics
classification_cost.sf <- function(data,...){
  data %>% dplyr::as_tibble() %>% yardstick::classification_cost(...)
}

#' @description [yardstick::gain_capture()]
#' @export
#' @rdname prob_metrics
gain_capture.sf <- function(data,...){
  data %>% dplyr::as_tibble() %>% yardstick::gain_capture(...)
}

#' @description [yardstick::mn_log_loss()]
#' @export
#' @rdname prob_metrics
mn_log_loss.sf <- function(data,...){
  data %>% dplyr::as_tibble() %>% yardstick::mn_log_loss(...)
}

#' @description [yardstick::pr_auc()]
#' @export
#' @rdname prob_metrics
pr_auc.sf <- function(data,...){
  data %>% dplyr::as_tibble() %>% yardstick::pr_auc(...)
}

#' @description [yardstick::roc_auc()]
#' @export
#' @rdname prob_metrics
roc_auc.sf <- function(data,...){
  data %>% dplyr::as_tibble() %>% yardstick::roc_auc(...)
}

#' @description [yardstick::roc_aunp()]
#' @export
#' @rdname prob_metrics
roc_aunp.sf <- function(data,...){
  data %>% dplyr::as_tibble() %>% yardstick::roc_aunp(...)
}

#' @description [yardstick::roc_aunu()]
#' @export
#' @rdname prob_metrics
roc_aunu.sf <- function(data,...){
  data %>% dplyr::as_tibble() %>% yardstick::roc_aunu(...)
}
