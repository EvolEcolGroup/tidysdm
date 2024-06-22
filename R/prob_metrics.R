#' Probability metrics for `sf` objects
#'
#' `tidysdm` provides specialised metrics for SDMs, which have their own
#' help pages([boyce_cont()], [kap_max()], and [tss_max()]). Additionally, it also 
#' provides methods to handle [sf::sf] objects for the following
#' standard `yardstick` metrics:
#' 
#' Note that `roc_aunp` and `roc_aunu` are multiclass metrics, and as such are
#' are not relevant for SDMs (which work on a binary response). They are included
#' for completeness, so that all class probability metrics from `yardstick` have
#' an `sf` method, for applications other than SDMs.
#' @param data an [sf::sf] object
#' @param ... any other parameters to pass to the `data.frame` version of
#' the metric. See the specific man page for the metric of interest.
#' @returns A tibble with columns `.metric`, `.estimator`, and `.estimate`
#'  and 1 row of values.
#' @import yardstick
#' @name prob_metrics_sf
#'
NULL

#' @description [yardstick::average_precision()]
#' @export
#' @rdname prob_metrics_sf
average_precision.sf <- function(data, ...) {
  data %>%
    dplyr::as_tibble() %>%
    yardstick::average_precision(...)
}

#' @description [yardstick::brier_class()]
#' @export
#' @rdname prob_metrics_sf
brier_class.sf <- function(data, ...) {
  data %>%
    dplyr::as_tibble() %>%
    yardstick::brier_class(...)
}

#' @description [yardstick::classification_cost()]
#' @export
#' @rdname prob_metrics_sf
classification_cost.sf <- function(data, ...) {
  data %>%
    dplyr::as_tibble() %>%
    yardstick::classification_cost(...)
}

#' @description [yardstick::gain_capture()]
#' @export
#' @rdname prob_metrics_sf
gain_capture.sf <- function(data, ...) {
  data %>%
    dplyr::as_tibble() %>%
    yardstick::gain_capture(...)
}

#' @description [yardstick::mn_log_loss()]
#' @export
#' @rdname prob_metrics_sf
mn_log_loss.sf <- function(data, ...) {
  data %>%
    dplyr::as_tibble() %>%
    yardstick::mn_log_loss(...)
}

#' @description [yardstick::pr_auc()]
#' @export
#' @rdname prob_metrics_sf
pr_auc.sf <- function(data, ...) {
  data %>%
    dplyr::as_tibble() %>%
    yardstick::pr_auc(...)
}

#' @description [yardstick::roc_auc()]
#' @export
#' @rdname prob_metrics_sf
roc_auc.sf <- function(data, ...) {
  data %>%
    dplyr::as_tibble() %>%
    yardstick::roc_auc(...)
}

#' @description [yardstick::roc_aunp()]
#' @export
#' @rdname prob_metrics_sf
roc_aunp.sf <- function(data, ...) {
  data %>%
    dplyr::as_tibble() %>%
    yardstick::roc_aunp(...)
}

#' @description [yardstick::roc_aunu()]
#' @export
#' @rdname prob_metrics_sf
roc_aunu.sf <- function(data, ...) {
  data %>%
    dplyr::as_tibble() %>%
    yardstick::roc_aunu(...)
}
