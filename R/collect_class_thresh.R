#' Obtain and format the class thresholds for ensemble objects
#'
#' Return a tibble of class thresholds, as computed by `calib_class_thresh()`.
#' @param x A [`simple_ensemble`]
#' @param ... Not currently used.
#' @return A tibble.
#' @examplesIf rlang::is_installed("earth")
#' test_ens <- simple_ensemble() %>%
#'   add_member(two_class_res[1:3, ], metric = "roc_auc")
#' test_ens <- calib_class_thresh(test_ens, class_thresh = "tss_max")
#' test_ens <- calib_class_thresh(test_ens, class_thresh = "kap_max")
#' collect_class_thresh(test_ens)
#' @export
#' @keywords predict
#'

# note that this function is tested in test_calib_class_thresh.R

# define the generic S3 method
collect_class_thresh <- function(x, ...) {
  UseMethod("collect_class_thresh", object = x)
}

# default method
#' @export
collect_class_thresh.default <- function(x, ...) {
  stop("no method available for this object type: ", class(x))
}

# method for simple_ensemble
#' @export
collect_class_thresh.simple_ensemble <- function(x, ...) {
  attr(x, "class_thresholds")
}
