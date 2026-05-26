#' Calibrate class thresholds
#'
#' Calibrate the probability thresholds that convert probabilities into classes
#' for a simple ensemble object. This is done by generating predictions for the
#' training data, and then optimizing the threshold according to the metric
#' given in `class_thresh`. The calibration depends on how the ensemble is
#' pruned, which is defined by `metric_thresh`, and how predictions are
#' combined. `calib_class_threshold` considers the four possible combining
#' options available via the parameter `fun` in [predict.simple_ensemble]; note
#' that the weighted functions `weighted_mean` and `weighted_median` use weights
#' based on the metric used to tune the ensemble, and so they might make little
#' sense if used in conjunction with a different metric. The updated simple
#' ensemble contains information on the optimal thresholds for the given
#' combination of `class_thresh`, `metric_thresh` and `fun`, and these will be
#' used when predicting classes with [predict.simple_ensemble].
#' @param object an simple_ensemble object
#' @param metric_thresh a vector of length 2 giving a metric and its threshold,
#'   which will be used to prune which models in the ensemble will be used for
#'   the prediction. The 'metrics' need to have been computed when the workflow
#'   was tuned. The metric's threshold needs to match the value used during
#'   prediction. Examples are c("accuracy",0.8) or c("boyce_cont",0.7).
#' @param class_thresh probability threshold used to convert probabilities into
#'   classes. It can be a number (between 0 and 1), or a character metric
#'   (currently "tss_max", "kap_max" or "sensitivity"). For sensitivity, an
#'   additional target value is passed along as a second element of a vector,
#'   e.g. c("sensitivity",0.8).
#' @returns a [simple_ensemble] object with an additional attribute
#'   `class_thresholds`, which is a tibble with columns:
#'  * `class_thresh`: the value passed to `class_thresh`
#'  * `metric_thresh`: the value passed to `metric_thresh`
#'  * `fun`: the aggregating function used to combine predictions
#'  * `optim_value`: the optimal threshold for the given combination of
#'   `class_thresh`, `metric_thresh` and `fun`
#' @examplesIf rlang::is_installed("earth")
#' # for a simple ensemble
#' test_ens <- simple_ensemble() %>%
#'   add_member(two_class_res[1:3, ], metric = "roc_auc")
#' test_ens <- calib_class_thresh(test_ens, class_thresh = "tss_max")
#' test_ens <- calib_class_thresh(test_ens, class_thresh = "kap_max")
#' test_ens <- calib_class_thresh(test_ens, class_thresh = c("sens", 0.9))
#' collect_class_thresh(test_ens)
#' # for a repeat ensemble
#' rep_ens_calib <- calib_class_thresh(lacerta_rep_ens,
#'   class_thresh = "tss_max")
#' collect_class_thresh(rep_ens_calib)
#' @export
#' @keywords predict
calib_class_thresh <- function(object, class_thresh,
                               metric_thresh = NULL) {
  UseMethod("calib_class_thresh", object = object)
}

#' @rdname calib_class_thresh
#' @export
calib_class_thresh.default <- function(object, class_thresh,
                                       metric_thresh = NULL) {
  stop("no method available for this object type")
}

#' @rdname calib_class_thresh
#' @export
calib_class_thresh.simple_ensemble <- function(object,
                                               class_thresh,
                                               metric_thresh = NULL) {
  # check that there is no entry for this calibration
  if (!is.null(attr(object, "class_thresholds", exact = TRUE))) {
    ref_calib_tb <- attr(object, "class_thresholds", exact = TRUE)
    if (any(unlist(
      lapply(
        ref_calib_tb %>% dplyr::pull("metric_thresh"),
        identical,
        metric_thresh
      )
    ) &
      unlist(
        lapply(
          ref_calib_tb %>% dplyr::pull("class_thresh"),
          identical,
          class_thresh
        )
      ))) {
      message(
        "this ensemble is already calibrated for this combination of ",
        "`class_thresh` and `metric_thresh`"
      )
      return(object)
    }
  }

  fun_names <- c("mean", "median", "weighted_mean", "weighted_median")
  # generate predictions from the training data
  training_preds <- stats::predict(
    object,
    new_data = workflows::extract_mold(object$workflow[[1]])$predictors,
    type = "prob",
    fun = fun_names,
    class_thresh = class_thresh,
    metric_thresh = metric_thresh
  )
  # extract the truth from the training data
  training_outcomes <-
    workflows::extract_mold((object$workflow[[1]]))$outcome %>% dplyr::pull(1)

  # get the thresholds for each model
  calib_tb <- tibble::tibble(
    class_thresh = list(),
    metric_thresh = list(),
    fun = character(),
    optim_value = numeric()
  )
  for (i_col in seq.int(ncol(training_preds))) {
    optim_value <- optim_thresh(training_outcomes,
      training_preds[, i_col],
      metric = class_thresh
    )
    calib_tb <- calib_tb %>%
      dplyr::bind_rows(
        tibble::tibble(
          class_thresh = list(class_thresh),
          metric_thresh = list(metric_thresh),
          fun = fun_names[i_col],
          optim_value = optim_value
        )
      )
  }

  # now store the new thresholds
  if (is.null(attr(object, "class_thresholds", exact = TRUE))) {
    attr(object, "class_thresholds") <- calib_tb
  } else {
    attr(object, "class_thresholds") <-
      attr(object, "class_thresholds", exact = TRUE) %>%
      dplyr::bind_rows(calib_tb)
  }
  object
}

#' @rdname calib_class_thresh
#' @export
calib_class_thresh.repeat_ensemble <- function(object,
                                               class_thresh,
                                               metric_thresh = NULL) {
  # cycle over the repeats and calibrate each simple ensemble
  repeat_ids <- unique(object$rep_id)
  # if we don't have a class_thresholds_list attribute, we will create it;
  # otherwise, we will add to it
  if (is.null(attr(object, "class_thresholds_list", exact = TRUE))) {
    attr(object, "class_thresholds_list") <- list()
  }
  for (i_rep in repeat_ids) {
    object_rep <- get_repeat(object, i_rep)
    object_rep <- calib_class_thresh(
      object_rep,
      class_thresh = class_thresh,
      metric_thresh = metric_thresh
    )
    
    # Store the updated calibration table for this repeat.
    # Do not bind rows here: calib_class_thresh.simple_ensemble()
    # already appends new calibrations when needed and returns unchanged
    # thresholds when the calibration already exists.
    attr(object, "class_thresholds_list")[[i_rep]] <-
      attr(object_rep, "class_thresholds", exact = TRUE)
  }
  object
}
