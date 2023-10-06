calib_class_thresh <- function(object, class_thresh, metric_thresh = NULL) {
  # check that there is no entry for this calibration
  if (!is.null(attr(object, "class_thresholds"))) {

  }

  browser()
  fun_names <- c("mean", "median", "weighted_mean", "weighted_median")
  # generate predictions from the training data
  training_preds <- predict(object,
    new_data = extract_mold(object$workflow[[1]])$predictors,
    type = "prob", fun = fun_names,
    class_thresh = class_thresh, metric_thresh = metric_thresh
  )
  # extract the truth from the training data
  training_outcomes <- extract_mold((object$workflow[[1]]))$outcome %>% dplyr::pull(1)

  # get the thresholds for each model
  calib_tb <- tibble::tibble(
    class_thresh = list(),
    metric_thresh = list(),
    fun = character(),
    optim_value = numeric()
  )
  for (i_col in seq_along(ncol(training_preds))) {
    calib_tb <- calib_tb %>%
      dplyr::bind_rows(tibble::tibble(
        class_thresh = list(class_thresh),
        metric_thresh = list(metric_thresh),
        fun = fun_names[i_col],
        optim_value = optim_thresh(training_outcomes,
          training_preds[, i_col],
          metric = class_thresh
        )
      ))
  }

  # now store the new thresholds
  if (!is.null(attr(object, "class_thresholds"))) {
    attr(object, "class_thresholds") <- calib_tb
  } else {
    attr(object, "class_thresholds") <-
      attr(object, "class_thresholds") %>%
      bind_rows(calib_tb)
  }
  object
}

calib_class_thresh(test_ens, class_thresh = "tss_max")
calib_class_thresh(test_ens, class_thresh = c("sens", 0.9))


optim_thres_workflow <- function(object, metric, event_level = "first") {
  outcomes_model <- extract_mold(object)$outcome %>%
    bind_cols(predict(object, new_data = extract_mold(object)$predictors, type = "prob"))
  if (event_level == "first") {
    pred_col <- 2
  } else if (event_level == "second") {
    pred_col <- 3
  }
  return(optim_thresh(
    outcomes_model %>% dplyr::pull(1),
    outcomes_model %>% dplyr::pull(pred_col),
    metric, event_level
  ))
}

fitted_wflow <- fit_best(two_class_res)
optim_thres_workflow(fitted_wflow, metric = "tss_max")
