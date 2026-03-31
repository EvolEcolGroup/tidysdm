#' Predict for a repeat ensemble set
#'
#' Predict for a new dataset by using a repeat ensemble. Predictions from
#' individual models are combined according to `fun`: if a
#' weighted function is used (`weighted_mean` or `weighted_median`), weights are
#' based on the metric used to tune models in the ensemble (see
#' [`repeat_ensemble`]).
#' @param object an repeat_ensemble object
#' @param new_data a data frame in which to look for variables with which to
#'   predict.
#' @param type the type of prediction, "prob" or "class".
#' @param fun string defining the aggregating function. It can take values
#'   `mean`, `median`, `weighted_mean`, `weighted_median` and `none`. It is
#'   possible to combine multiple functions, except for "none". If it is set to
#'   "none", only the individual member predictions are returned.
#' @param metric_thresh a vector of length 2 giving a metric and its threshold,
#'   which will be used to prune which models in the ensemble will be used for
#'   the prediction. The 'metrics' need to have been computed when the workflow
#'   was tuned. Examples are c("accuracy",0.8) or c("boyce_cont",0.7)
#' @param class_thresh probability threshold used to convert probabilities into
#'   classes. It can be a number (between 0 and 1), or a character metric
#'   (currently "tss_max" or "sensitivity"). For sensitivity, an additional
#'   target value is passed along as a second element of a vector, e.g.
#'   c("sensitivity",0.8).
#' @param by_repeat boolean defining whether individual predictions for each
#'   repeat should be returned (no aggregating function will be applied at the
#'   repeat level). The columns for
#'   individual members have the name of the workflow a a prefix, separated by
#'   "." from the usual column names of the predictions.
#' @param ... not used in this method.
#' @returns a tibble of predictions
#' @method predict repeat_ensemble
#' @export
#' @keywords predict

predict.repeat_ensemble <-
  function(object,
           new_data,
           type = "prob",
           fun = "mean",
           metric_thresh = NULL,
           class_thresh = NULL,
           by_repeat = FALSE,
           ...) {
    # check that fun is not "none" if length >1
    if (length(fun) > 1 && "none" %in% fun) {
      stop("if 'fun' has length >1, it cannot be 'none'")
    }


    # we change the names of the workflows to combine with the repeat ids
    object$workflow_id <- paste(object$rep_id, object$wflow_id, sep = ".")
    class(object)[1] <- "simple_ensemble"
    repeat_ids <- unique(object$rep_id)
    # now predict for each simple ensemble
    for (i_rep in repeat_ids) {
      object_rep <- object %>% dplyr::filter(.data$rep_id == i_rep)
      pred_rep <- predict(
        object_rep,
        new_data = new_data,
        type = type,
        fun = fun,
        metric_thresh = metric_thresh,
        class_thresh = class_thresh
      )
      names(pred_rep) <- paste(i_rep, names(pred_rep), sep = ".")
      if (i_rep == repeat_ids[1]) {
        pred_all <- pred_rep
      } else {
        pred_all <- pred_all %>% dplyr::bind_cols(pred_rep)
      }
    }
    # return the individual repeat predictions if requested
    if (by_repeat || ("none" %in% fun)){
      return(pred_all)
    }

    # combine predictions across repeats
      pred_rep_ensemble <- list()
    # if we are predicting probabilities
    if (type == "prob") {
      # if we have an aggregating function
      for (i_fun in fun) {
        # subset to columns for this function
        pred_this_fun <- pred_all %>% dplyr::select(dplyr::contains(i_fun))
        i_rep_fun <- gsub("weighted_", "", i_fun)
        pred_rep_ensemble[[i_fun]] <- apply(pred_this_fun, 1, eval(parse(text = i_rep_fun)))

        # # convert to classes
        # if (type == "class") {
        #   pred_ensemble[[i_fun]] <- prob_to_binary(pred_ensemble[[i_fun]],
        #                                            thresh = ref_calib_tb %>%
        #                                              dplyr::filter(fun == i_fun) %>%
        #                                              dplyr::pull("optim_value"),
        #                                            class_levels = class_levels
        #   )
        # }
      }
      pred_rep_ensemble <- data.frame(pred_rep_ensemble)
    } else {
      # if we are predicting classes, we just take the majority vote across
      # repeats
    }

     return(pred_rep_ensemble)
}
