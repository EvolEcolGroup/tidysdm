#' Predict for a repeat ensemble set
#'
#' Predict for a new dataset by using a repeat ensemble. Predictions from
#' individual models are combined according to `fun`: if a weighted function is
#' used (`weighted_mean` or `weighted_median`), weights are based on the metric
#' used to tune models in the ensemble (see [`repeat_ensemble`]).
#' @param object an repeat_ensemble object
#' @param new_data a data frame in which to look for variables with which to
#'   predict.
#' @param type the type of prediction, "prob" or "class".
#' @param fun string defining the aggregating function. It can take values
#'   `mean`, `median`, `weighted_mean`, `weighted_median` and `none`. It is
#'   possible to combine multiple functions, except for "none". If it is set to
#'   "none", only the individual member predictions are returned.
#' @param class_fun the function to use to combine class predictions across
#'   repeats. It can be "majority" (the class with the highest proportion across
#'   repeats is predicted) or "prop" (the proportion of the "presence" class
#'   across repeats is returned). This argument is only used if `type` is
#'   "class".
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
#'   repeat level). The columns for individual members have the name of the
#'   workflow as a prefix, separated by "." from the usual column names of the
#'   predictions.
#' @param ... not used in this method.
#' @returns a tibble of predictions
#' @method predict repeat_ensemble
#' @export
#' @keywords predict
#' @examples
#' # we need a dataset to predict, we extract it from one of the models
#' new_data_ex <- workflowsets::extract_mold(
#'   lacerta_rep_ens$workflow[[1]])$predictors
#' ens_pred <- predict(lacerta_rep_ens,
#'   new_data = new_data_ex,
#'   fun = c("mean", "weighted_mean", "median")
#' )
#' head(ens_pred)
#' # set class thresholds for binary prediction
#' lacerta_rep_ens_calib <- calib_class_thresh(lacerta_rep_ens,
#'   class_thresh = c("tss_max"))
#' ens_class_pred <- predict(lacerta_rep_ens_calib, new_data = new_data_ex,
#'   fun = c("mean", "median"), type = "class", class_thresh = c("tss_max"))
#' head(ens_class_pred)
predict.repeat_ensemble <-
  function(object,
           new_data,
           type = "prob",
           fun = "mean",
           class_fun = c("majority", "prop"),
           metric_thresh = NULL,
           class_thresh = NULL,
           by_repeat = FALSE,
           ...) {
    # check that fun is not "none" if length >1
    if (length(fun) > 1 && "none" %in% fun) {
      stop("if 'fun' has length >1, it cannot be 'none'")
    }

    class_fun <- match.arg(class_fun)

    # we add names of the workflows to combine with the repeat ids
    repeat_ids <- unique(object$rep_id)
    
    pred_all = NULL
    valid_repeats = c()
    
    # now predict for each simple ensemble
    for (i_rep in repeat_ids) {
      object_rep <- get_repeat(object, i = i_rep)
      
      # restore calibration attribute for this repeat
      calib_list <- attr(object, "class_thresholds_list", exact = TRUE)
      if (!is.null(calib_list[[i_rep]])) {
        attr(object_rep, "class_thresholds") <- calib_list[[i_rep]]
      }
      
      pred_rep <- tryCatch(
        {
        stats::predict(
        object_rep,
        new_data = new_data,
        type = type,
        fun = fun,
        metric_thresh = metric_thresh,
        class_thresh = class_thresh
      )
        },
      error = function(e){
        if (grepl("metric_threshold excludes all models", e$message)) {
          warning(
            paste(
              "Skipping repeat", i_rep,
              "because all models were excluded by metric_thresh"
            )
          )
          return(NULL)
        }
        
        stop(e)
      }
      )
      
      # skip failed repeats
      if (is.null(pred_rep)) {
        next
      }
      names(pred_rep) <- paste(i_rep, names(pred_rep), sep = ".")
      
      valid_repeats <- c(valid_repeats, i_rep)
      
      if (is.null(pred_all)) {
        pred_all <- pred_rep
      } else {
        pred_all <- pred_all %>% dplyr::bind_cols(pred_rep)
      }
    }
    
    # if ALL repeats failed
    if (is.null(pred_all)) {
      stop("All repeats were excluded by metric_thresh")
    }
    
    # return the individual repeat predictions if requested
    if (by_repeat || ("none" %in% fun)) {
      return(pred_all)
    }

    # combine predictions across repeats
    pred_rep_ensemble <- list()
    # if we are predicting probabilities
    if (type == "prob") {
      # if we have an aggregating function
      for (i_fun in fun) {
        # subset to columns for this function
        pred_this_fun <- pred_all[
          ,
          endsWith(names(pred_all), paste0(".", i_fun)),
          drop = FALSE
        ]
        i_rep_fun <- gsub("weighted_", "", i_fun)
        pred_rep_ensemble[[i_fun]] <- apply(pred_this_fun, 1,
                                            eval(parse(text = i_rep_fun)))
      }
    } else { # if we are predicting classes
      class_levels <- levels(pred_all[[1]])
      # if we have an aggregating function
      for (i_fun in fun) {
        # subset to columns for this function
        pred_this_fun <- pred_all[
          ,
          endsWith(names(pred_all), paste0(".", i_fun)),
          drop = FALSE
        ]
        this_pred <- rowSums(pred_this_fun == "presence") /
          ncol(pred_this_fun)


        # compute the proportion of suitable classes across repeats for each
        # observation, and then apply the class_fun to get the final prediction
        if (class_fun == "majority") {
          this_pred <- factor(
            ifelse(this_pred > 0.5, class_levels[1], class_levels[2]),
            levels = class_levels
          )
        } else {
          if (class_levels[2] != "presence") {
            # flip the proportion if the "presence" class is the second level
            this_pred <- 1 - this_pred
          }
        }
        pred_rep_ensemble[[paste(i_fun, class_fun, sep = ".")]] <- this_pred
      }
    }
    pred_rep_ensemble <- data.frame(pred_rep_ensemble)
    return(pred_rep_ensemble)
  }
