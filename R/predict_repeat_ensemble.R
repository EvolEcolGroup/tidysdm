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
#'   "none", only the individual member predictions are returned (this
#'   automatically sets `member` to TRUE)
#' @param metric_thresh a vector of length 2 giving a metric and its threshold,
#'   which will be used to prune which models in the ensemble will be used for
#'   the prediction. The 'metrics' need to have been computed when the workflow
#'   was tuned. Examples are c("accuracy",0.8) or c("boyce_cont",0.7)
#' @param class_thresh probability threshold used to convert probabilities into
#'   classes. It can be a number (between 0 and 1), or a character metric
#'   (currently "tss_max" or "sensitivity"). For sensitivity, an additional
#'   target value is passed along as a second element of a vector, e.g.
#'   c("sensitivity",0.8).
#' @param members boolean defining whether individual predictions for each
#'   member should be added to the ensemble prediction. The columns for
#'   individual members have the name of the workflow a a prefix, separated by
#'   "." from the usual column names of the predictions.
#' @param ... not used in this method.
#' @returns a tibble of predictions
#' @method predict repeat_ensemble
#' @export
#' @keywords predict

# predict.repeat_ensemble <-
#   function(object,
#            new_data,
#            type = "prob",
#            fun = "mean",
#            metric_thresh = NULL,
#            class_thresh = NULL,
#            members = FALSE,
#            ...) {
#     # we change the names of the workflows to combine with the repeat ids
#     object$workflow_id <- paste(object$rep_id, object$wflow_id, sep = ".")
#     class(object)[1] <- "simple_ensemble"
#     # now predict the object as if it was a simple ensemble
#     stats::predict(
#       object = object,
#       new_data = new_data,
#       type = type,
#       fun = fun,
#       metric_thresh = metric_thresh,
#       class_thresh = class_thresh,
#       members = members
#     )
#   }


# predict.repeat_ensemble <- function(object,
#                                     new_data,
#                                     type = "prob",
#                                     fun = "mean",
#                                     metric_thresh = NULL,
#                                     class_thresh = NULL,
#                                     members = FALSE,
#                                     ...) {
#   # store unique repeat ids
#   rep_ids <- unique(object$rep_id)
# 
#   # predict within each repeat (aggregate members within repeat)
#   pred_per_rep <- lapply(rep_ids, function(rep_id) {
#     obj_rep <- object[object$rep_id == rep_id, , drop = FALSE]
# 
#     # reuse simple_ensemble predict method
#     class(obj_rep)[1] <- "simple_ensemble"
# 
#     stats::predict(
#       object = obj_rep,
#       new_data = new_data,
#       type = "prob",
#       fun = fun,
#       metric_thresh = metric_thresh,
#       class_thresh = class_thresh,
#       ...
#     )
#   })
# 
#   # set names of the list to the unique repeat ids
#   names(pred_per_rep) <- rep_ids
# 
#   # average repeat level predictions across repeats (equal weight per repeat)
#   out <- lapply(fun, function(col) {
#     mat <- do.call(cbind, lapply(pred_per_rep, function(x) { x[[col]] }))
#     rowMeans(mat, na.rm = TRUE)
#   })
#   names(out) <- fun
# 
#   return(as.data.frame(out))
# }


predict.repeat_ensemble <- function(object,
                                    new_data,
                                    type = "prob",
                                    fun = "mean",
                                    metric_thresh = NULL,
                                    metric_thresh_repeat = NULL,
                                    class_thresh = NULL,
                                    members = FALSE,
                                    ...) {
  # type check
  if (!type %in% c("prob", "class")) {
    stop("'type' can only take values 'prob' or 'class'")
  }
  
  if (type == "class") {
    if (fun[1] == "none") {
      stop(
        "classes can be generated only if an aggregating ",
        "function is given"
      )
    }
    if (members) {
      message("classes are only provided for aggregated ensemble predictions")
      members <- FALSE
    }
    if (is.null(class_thresh)) {
      message("as no 'threshold' was defined, a default of 0.5 will be used")
      class_thresh <- 0.5
    }
    
    if (!is.numeric(class_thresh)) {
      ref_calib_tb <- attr(object, "class_thresholds")
      if (is.null(ref_calib_tb)) {
        stop(
          "this model needs to be first calibrated before classes can ",
          "be produced\n",
          "use 'calib_class_thresh()' first"
        )
      } else if (!any(unlist(lapply(
        ref_calib_tb %>% dplyr::pull("metric_thresh"),
        identical,
        metric_thresh_repeat
      )) &
      unlist(lapply(
        ref_calib_tb %>% dplyr::pull("class_thresh"),
        identical,
        class_thresh
      )))) {
        stop(
          "this model needs to be first calibrated before classes can ",
          "be produced\n",
          "use 'calib_class_thresh()' first"
        )
      }
      
      ref_calib_tb <- ref_calib_tb[
        (unlist(lapply(
          ref_calib_tb %>% dplyr::pull("metric_thresh"),
          identical,
          metric_thresh_repeat
        )) &
          unlist(lapply(
            ref_calib_tb %>% dplyr::pull("class_thresh"),
            identical,
            class_thresh
          ))) &
          ref_calib_tb$fun %in% fun,
      ]
    } else {
      ref_calib_tb <- tibble::tibble(
        fun = fun,
        optim_value = rep(class_thresh, length(fun))
      )
    }
    
    class_levels <- levels(
      workflows::extract_mold((object$workflow[[1]]))$outcome %>%
        dplyr::pull(1)
    )
  }
  
  # set up the aggregating function
  have_fun <- TRUE
  if (inherits(fun, "character")) {
    if (!(((length(fun) == 1 && fun[1] == "none")) ||
          all(fun %in% c(
            "mean", "median", "weighted_mean",
            "weighted_median"
          )))) {
      stop(
        "fun should be either 'none', or a combination of 'mean', ",
        "'median', 'weighted_mean', and 'weighted_median'"
      )
    }
    if (fun[1] == "none") {
      have_fun <- FALSE
      members <- TRUE
    }
  } else {
    stop(
      "fun should be either 'none', or a combination of 'mean', ",
      "'median', 'weighted_mean', and 'weighted_median'"
    )
  }
  
  # store unique repeat ids
  rep_ids <- unique(object$rep_id)
  
  # helper to get repeat-level metric values
  get_repeat_metric <- function(obj_rep, metric_name, metric_thresh_member = NULL) {
    metric_rep <-
      dplyr::bind_rows(obj_rep$metrics) %>%
      dplyr::filter(.data$.metric == metric_name)
    
    if (!is.null(metric_thresh_member)) {
      metric_rep <- metric_rep[metric_rep$mean > as.numeric(metric_thresh_member[2]), ]
    }
    
    if (nrow(metric_rep) == 0) {
      return(NA_real_)
    }
    
    mean(metric_rep$mean)
  }
  
  # predict within each repeat
  pred_per_rep <- list()
  metric_repeat <- numeric(length(rep_ids))
  
  for (i_rep in seq_along(rep_ids)) {
    rep_id <- rep_ids[i_rep]
    obj_rep <- object[object$rep_id == rep_id, , drop = FALSE]
    
    # reuse simple_ensemble predict method
    class(obj_rep)[1] <- "simple_ensemble"
    
    pred_per_rep[[i_rep]] <- stats::predict(
      object = obj_rep,
      new_data = new_data,
      type = "prob",
      fun = fun,
      metric_thresh = metric_thresh,
      class_thresh = class_thresh,
      members = members,
      ...
    )
    
    # repeat-level metric for filtering / weighting across repeats
    if (!is.null(metric_thresh_repeat)) {
      metric_repeat[i_rep] <-
        get_repeat_metric(
          obj_rep = obj_rep,
          metric_name = metric_thresh_repeat[1],
          metric_thresh_member = metric_thresh
        )
    } else {
      metric_repeat[i_rep] <-
        get_repeat_metric(
          obj_rep = obj_rep,
          metric_name = attr(object, "best_metric"),
          metric_thresh_member = metric_thresh
        )
    }
  }
  
  names(pred_per_rep) <- rep_ids
  
  # filter repeats if requested
  if (!is.null(metric_thresh_repeat)) {
    if (!metric_thresh_repeat[1] %in% attr(object, "metrics")) {
      stop(
        "'metric_thresh_repeat' is not among the metrics estimated ",
        "for this ensemble"
      )
    }
    
    keep_rep <- metric_repeat > as.numeric(metric_thresh_repeat[2])
    pred_per_rep <- pred_per_rep[keep_rep]
    metric_repeat <- metric_repeat[keep_rep]
    
    if (length(pred_per_rep) == 0) {
      stop("the current metric_thresh_repeat excludes all repeats")
    }
  }
  
  # weighted functions across repeats
  weighted_mean <- function(x, w = metric_repeat) { # nolint
    stats::weighted.mean(x, weights = w)
  }
  
  weighted_median <- function(x, w = metric_repeat) { # nolint
    w <- w[order(x)]
    x <- x[order(x)]
    prob <- cumsum(w) / sum(w)
    ps <- which(abs(prob - .5) == min(abs(prob - .5)))
    return(x[ps])
  }
  
  # aggregate across repeats
  if (have_fun) {
    pred_ensemble <- list()
    
    for (i_fun in fun) {
      mat <- do.call(cbind, lapply(pred_per_rep, function(x) x[[i_fun]]))
      pred_ensemble[[i_fun]] <- apply(mat, 1, eval(parse(text = i_fun)))
      
      if (type == "class") {
        pred_ensemble[[i_fun]] <- prob_to_binary(
          pred_ensemble[[i_fun]],
          thresh = ref_calib_tb %>%
            dplyr::filter(fun == i_fun) %>%
            dplyr::pull("optim_value"),
          class_levels = class_levels
        )
      }
    }
    
    pred_ensemble <- data.frame(pred_ensemble)
  } else {
    pred_ensemble <- NULL
  }
  
  # add repeat-level/member predictions if requested
  if (members) {
    member_pred <- lapply(seq_along(pred_per_rep), function(i_rep) {
      x <- pred_per_rep[[i_rep]]
      
      if (have_fun) {
        x <- x[, !names(x) %in% fun, drop = FALSE]
      }
      
      if (ncol(x) > 0) {
        names(x) <- paste0(names(pred_per_rep)[i_rep], ".", names(x))
      }
      
      x
    })
    
    member_pred <- dplyr::bind_cols(member_pred)
    
    if (!is.null(pred_ensemble)) {
      pred_ensemble <- dplyr::bind_cols(pred_ensemble, member_pred)
    } else {
      pred_ensemble <- member_pred
    }
  }
  
  return(pred_ensemble)
}









#' Predict for a repeat stack
predict.repeat_stack <- function(object, 
                                 new_data, 
                                 fun = "mean", 
                                 ...) {

  pred_per_rep <- lapply(object$.stack, function(s) {
    p <- stats::predict(s, new_data = new_data, type = "prob", ...)
    p[[".pred_presence"]]  # probability of "presence"
  })
  
  mat <- do.call(cbind, pred_per_rep)
  
  out <- lapply(fun, function(f) {
    if (f == "mean") {
      rowMeans(mat, na.rm = TRUE)
    } else if (f == "median") {
      apply(mat, 1, stats::median, na.rm = TRUE)
    } else {
      stop("fun must be 'mean' and/or 'median'")
    }
  })
  
  names(out) <- fun
  return(as.data.frame(out))
}


# TODO appply all the things from predict_simple_ensemble, such as weighted mean/median





