#' Predict for a simple ensemble set
#'
#' Predict for a new dataset by using a simple ensemble. Predictions from
#' individual models (i.e. workflows) are combined according to `fun`
#' @param object an simple_ensemble object
#' @param new_data a data frame in which to look for variables with which to
#'   predict. If `NULL`, the predictors from the first workflow in the ensemble
#'   are used; note that this only makes sense if all workflows have the same
#'   predictors.
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
#' @method predict simple_ensemble
#' @export
#' @keywords predict

predict.simple_ensemble <-
  function(object,
           new_data = NULL,
           type = "prob",
           fun = "mean",
           metric_thresh = NULL,
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
        # warning if we return class without calibration
        message("as no 'threshold' was defined, a default of 0.5 will be used")
        class_thresh <- 0.5
      }

      # if the class_threshold is not numeric, then we need to ensure the model
      # has been calibrated for the right metric
      if (!is.numeric(class_thresh)) {
        # check that we have an entry for this calibration
        ref_calib_tb <- attr(object, "class_thresholds")
        # give up immediately if ref_calib_tb is null
        if (is.null(ref_calib_tb)) {
          stop(
            "this model needs to be first calibrated before classes can ",
            "be produced\n",
            "use 'calib_class_thresh()' first"
          )
        } else if (!any(unlist(lapply(
          ref_calib_tb %>%
            dplyr::pull("metric_thresh"),
          identical,
          metric_thresh
        )) &
          unlist(lapply(
            ref_calib_tb %>%
              dplyr::pull("class_thresh"),
            identical,
            class_thresh
          )))) {
          stop(
            "this model needs to be first calibrated before classes can ",
            "be produced\n",
            "use 'calib_class_thresh()' first"
          )
        }


        # subset the calibration thresholds
        ref_calib_tb <- ref_calib_tb[
          (unlist(lapply(
            ref_calib_tb %>%
              dplyr::pull("metric_thresh"),
            identical,
            metric_thresh
          )) &
            unlist(lapply(
              ref_calib_tb %>%
                dplyr::pull("class_thresh"),
              identical,
              class_thresh
            ))) &
            ref_calib_tb$fun %in% fun,
        ]
      } else { # if class_thresh is numeric, fake a calib table
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
    have_fun <-
      TRUE # boolean determining whether we have an aggregating function
    if (inherits(fun, "character")) {
      # check that we have valid values
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
        members <-
          TRUE # without an aggregating function, we want the member predictions
      }
    } else {
      stop(
        "fun should be either 'none', or a combination of 'mean', ",
        "'median', 'weighted_mean', and 'weighted_median'"
      )
    }

    # if there is no data, grab it from the first workflow
    if (is.null(new_data)) {
      new_data <- workflowsets::extract_mold(object$workflow[[1]])$predictors
    }

    # create list of predictions
    pred_list <-
      lapply(object$workflow,
        stats::predict,
        new_data = new_data,
        type = "prob"
      )
    names(pred_list) <- object$wflow_id
    # turn it into a data.frame
    pred_list <- dplyr::bind_cols(data.frame(pred_list)) %>% tibble::as_tibble()
    # remove every other column (so that we the probability for presences)
    pred_list <- pred_list[, seq(1, ncol(pred_list), by = 2)]

    # filter models if we have a metric_thresh (and create weights based on this
    # metric)
    if (!is.null(metric_thresh)) {
      if (!metric_thresh[1] %in% attr(object, "metrics")) {
        stop(
          "'metric_thresh' is not among the metrics estimated ",
          "for this ensemble"
        )
      }
      # values for this metric
      metric_ens <-
        dplyr::bind_rows(object$metrics) %>%
        dplyr::filter(.data$.metric == metric_thresh[1])
      # subset the data.frame
      pred_list <-
        pred_list[, metric_ens$mean > as.numeric(metric_thresh[2])]
      metric_ens <- metric_ens$mean[
        metric_ens$mean > as.numeric(metric_thresh[2])
      ]
      if (length(pred_list) == 0) {
        stop("the current metric_threshold excludes all models")
      }
    } else {
      # we create weights based on the best_metric used to choose models in
      # workflows
      metric_ens <-
        dplyr::bind_rows(object$metrics) %>%
        dplyr::filter(.data$.metric == attr(object, "best_metric")) %>%
        dplyr::pull(mean)
    }

    # define the weighted functions
    weighted_mean <- function(x, w = metric_ens) { # nolint
      stats::weighted.mean(x, weights = w)
    }

    weighted_median <- function(x, w = metric_ens) { # nolint
      w <- w[order(x)]
      x <- x[order(x)]
      prob <- cumsum(w) / sum(w)
      ps <- which(abs(prob - .5) == min(abs(prob - .5)))
      return(x[ps])
    }

    # if we have an aggregating function
    if (have_fun) {
      pred_ensemble <- list()
      for (i_fun in fun) {
        pred_ensemble[[i_fun]] <- apply(pred_list, 1, eval(parse(text = i_fun)))

        # convert to classes
        if (type == "class") {
          pred_ensemble[[i_fun]] <- prob_to_binary(pred_ensemble[[i_fun]],
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

    # add predictions for individual members if requested
    if (members) {
      if (!is.null(pred_ensemble)) {
        # if we already have an aggregated prediction
        pred_ensemble <-
          pred_ensemble %>% dplyr::bind_cols(data.frame(pred_list))
      } else {
        # if fun=="none" we just return the member predictions
        pred_ensemble <- data.frame(pred_list)
      }
    }

    return(pred_ensemble)
  }


#' simple function to convert probability to binary classes
#' @param x a vector of probabilities
#' @param thresh the threshold to convert to binary
#' @param class_levels the binary levels
#' @returns a vector of binary values
#' @keywords internal

prob_to_binary <- function(x, thresh, class_levels) {
  classes_bin <- rep(class_levels[2], length(x))
  classes_bin[x >= thresh] <- class_levels[1]
  stats::relevel(factor(classes_bin), ref = class_levels[1])
}
