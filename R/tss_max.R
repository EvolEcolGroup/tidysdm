#' Maximum TSS - True Skill Statistics
#'
#' The True Skills Statistic, which is defined as
#'
#' *sensitivity*+*specificity* +1
#'
#' This function calibrates the probability threshold to classify presences to maximise the
#' TSS.
#'
#' There is no multiclass version of this function, it only operates on binary
#' predictions (e.g. presences and absences in SDMs).
#'
#' @param data Either a data.frame containing the columns specified by the truth
#' and estimate arguments, or a table/matrix where the true class
#' results should be in the columns of the table.
#' @param ... A set of unquoted column names or one or more dplyr selector functions to choose which variables contain the class probabilities. If truth is binary, only 1 column should be selected, and it should correspond to the value of event_level. Otherwise, there should be as many columns as factor levels of truth and the ordering of the columns should be the same as the factor levels of truth.
#' @param truth The column identifier for the true class results (that is a factor). This should be an unquoted column name although this argument is passed by expression and supports quasiquotation (you can unquote column names). For _vec() functions, a factor vector.
#' @param estimator One of "binary", "hand_till", "macro", or "macro_weighted" to specify the type of averaging to be done. "binary" is only relevant for the two class case. The others are general methods for calculating multiclass metrics. The default will automatically choose "binary" if truth is binary, "hand_till" if truth has >2 levels and case_weights isn't specified, or "macro" if truth has >2 levels and case_weights is specified (in which case "hand_till" isn't well-defined).
#' @param na_rm A logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param event_level A single string. Either "first" or "second" to specify which level of truth to consider as the "event". This argument is only applicable when estimator = "binary". The default uses an internal helper that generally defaults to "first"
#' @param case_weights The optional column identifier for case weights. This should be an unquoted column name that evaluates to a numeric column in data. For _vec() functions, a numeric vector.
#' @param estimate If truth is binary, a numeric vector of class probabilities corresponding to the "relevant" class. Otherwise, a matrix with as many columns as factor levels of truth. It is assumed that these are in the same order as the levels of truth.
#' @returns A tibble with columns .metric, .estimator, and .estimate and 1 row of values.
#' For grouped data frames, the number of rows returned will be the same as the
#' number of groups.
#' @family class probability metrics
#'
#' @examples
#' tss_max(two_class_example, truth, Class1)
#'
#' @export
tss_max <- function(data, ...) {
  UseMethod("tss_max")
}
tss_max <- yardstick::new_prob_metric(
  tss_max,
  direction = "maximize"
)

#' @rdname tss_max
#' @export
tss_max.data.frame <- function(data,
                               truth,
                               ...,
                               estimator = NULL,
                               na_rm = TRUE,
                               event_level = "first",
                               case_weights = NULL) {
  yardstick::prob_metric_summarizer(
    name = "tss_max",
    fn = tss_max_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    ...,
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !!rlang::enquo(case_weights),
    event_level = event_level
  )
}

#' @rdname tss_max
#' @export
tss_max.sf <- function(data, ...) {
  data %>%
    dplyr::as_tibble() %>%
    tss_max(...)
}


#' @export
#' @rdname tss_max
tss_max_vec <- function(truth,
                        estimate,
                        estimator = NULL,
                        na_rm = TRUE,
                        event_level = "first",
                        case_weights = NULL,
                        ...) {
  abort_if_class_pred(truth)

  estimator <- yardstick::finalize_estimator(truth, estimator, "tss_max")

  yardstick::check_prob_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  tss_max_estimator_impl(
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    event_level = event_level,
    case_weights = case_weights
  )
}

tss_max_estimator_impl <- function(truth,
                                   estimate,
                                   estimator,
                                   event_level,
                                   case_weights) {
  if (!identical(estimator, "binary")) {
    stop("tss_max is only available for binary classes; multiclass is not supported")
  }
  # separate estimates into presences and background
  if (identical(event_level, "first")) {
    pres_level <- levels(truth)[1]
    absence_level <- levels(truth)[2]
  } else {
    pres_level <- levels(truth)[2]
    absence_level <- levels(truth)[1]
  }
  presences <- estimate[truth == pres_level]
  absences <- estimate[truth == absence_level]

  # TODO we could implement case weights by properly fitting TSS from yardstick
  if (!is.null(case_weights)) {
    stop("tss_max with case_weights has not been implemented yet")
  }

  conf_matrix_df <- conf_matrix_df(presences, absences)
  sens <- (conf_matrix_df$tp / (conf_matrix_df$tp + conf_matrix_df$fn))
  spec <- (conf_matrix_df$tn / (conf_matrix_df$tn + conf_matrix_df$fp))

  tss <- (sens + spec) - 1
  max(tss) ## return the maximum TSS
}
