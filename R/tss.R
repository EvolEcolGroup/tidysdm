#' TSS - True Skill Statistics
#'
#' The True Skills Statistic, which is defined as
#'
#' *sensitivity*+*specificity* +1
#'
#' This function is a wrapper around [yardstick::j_index()], another name for the
#' same quantity. Note that this function takes the classes as predicted by the
#' model without any calibration (i.e. making a split at 0.5 probability). This
#' is usually not the metric used for Species Distribution Models, where the
#' threshold is recalibrated to maximise TSS; for that purpose, use [tss_max()].
#'
#' @param data Either a data.frame containing the columns specified by the truth
#' and estimate arguments, or a table/matrix where the true class
#' results should be in the columns of the table.
#' @param ... Not currently used.
#' @param truth The column identifier for the true class results (that is a factor). This should be an unquoted column name although this argument is passed by expression and supports quasiquotation (you can unquote column names). For ⁠_vec()⁠ functions, a factor vector.
#' @param estimate The column identifier for the predicted class results (that is also factor). As with truth this can be specified different ways but the primary method is to use an unquoted variable name. For ⁠_vec()⁠ functions, a factor vector.
#' @param estimator One of: "binary", "macro", "macro_weighted", or "micro" to specify the type of averaging to be done. "binary" is only relevant for the two class case. The other three are general methods for calculating multiclass metrics. The default will automatically choose "binary" or "macro" based on estimate.
#' @param na_rm A logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param case_weights The optional column identifier for case weights. This should be an unquoted column name that evaluates to a numeric column in data. For ⁠_vec()⁠ functions, a numeric vector.
#' @param event_level A single string. Either "first" or "second" to specify which level of truth to consider as the "event". This argument is only applicable when estimator = "binary". The default is "first".
#' @returns A tibble with columns .metric, .estimator, and .estimate and 1 row of values.
#' For grouped data frames, the number of rows returned will be the same as the
#' number of groups.
#' @examples
#' #Two class
#' data("two_class_example")
#' tss(two_class_example, truth, predicted)
#' # Multiclass
#' library(dplyr)
#' data(hpc_cv)
#' # Groups are respected
#' hpc_cv %>%
#'  group_by(Resample) %>%
#'  tss(obs, pred)
#' @export
tss <- function(data, ...) {
  UseMethod("tss")
}
tss <- new_class_metric(
  tss,
  direction = "maximize"
)

#' @rdname tss
#' @export
tss.data.frame <- function(data,
                               truth,
                               estimate,
                               estimator = NULL,
                               na_rm = TRUE,
                               case_weights = NULL,
                               event_level = "first",
                               ...) {
  class_metric_summarizer(
    name = "tss",
    fn = yardstick::j_index_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !!rlang::enquo(case_weights),
    event_level = event_level
  )
}

