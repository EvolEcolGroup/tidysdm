#' Find threshold that optimises a given metric
#'
#' This function returns the threshold to turn probabilities into binary classes
#' whilst optimising a given metric. Currently available for [`tss_max`], [`kap_max`] and
#' `sensitivity` (for which a target sensitivity is required).
#' @param truth The column identifier for the true class results (that is a
#' factor). This should be an unquoted column name although this argument is
#' passed by expression and supports quasiquotation (you can unquote column
#' names). For ⁠_vec()⁠ functions, a factor vector.
#' @param estimate the predicted probability for the event
#' @param metric character of metric to be optimised. Currently only "tss_max",
#' "kap_max", and "sensitivity" with a given target
#' (e.g. c("sensitivity",0.8))
#' @param event_level A single string. Either "first" or "second" to specify
#' which level of truth to consider as the "event". This argument is only
#' applicable when estimator = "binary". The default uses an internal helper
#' that generally defaults to "first"
#' @returns the probability threshold for the event
#' @examples
#' optim_thresh(two_class_example$truth, two_class_example$Class1, metric = c("tss_max"))
#' optim_thresh(two_class_example$truth, two_class_example$Class1, metric = c("sens", 0.9))
#' @export

optim_thresh <- function(truth, estimate, metric, event_level = "first") {
  if (!is.factor(truth)) {
    stop("truth should be a factor!")
  }
  # separate estimates into presences and background
  if (identical(event_level, "first")) {
    pres_level <- levels(truth)[1]
    absence_level <- levels(truth)[2]
  } else if (identical(event_level, "second")) {
    pres_level <- levels(truth)[2]
    absence_level <- levels(truth)[1]
  } else {
    stop("event_level should be either 'first' or 'second'")
  }

  presences <- estimate[truth == pres_level]
  absences <- estimate[truth == absence_level]

  if (length(metric) == 2) {
    if (metric[1] %in% c("sensitivity", "sens")) {
      optim_thresh_sens(presences, absences, as.numeric(metric[2]))
    } else {
      "invalid metric"
    }
  } else if (metric == "tss_max") {
    optim_thresh_tss_max(presences, absences)
  } else if (metric == "kap_max") {
    optim_thresh_kap_max(presences, absences)
  } else {
    "invalid metric"
  }
}


#' Find threshold that gives a target sensitivity
#'
#' This is an internal function returns the threshold to turn probabilities into binary classes
#' for a given target sensitivity
#' @param presences Probabilities for presences.
#' @param absences Provabilities for absences
#' @param sens_target the target sensitivity
#' @returns the probability threshold for the event
#' @keywords internal
optim_thresh_sens <- function(presences, absences, sens_target) {
  conf_matrix_df <- conf_matrix_df(presences, absences)
  sens <- (conf_matrix_df$tp / (conf_matrix_df$tp + conf_matrix_df$fn))
  return(conf_matrix_df$thresh[which.min(sens > sens_target)])
}

#' Find threshold that maximises TSS
#'
#' This is an internal function returns the threshold to turn probabilities into binary classes
#' to maximise TSS
#' @param presences Probabilities for presences.
#' @param absences Provabilities for absences
#' @returns the probability threshold for the event
#' @keywords internal
optim_thresh_tss_max <- function(presences, absences) {
  conf_matrix_df <- conf_matrix_df(presences, absences)
  sens <- (conf_matrix_df$tp / (conf_matrix_df$tp + conf_matrix_df$fn))
  spec <- (conf_matrix_df$tn / (conf_matrix_df$tn + conf_matrix_df$fp))

  tss <- (sens + spec) - 1
  return(conf_matrix_df$thresh[which.max(tss)])
}

#' Find threshold that maximises Kappa
#'
#' This is an internal function returns the threshold to turn probabilities into binary classes
#' to maximise kappa
#' @param presences Probabilities for presences.
#' @param absences Provabilities for absences
#' @returns the probability threshold for the event
#' @keywords internal
optim_thresh_kap_max <- function(presences, absences) {
  conf_matrix_df <- conf_matrix_df(presences, absences)
  n <- rowSums(conf_matrix_df[, 2:5])
  obs_accuracy <- (conf_matrix_df$tp + conf_matrix_df$tn) / n
  exp_accuracy <- (((conf_matrix_df$tn + conf_matrix_df$fp) *
    (conf_matrix_df$tn + conf_matrix_df$fn) / n) +
    ((conf_matrix_df$tp + conf_matrix_df$fn) *
      (conf_matrix_df$tp + conf_matrix_df$fp) / n)) / n
  kap <- (obs_accuracy - exp_accuracy) / (1 - exp_accuracy)
  return(conf_matrix_df$thresh[which.max(kap)])
}
