#' Boyce continuous index (BCI)
#'
#' This function the Boyce Continuous Index, a measure of model accuracy appropriate
#' for Species Distribution Models with presence only data (i.e. using pseudoabsences
#' or background). The algorithm used here comes from the package `enmSdm`, and uses multiple
#' overlapping windows.
#'
#' There is no multiclass version of this function, it only operates on binary
#' predictions (e.g. presences and absences in SDMs).
#'
#' @param data Either a data.frame containing the columns specified by the truth
#' and estimate arguments, or a table/matrix where the true class
#' results should be in the columns of the table.
#' @param ... A set of unquoted column names or one or more dplyr selector functions to choose which variables contain the class probabilities. If truth is binary, only 1 column should be selected, and it should correspond to the value of event_level. Otherwise, there should be as many columns as factor levels of truth and the ordering of the columns should be the same as the factor levels of truth.
#' @param truth The column identifier for the true class results (that is a factor). This should be an unquoted column name although this argument is passed by expression and supports quasiquotation (you can unquote column names). For ⁠_vec()⁠ functions, a factor vector.
#' @param estimator One of "binary", "hand_till", "macro", or "macro_weighted" to specify the type of averaging to be done. "binary" is only relevant for the two class case. The others are general methods for calculating multiclass metrics. The default will automatically choose "binary" if truth is binary, "hand_till" if truth has >2 levels and case_weights isn't specified, or "macro" if truth has >2 levels and case_weights is specified (in which case "hand_till" isn't well-defined).
#' @param na_rm A logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param event_level A single string. Either "first" or "second" to specify which level of truth to consider as the "event". This argument is only applicable when estimator = "binary". The default uses an internal helper that generally defaults to "first"
#' @param case_weights The optional column identifier for case weights. This should be an unquoted column name that evaluates to a numeric column in data. For ⁠_vec()⁠ functions, a numeric vector.
#' @param estimate If truth is binary, a numeric vector of class probabilities corresponding to the "relevant" class. Otherwise, a matrix with as many columns as factor levels of truth. It is assumed that these are in the same order as the levels of truth.
#' @returns A tibble with columns .metric, .estimator, and .estimate and 1 row of values.
#' For grouped data frames, the number of rows returned will be the same as the
#' number of groups.
#' @family class probability metrics
#'
#' @references
#' Boyce, M.S., P.R. Vernier, S.E. Nielsen and F.K.A. Schmiegelow. 2002.
#' Evaluating resource selection functions. Ecol. Model., 157, 281-300.
#'
#' Hirzel, A.H., G. Le Lay, V. Helfer, C. Randin and A. Guisan. 2006.
#' Evaluating the ability of habitat suitability models to predict
#' species presences. Ecol. Model., 199, 142-152.
#'
#' @examples
#' boyce_cont(two_class_example, truth, Class1)
#'
#' @export
boyce_cont <- function(data, ...) {
  UseMethod("boyce_cont")
}
boyce_cont <- new_prob_metric(
  boyce_cont,
  direction = "maximize"
)

#' @rdname boyce_cont
#' @export
boyce_cont.data.frame <- function(data,
                               truth,
                               ...,
                               estimator = NULL,
                               na_rm = TRUE,
                               event_level = "first",
                               case_weights = NULL) {
  prob_metric_summarizer(
    name = "boyce_cont",
    fn = boyce_cont_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    ...,
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !!rlang::enquo(case_weights),
    event_level = event_level
  )
}

#' @rdname boyce_cont
#' @export
boyce_cont.sf <- function(data,...){
  data %>% dplyr::as_tibble() %>% boyce_cont(...)
}


#' @export
#' @rdname boyce_cont
boyce_cont_vec <- function(truth,
                                  estimate,
                                  estimator = NULL,
                                  na_rm = TRUE,
                                  event_level = "first",
                                  case_weights = NULL,
                                  ...) {
  utils::getFromNamespace("abort_if_class_pred", "yardstick")(truth)

  estimator <- yardstick::finalize_estimator(truth, estimator, "boyce_cont")

  yardstick::check_prob_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  boyce_cont_estimator_impl(
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    event_level = event_level,
    case_weights = case_weights
  )
}

boyce_cont_estimator_impl <- function(truth,
                                             estimate,
                                             estimator,
                                             event_level,
                                             case_weights) {
  if (!utils::getFromNamespace("is_binary", "yardstick")(estimator)) {
    stop("boyce_cont is only available for binary classes; multiclass is not supported")
  }
  # separate estimates into presences and background
  if (identical(event_level, "first")) {
    pres_level <- levels(truth)[1]
    contrast_level <- levels(truth)[2]
  } else {
    pres_level <- levels(truth)[2]
    contrast_level <- levels(truth)[1]
  }
  pres <- estimate[truth==pres_level]
  contrast <- estimate[truth==contrast_level]
  if (!is.null(case_weights)){
  presWeight <- case_weights[truth==pres_level]
  contrastWeight <- case_weights[truth==contrast_level]
  } else {
    presWeight = rep(1, length(pres))
    contrastWeight = rep(1, length(contrast))
  }
  contBoyce(pres = pres, contrast = contrast, presWeight = presWeight, contrastWeight = contrastWeight)
}


# copy of contBoyce from enmSdm

contBoyce <- function(
    pres,
    contrast,
    presWeight = rep(1, length(pres)),
    contrastWeight = rep(1, length(contrast)),
    numBins = 101,
    binWidth = 0.1,
    autoWindow = TRUE,
    method = 'spearman',
    dropZeros = TRUE,
    #graph = FALSE,
    na.rm = FALSE,
    ...
) {

  # if all NAs
  if (all(is.na(pres)) | all(is.na(contrast)) | all(is.na(presWeight)) | all(is.na(contrastWeight))) return(NA)

  # catch errors
  if (binWidth > 1 | binWidth <= 0) stop('Argument "binWidth" must be between 0 and 1.')

  eps <- .Machine$double.eps

  # right hand side of each class (assumes max value is >0)
  lowest <- if (autoWindow) { min(c(pres, contrast), na.rm=na.rm) } else { 0 }
  highest <- if (autoWindow) { max(c(pres, contrast), na.rm=na.rm) + eps } else { 1 + eps }



  windowWidth <- binWidth * (highest - lowest)

  lows <- seq(lowest, highest - windowWidth, length.out=numBins)
  highs <- seq(lowest + windowWidth + eps, highest, length.out=numBins)

  ##########
  ## MAIN ##
  ##########

  ## initiate variables to store predicted/expected (P/E) values
  freqPres <- freqContrast <- rep(NA, length(numBins))

  ### tally proportion of test presences/background sites in each class
  for (countClass in 1:numBins) {

    # number of presence predictions in this class
    presInBin <- pres >= lows[countClass] & pres < highs[countClass]
    presInBin <- presInBin * presWeight
    freqPres[countClass] <- sum(presInBin, na.rm=na.rm)

    # number of background predictions in this class
    bgInBin <- contrast >= lows[countClass] & contrast < highs[countClass]
    bgInBin <- bgInBin * contrastWeight
    freqContrast[countClass] <- sum(bgInBin, na.rm=na.rm)

  } # next predicted value class

  # mean bin prediction
  meanPred <- rowMeans(cbind(lows, highs))

  # add small number to each bin that has 0 background frequency but does have a presence frequency > 0
  if (any(freqPres > 0 & freqContrast == 0)) {
    smallValue <- min(0.5 * c(presWeight[presWeight > 0], contrastWeight[contrastWeight > 0]))
    freqContrast[freqPres > 0 & freqContrast == 0] <- smallValue
  }

  # remove classes with 0 presence frequency
  if (dropZeros && 0 %in% freqPres) {
    zeros <- which(freqPres == 0)
    meanPred[zeros] <- NA
    freqPres[zeros] <- NA
    freqContrast[zeros] <- NA
  }

  # remove classes with 0 background frequency
  if (any(0 %in% freqContrast)) {
    zeros <- which(freqContrast == 0)
    meanPred[zeros] <- NA
    freqPres[zeros] <- NA
    freqContrast[zeros] <- NA
  }

  P <- freqPres / sum(presWeight, na.rm=TRUE)
  E <- freqContrast / sum(contrastWeight, na.rm=TRUE)
  PE <- P / E

  # # plot (transfer the code to a boyce_curve function in the future)
  # if (graph) {
  #   graphics::par(mfrow=c(1, 2))
  #   lims <- c(0, max(P, E, na.rm=TRUE))
  #   plot(E, P, col='white', xlab='Expected', ylab='Predicted', main='P/E\nNumbered from lowest to highest class', xlim=lims, ylim=lims)
  #   graphics::text(E, P, labels=1:numBins, col=1:20)
  #   plot(meanPred, PE, type='l', xlab='Mean Prediction in Bin', ylab='P/E Ratio', main='CBI\nNumbered from lowest to highest class')
  #   graphics::text(meanPred, PE, labels=1:numBins, col='blue')
  # }

  # remove NAs
  na_in_either <- (is.na(meanPred) | is.na(PE))
  meanPred <- meanPred[!na_in_either]
  PE <- PE[!na_in_either]

  # calculate continuous Boyce index (cbi)
  cbi <- stats::cor(x=meanPred, y=PE, method=method)
  cbi

}

