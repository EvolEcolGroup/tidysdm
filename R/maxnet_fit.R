#' Wrapper to fit maxnet models with formulae
#'
#' This function is a wrapper around [maxnet::maxnet], which takes a formula
#' with data as well exposing parameters for normalisation in a manner
#' compatible with `parsnip`. Users are unlikely to use this function directly.
#' For the `parsnip` model specification for MaxEnt, see [maxent()].
#'
#' The response needs to be a factor with the class representing presences as
#' the reference level of the factor (as expected by other classification
#' models). A good guide to how options of a Maxent model work can be found in
#' https://onlinelibrary.wiley.com/doi/full/10.1111/j.1600-0587.2013.07872.x
#'
#' @param formula a formula defining the outcome and the predictors
#' @param data a data.frame with the outcomes and predictors
#' @param regmult numeric, a constant to adjust regularization
#' @param regfun function, computes regularization constant for each feature
#' @param addsamplestobackground logical, if TRUE then add to the background any
#'   presence sample that is not already there
#' @param classes character, continuous feature classes desired, either
#'   "default" or any subset of "lqpht" (for example, "lh")
#' @param ... currently not used.
#' @returns Maxnet returns an object of class \code{maxnet}, which is a list
#'   consisting of a glmnet model with the following elements added:
#' \describe{
#'  \item{betas}{ nonzero coefficients of the fitted model }
#'  \item{alpha}{ constant offset making the exponential model sum to one
#'   over the background data }
#'  \item{entropy}{ entropy of the exponential model }
#'  \item{penalty.factor}{ the regularization constants used for each feature }
#'  \item{featuremins}{ minimum of each feature, to be used for clamping }
#'  \item{featuremaxs}{ maximum of each feature, to be used for clamping }
#'  \item{varmin}{ minimum of each predictor, to be used for clamping }
#'  \item{varmax}{ maximum of each predictor, to be used for clamping }
#'  \item{samplemeans}{ mean of each predictor over samples (majority for factors) }
#'  \item{levels}{ levels of each predictor that is a factor }
#' }
#' @examples
#' \donttest{
#' # we repeat the example in the `maxnet` package
#' data("bradypus", package = "maxnet")
#' bradypus_tb <- tibble::as_tibble(bradypus) %>%
#'   dplyr::mutate(presence = relevel(
#'     factor(
#'       dplyr::case_match(presence, 1 ~ "presence", 0 ~ "absence")
#'     ),
#'     ref = "presence"
#'   ))
#' mod <- maxnet_fit(presence ~ ., data = bradypus_tb, classes = "lq")
#' plot(mod, "tmp6190_ann")
#' }
#' @keywords internal
#' @export

maxnet_fit <- function(formula, data, regmult = 1.0, classes = "default",
                       regfun = maxnet::maxnet.default.regularization,
                       addsamplestobackground = TRUE, ...) {
  # extract the response and turn it into a numeric vector
  response <- data[, form_resp(stats::terms(formula, data = data))] %>%
    dplyr::pull(1)
  resp_levels <- levels(response)
  response <- dplyr::case_match(response, resp_levels[1] ~ 1, resp_levels[2] ~ 0)
  # extract the responses
  predictors <- data[, rsample::form_pred(stats::terms(formula, data = data))]

  maxnet_obj <- maxnet::maxnet(
    p = response,
    data = predictors,
    f = maxnet::maxnet.formula(
      p = response,
      data = predictors,
      classes = classes
    ),
    regmult = regmult,
    regfun = regfun,
    addsamplestobackground = addsamplestobackground
  )

  return(maxnet_obj)
}


#' Wrapper to predict maxnet models
#'
#' This function is a wrapper around the `predict` method for [maxnet::maxnet],
#' making the function compatible with
#' `parsnip`. Users are unlikely to use this function directly.  For the
#'  `parsnip` model specification for MaxEnt, see [maxent()].
#'
#' @param object the [maxnet::maxnet] object
#' @param newdata the dataframe of new data
#' @param type either "prob" or "class"
#' @param maxnet_type the transformation used for the prediction
#' @param clamp logical, defining whether clamping to observed ranges should
#'  be used
#' @returns a tibble of predictions
#' @export
#' @keywords internal
maxnet_predict <- function(object, newdata, type = c("class", "prob"),
                           maxnet_type = c(
                             "cloglog", "link", "exponential",
                             "logistic"
                           ),
                           clamp = TRUE) {
  type <- match.arg(type)
  maxnet_type <- match.arg(maxnet_type)

  prob_vect <- stats::predict(object,
    newdata = as.data.frame(newdata),
    type = maxnet_type, clamp = clamp
  )
  if (type == "class") {
    class_vect <- rep(0, length(prob_vect))
    class_vect[prob_vect >= 0.5] <- 1
    return(class_vect)
  } else {
    return(prob_vect)
  }
}
