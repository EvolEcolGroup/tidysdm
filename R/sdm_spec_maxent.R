#' Model specification for a MaxEnt for SDM
#'
#' This function returns a [parsnip::model_spec] for a MaxEnt model to
#' be used in Species Distribution
#' Models.
#'
#' @param ... parameters to be passed to [maxent()] to
#' customise the model. See the help of that function for details.
#' @param tune character defining the tuning strategy. Valid strategies are:
#' * "sdm" chooses hyper-parameters that are most important to tune for
#' an sdm (for *maxent*, 'mtry')
#' * "all" tunes all hyperparameters (for *maxent*, 'mtry', 'trees' and 'min')
#' * "custom" passes the options from '...'
#' * "none" does not tune any hyperparameter
#' @returns a [parsnip::model_spec] of the model.
#' @examples
#' test_maxent_spec <- sdm_spec_maxent(tune = "sdm")
#' test_maxent_spec
#' # setting specific values
#' sdm_spec_maxent(tune = "custom", feature_classes = "lq")
#' @export

sdm_spec_maxent <- function(..., tune = c("sdm", "all", "custom", "none")) {
  tune <- rlang::arg_match(tune)
  if (tune == "sdm") {
    base_spec <- maxent(
      feature_classes = tune(),
      regularization_multiplier = tune(),
      ...
    )
  } else if (tune == "all") {
    base_spec <- maxent(
      feature_classes = tune(),
      regularization_multiplier = tune(),
      ...
    )
  } else if ((tune == "custom") | (tune == "none")) {
    base_spec <- maxent(...)
  }
  base_spec
}
