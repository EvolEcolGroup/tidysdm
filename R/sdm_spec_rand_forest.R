#' Model specification for a Random Forest for SDM
#'
#' This function returns a [parsnip::model_spec] for a Random Forest to
#' be used as a classifier of presences and absences in Species Distribution
#' Models.
#'
#' `sdm_spec_rf()` is simply a short form for `sm_spec_rand_forest()`.
#'
#' @param ... parameters to be passed to [parsnip::rand_forest()] to
#' customise the model. See the help of that function for details.
#' @param tune character defining the tuning strategy. Valid strategies are:
#' {itemize:
#' /item: "sdm" chooses hyperparameters that are most important to tune for
#' an sdm (for *rf*, 'mtry')
#' /item: "all" tunes all hyperparameters (for *rf*, 'mtry', 'trees' and 'min')
#' /item: "custom" passes the options from '...'
#' /item: "none" does not tune any hyperparameter
#' }
#' @return a [parsnip::model_spec] of the model.
#' @examples
#' test_rf_spec <- sdm_spec_rf(tune="sdm")
#' test_rf_spec
#' # combining tuning with specific values for other hyperparameters
#' sdm_spec_rf(tune="sdm", trees=100)
#' @export

sdm_spec_rand_forest <- function(..., tune=c("sdm","all","custom","none")){
 tune <- rlang::arg_match(tune)
  if (tune=="sdm"){
    base_spec <- parsnip::rand_forest(mtry = tune(),
                                     ...)
  } else if (tune=="all"){
    base_spec <- parsnip::rand_forest(mtry = tune(),
                                     min_n = tune(),
                                     trees = tune(),
                                     ...)
  } else if ((tune=="custom") | (tune=="none")){
    base_spec <- parsnip::rand_forest(...)
  }
  base_spec %>%
    parsnip::set_engine("ranger") %>%
    parsnip::set_mode("classification")
}

#' @rdname sdm_spec_rand_forest
#' @export

sdm_spec_rf <- function(..., tune=c("sdm","all","custom","none")){
  sdm_spec_rand_forest(..., tune = tune)
}
