#' Model specification for a Boosted Trees model for SDM
#'
#' This function returns a [parsnip::model_spec] for a Boosted Trees model to
#' be used as a classifier of presences and absences in Species Distribution Model.
#'
#' @param ... parameters to be passed to [parsnip::boost_tree()] to
#' customise the model. See the help of that function for details.
#' @param tune character defining the tuning strategy. Valid strategies are:
#' {itemize:
#' /item: "sdm" chooses hyperparameters that are most important to tune for
#' an sdm (for *boost_tree*: 'mtry', 'trees', 'tree_depth', 'learn_rate',
#' 'loss_reduction', and 'stop_iter')
#' /item: "all" tunes all hyperparameters (for *boost_tree*: 'mtry', 'trees',
#' 'tree_depth', 'learn_rate',
#' 'loss_reduction', 'stop_iter','min_n' and 'sample_size')
#' /item: "custom" passes the options from '...'
#' /item: "none" does not tune any hyperparameter
#' }
#' @returns a [parsnip::model_spec] of the model.
#' @examples
#' standard_bt_spec <- sdm_spec_boost_tree()
#' full_bt_spec <- sdm_spec_boost_tree(tune = "all")
#' custom_bt_spec <- sdm_spec_boost_tree(tune = "custom", mtry = tune())
#' @export

sdm_spec_boost_tree <- function(..., tune=c("sdm","all","custom","none")){
  tune <- rlang::arg_match(tune)
  if (tune=="sdm"){
    base_spec <- parsnip::boost_tree(mtry = tune(), #
                                     trees = tune(), #
                                     tree_depth = tune(), #
                                     learn_rate = tune(), #
                                     loss_reduction = tune(), #
                                     stop_iter = tune(), #
                                      ...)
  } else if (tune=="all"){
    base_spec <- parsnip::boost_tree(mtry = tune(),
                                     trees = tune(),
                                     min_n = tune(),
                                     tree_depth = tune(),
                                     learn_rate = tune(),
                                     loss_reduction = tune(),
                                     sample_size = tune(),
                                     stop_iter = tune(),
                                      ...)
  } else if ((tune=="custom") | (tune=="none")){
    base_spec <- parsnip::boost_tree(...)
  }
  base_spec %>%    parsnip::set_engine("xgboost") %>%
    parsnip::set_mode("classification")
}
