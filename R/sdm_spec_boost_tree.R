#' Model specification for a Boosted Trees model for SDM
#'
#' This function returns a [parsnip::model_spec] for a Boosted Trees model to be
#' used as a classifier of presences and absences in Species Distribution Model.
#' It uses the library `xgboost` to fit boosted trees; to use another library,
#' simply build the [parsnip::model_spec] directly.
#'
#' @param ... parameters to be passed to [parsnip::boost_tree()] to customise
#'   the model. See the help of that function for details.
#' @param tune character defining the tuning strategy. Valid strategies are:
#' * "sdm" chooses hyperparameters that are most important to tune for
#'   an sdm (for *boost_tree*: 'mtry', 'trees', 'tree_depth', 'learn_rate',
#'   'loss_reduction', and 'stop_iter')
#' * "all" tunes all hyperparameters (for *boost_tree*: 'mtry', 'trees',
#'   'tree_depth', 'learn_rate', 'loss_reduction', 'stop_iter','min_n' and
#'   'sample_size')
#' * "custom" passes the options from '...'
#' * "none" does not tune any hyperparameter
#' @returns a [parsnip::model_spec] of the model.
#' @examples
#' standard_bt_spec <- sdm_spec_boost_tree()
#' full_bt_spec <- sdm_spec_boost_tree(tune = "all")
#' custom_bt_spec <- sdm_spec_boost_tree(tune = "custom", mtry = tune::tune())
#' @export
#' @family "sdm model specifications"

sdm_spec_boost_tree <- function(..., tune = c("sdm", "all", "custom", "none")) {
  tune <- rlang::arg_match(tune)
  if (tune == "sdm") {
    base_spec <- parsnip::boost_tree(
      mtry = tune::tune(), #
      trees = tune::tune(), #
      tree_depth = tune::tune(), #
      learn_rate = tune::tune(), #
      loss_reduction = tune::tune(), #
      stop_iter = tune::tune(), #
      ...
    )
  } else if (tune == "all") {
    base_spec <- parsnip::boost_tree(
      mtry = tune::tune(),
      trees = tune::tune(),
      min_n = tune::tune(),
      tree_depth = tune::tune(),
      learn_rate = tune::tune(),
      loss_reduction = tune::tune(),
      sample_size = tune::tune(),
      stop_iter = tune::tune(),
      ...
    )
  } else if ((tune == "custom") | (tune == "none")) {
    base_spec <- parsnip::boost_tree(...)
  }
  base_spec %>%
    parsnip::set_engine("xgboost") %>%
    parsnip::set_mode("classification")
}
