#' Model specification for a GAM for SDM
#'
#' This function returns a [parsnip::model_spec] for a General Additive Model to
#' be used as a classifier of presences and absences in Species Distribution Model.
#'
#' @param ... parameters to be passed to [parsnip::gen_additive_mod()] to
#' customise the model. See the help of that function for details.
#' @param tune character defining the tuning strategy. As there are no hyperparameters
#' to tune in a *gam*, the only valid option is "none". This parameter is present
#' for consistency with other `sdm_spec_*` functions, but it does nothing in this
#' case.
#' @returns a [parsnip::model_spec] of the model.
#' @examples
#' my_gam_spec <- sdm_spec_gam()
#' @export

sdm_spec_gam <- function(..., tune="none"){
  tune <- rlang::arg_match(tune)
  parsnip::gen_additive_mod(...) %>%  # model type
    parsnip::set_engine(engine = "mgcv") %>%  # model engine
    parsnip::set_mode("classification") # model mode
}
