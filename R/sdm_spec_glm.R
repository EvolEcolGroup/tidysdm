#' Model specification for a GLM for SDM
#'
#' This function returns a [parsnip::model_spec] for a Generalised Linear Model to
#' be used as a classifier of presences and absences in Species Distribution Model.
#'
#' @param ... parameters to be passed to [parsnip::logistic_reg()] to customise
#' the model. See the help of that function for details.
#' @param tune character defining the tuning strategy. As there are no hyperparameters
#' to tune in a *glm*, the only valid option is "none". This parameter is present
#' for consistency with other `sdm_spec_*` functions, but it does nothing in this
#' case.
#' @return a [parsnip::model_spec] of the model.
#' @examples
#' my_spec_glm <- sdm_spec_glm()
#' @export

sdm_spec_glm <- function(..., tune="none"){
  tune <- rlang::arg_match(tune)
  parsnip::logistic_reg(...) %>%  # model type
    parsnip::set_engine(engine = "glm") %>%  # model engine
    parsnip::set_mode("classification") # model mode
}

