#' Model specification for a Random Forest for SDM
#'
#' This function is wrapper to [parsnip::predict.model_fit] to deal with the
#' case when `new_data` is an [sf::sf] object, as it is the case for data
#' used for Species Distribution Model.
#'
#' @param object An object of class [parsnip::model_fit].
#' @param new_data A rectangular data object, such as a data frame.
#' @param ... parameters to be passed to [parsnip::predict.model_fit]
#' customise the model. See the help of that function for details.
#' @return an appropriate object depending on the type argument, see
#' [parsnip::predict.model_fit] for details.
#' @export

predict.model_fit <- function(object, new_data, ...) {
  if (inherits(new_data, "sf")){
    new_data<-new_data %>% dplyr::as_tibble() %>% dplyr::select(-geometry)
  }
  predict(object, new_data=new_data, ...)
}
