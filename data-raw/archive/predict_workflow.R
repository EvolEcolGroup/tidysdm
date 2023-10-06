#' Predict from a workflow
#'
#' This function is wrapper to [workflows::predict.workflow] to deal with the
#' case when `new_data` is an [sf::sf] object, as it is the case for data
#' used for Species Distribution Model.
#'
#' @param object A workflow that has been fit by [workflows::fit.workflow()]
#' @param new_data An [sf::sf] data frame.
#' @param ... parameters to be passed to [workflows::predict.workflow]
#' to define the type of predictions. See the help of that function for details.
#' @returns an appropriate object depending on the type argument, see
#' [workflows::predict.workflow] for details.
#' @export
#' @import workflows

predict.workflow <- function(object, new_data, ...) {
  if (inherits(new_data, "sf")) {
    new_data <- new_data %>%
      dplyr::as_tibble() %>%
      dplyr::select(-geometry)
  }
  browser()
  workflows:::predict.workflow(object, new_data = new_data, ...)
}
