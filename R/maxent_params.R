#' Parameters for maxent models
#'
#' These parameters are auxiliary to MaxEnt models using the "maxnet" engine.
#' These functions are used by the tuning functions, and the user will rarely
#' access them directly.
#'
#' @param range A two-element vector holding the defaults for the smallest and
#' largest possible values, respectively. If a transformation is specified,
#' these values should be in the transformed units.
#' @param trans A trans object from the scales package, such as scales::log10_trans()
#' or scales::reciprocal_trans(). If not provided, the default is used which
#' matches the units used in range. If no transformation, NULL.
#' @param values For `feature_classes()`, a character string of
#' any subset of "lqpht" (for example, "lh")
#' @returns a `param` object that can be used for tuning.
#' @examples
#' regularization_multiplier()
#' feature_classes()
#' @name maxent_params
NULL

#' @rdname maxent_params
#' @export
regularization_multiplier <- function(range = c(0.5, 3), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(regularization_multiplier = "Reg. multiplier"),
    finalize = NULL
  )
}

#' @rdname maxent_params
#' @export
feature_classes <- function(values = c("l", "lq", "lqp", "lqph", "lqpht")) {
  dials::new_qual_param(
    type = c("character"),
    values = values,
    label = c(feature_classes = "Feature classes"),
    finalize = NULL
  )
}

#' @export
tunable.maxent <- function(x, ...) {
  tibble::tibble(
    name = c("regularization_multiplier", "feature_classes"),
    call_info = list(
      list(pkg = NULL, fun = "regularization_multiplier"),
      list(pkg = NULL, fun = "feature_classes")
    ),
    source = "model_spec",
    component = "maxent",
    component_id = "main"
  )
}
