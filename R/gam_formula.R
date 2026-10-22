#' Create a formula for gam
#'
#' This function takes the formula from a recipe, and turns numeric predictors
#' into smooths with a given k (other predictors, such as factors, are included
#' as linear terms). This formula can be passed to a workflow or
#' workflow set when fitting a gam.
#' @param object a [recipes::recipe], already trained
#' @param k the *k* value for the smooth
#' @returns a formula
#' @export
gam_formula <- function(object, k = 10) {
  base_formula <- object %>%
    recipes::prep() %>%
    stats::formula()
  predictors <- rsample::form_pred(base_formula)
  # Now check which predictors are numeric, and thus should be used in smooths
  predictors_type <- object$var_info$type[match(
    predictors,
    object$var_info$variable
  )]
  predictors_numeric <- predictors[!is.na(
    unlist(
      lapply(predictors_type, function(table) match("numeric", table))
    )
  )]
  if (length(predictors_numeric) > 0) {
    rhs <- paste0(
      "s(", predictors_numeric, ", k = ", k, ")",
      collapse = " + "
    )
  } else {
    stop(
      "there are no numeric predictors; a gam does not really ",
      "make much sense..."
    )
  }
  # other predictors are added as linear terms
  predictors_factor <- predictors[!predictors %in% predictors_numeric]
  if (length(predictors_factor)) {
    rhs <- paste(c(rhs, predictors_factor), collapse = " + ")
  }
  stats::formula(paste(form_resp(base_formula), "~", rhs))
}

#' Get the response variable from a formula
#'
#' This is the counterpart of [rsample::form_pred].
#'
#' Note: this might not behave well with functions such as log(y). But neither
#' does [rsample::form_pred].
#'
#' modified from
# nolint start
#' https://stackoverflow.com/questions/13217322/how-to-reliably-get-dependent-variable-name-from-formula-object
# nolint end
#' @param x a formula
#' @returns character the name of the response
#' @keywords internal
form_resp <- function(x) {
  if (attr(stats::terms(x), which = "response")) {
    all.vars(x)[1]
  } else {
    NULL
  }
}
