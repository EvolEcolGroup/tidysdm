#' Control wrappers
#'
#' @description
#' Supply these light wrappers as the `control` argument in a
#' [tune::tune_grid()], [tune::tune_bayes()], or [tune::fit_resamples()]
#' call to return the needed elements for use in an ensemble.
#' These functions will return the appropriate control grid to ensure that
#' assessment set predictions and information on model specifications and
#' preprocessors, is supplied in the resampling results object!
#'
#' To integrate ensemble settings with your existing control settings, note
#' that these functions just call the appropriate `tune::control_*` function
#' with the arguments `save_pred = TRUE, save_workflow = TRUE`.
#'
#' These wrappers are equivalent to the ones used in the `stacks` package.
#'
#' @returns A [tune::control_grid], [tune::control_bayes],
#' or [tune::control_resamples] object.
#'
#' @seealso See the vignettes for examples of these functions used in context.
#'
#' @rdname control_ensemble
#' @export
control_ensemble_grid <- function() {
  tune::control_grid(
    save_pred = TRUE,
    save_workflow = TRUE
  )
}

#' @rdname control_ensemble
#' @export
control_ensemble_resamples <- function() {
  tune::control_resamples(
    save_pred = TRUE,
    save_workflow = TRUE
  )
}

#' @rdname control_ensemble
#' @export
control_ensemble_bayes <- function() {
  tune::control_bayes(
    save_pred = TRUE,
    save_workflow = TRUE
  )
}
