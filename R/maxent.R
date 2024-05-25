#' Maxent model
#'
#' [maxent] defines the MaxEnt model as used in Species
#' Distribution Models.
#' A good guide to how options of a Maxent model work can be found in
#' https://onlinelibrary.wiley.com/doi/full/10.1111/j.1600-0587.2013.07872.x
#'
#' @param mode A single character string for the type of model. The only
#' possible value for this model is "classification".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting. Currently only "maxnet" is available.
#' @param feature_classes character, continuous feature classes desired, either
#'   "default" or any subset of "lqpht" (for example, "lh")
#' @param regularization_multiplier  numeric, a constant to adjust regularization
#' @returns a [`model_spec`] for a `maxent` model
#' @examples
#' \donttest{
#' # format the data
#' data("bradypus", package = "maxnet")
#' bradypus_tb <- tibble::as_tibble(bradypus) %>%
#'   dplyr::mutate(presence = relevel(
#'     factor(
#'       dplyr::case_match(presence, 1 ~ "presence", 0 ~ "absence")
#'     ),
#'     ref = "presence"
#'   )) %>%
#'   select(-ecoreg)
#'
#' # fit the model, and make some predictions
#' maxent_spec <- maxent(feature_classes = "lq")
#' maxent_fitted <- maxent_spec %>%
#'   fit(presence ~ ., data = bradypus_tb)
#' pred_prob <- predict(maxent_fitted, new_data = bradypus[, -1], type = "prob")
#' pred_class <- predict(maxent_fitted, new_data = bradypus[, -1], type = "class")
#'
#' # Now with tuning
#' maxent_spec <- maxent(
#'   regularization_multiplier = tune(),
#'   feature_classes = tune()
#' )
#' set.seed(452)
#' cv <- vfold_cv(bradypus_tb, v = 2)
#' maxent_tune_res <- maxent_spec %>%
#'   tune_grid(presence ~ ., cv, grid = 3)
#' show_best(maxent_tune_res, metric = "roc_auc")
#' }
#' @export
maxent <-
  function(mode = "classification", engine = "maxnet", feature_classes = NULL,
           regularization_multiplier = NULL) {
    # Check for correct mode
    if (mode != "classification") {
      rlang::abort("`mode` should be 'classification'")
    }

    if (engine != "maxnet") {
      rlang::abort("`engine` should be `maxnet`")
    }

    # Capture the arguments in quosures
    args <- list(
      feature_classes = rlang::enquo(feature_classes),
      regularization_multiplier = regularization_multiplier
    )

    # Save some empty slots for future parts of the specification
    parsnip::new_model_spec(
      "maxent",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }


#' @export
update.maxent <- function(object,
                          parameters = NULL,
                          feature_classes = NULL,
                          regularization_multiplier = NULL,
                          fresh = FALSE, ...) {
  args <- list(
    feature_classes = rlang::enquo(feature_classes),
    regularization_multiplier = rlang::enquo(regularization_multiplier)
  )

  parsnip::update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "maxent",
    ...
  )
}

# see make_maxent for the function to register the model with parsnip
