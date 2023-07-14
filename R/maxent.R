#' Maxent model
#'
#' [maxent] defines a MaxEnt model for binary outcomes as used in Species
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
#' # format the data
#' data("bradypus", package="maxnet")
#' bradypus_tb <- tibble::as_tibble(bradypus) %>% dplyr::mutate(presence = relevel(factor(
#'   dplyr::case_match (presence, 1~"presence",0 ~"absence")),
#'   ref="presence")) %>% select(-ecoreg)
#'
#' # fit the model, and make some predictions
#' maxent_spec <- maxent(feature_classes = "lq")
#' maxent_fitted <- maxent_spec %>%
#'   fit(presence ~ ., data = bradypus_tb)
#' pred_prob <-predict(maxent_fitted,new_data = bradypus[,-1], type="prob")
#' pred_class <- predict(maxent_fitted,new_data = bradypus[,-1], type="class")
#'
#' # Now with tuning
#' maxent_spec <- maxent(regularization_multiplier = tune(),
#'                       feature_classes = tune())
#' set.seed(452)
#' cv <- vfold_cv(bradypus_tb, v=2)
#' maxent_tune_res <- maxent_spec %>%
#'   tune_grid(presence ~ ., cv, grid = 3)
#' show_best(maxent_tune_res, metric = "roc_auc")
#'
#' @export
maxent <-
  function(mode = "classification",  engine = "maxnet", feature_classes = NULL,
           regularization_multiplier = NULL) {
    # Check for correct mode
    if (mode  != "classification") {
      rlang::abort("`mode` should be 'classification'")
    }

    if (engine != "maxnet") {
      rlang::abort("`engine` should be `maxnet`")
    }

    # Capture the arguments in quosures
    args <- list(feature_classes = rlang::enquo(feature_classes),
                 regularization_multiplier = regularization_multiplier)

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

# function to register the parnsip model for maxent
#' @keywords internal
make_maxent <- function() {
  parsnip::set_new_model("maxent")
  parsnip::set_model_mode(model = "maxent", mode = "classification")
  parsnip::set_model_engine(
    "maxent",
    mode = "classification",
    eng = "maxnet"
  )
  parsnip::set_dependency("maxent", eng = "maxnet", pkg = "maxnet")
  parsnip::set_dependency("maxent", eng = "maxnet", pkg = "tidysdm")

  parsnip::set_model_arg(
    model = "maxent",
    eng = "maxnet",
    parsnip = "feature_classes",
    original = "classes",
    # TODO this should be sorted out for tuning
    #func = list(pkg = "foo", fun = "bar"),
    func = list(pkg = "tidysdm", fun = "feature_classes"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "maxent",
    eng = "maxnet",
    parsnip = "regularization_multiplier",
    original = "regmult",
    # TODO this should be sorted out for tuning
    #func = list(pkg = "foo", fun = "bar"),
    func = list(pkg = "tidysdm", fun = "regularization_multiplier"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "maxent",
    eng = "maxnet",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "tidysdm", fun = "maxnet_fit"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "maxent",
    eng = "maxnet",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  class_info <-
    list(
      pre = NULL,
      post = function(x,object){
        class_vect<-rep(object$lvl[2],length(x))
        class_vect[x==1]<- object$lvl[1]
        class_vect <- stats::relevel(factor(class_vect),ref=object$lvl[1])
        tibble::tibble(class = class_vect)
      },
      func = c(pkg="tidysdm", fun = "maxnet_predict"),
      args =
        # These lists should be of the form:
        # {predict.maxnet argument name} = {values provided from parsnip objects}
        list(
          # We don't want the first two arguments evaluated right now
          # since they don't exist yet. `type` is a simple object that
          # doesn't need to have its evaluation deferred.
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "class"
        )
    )

  parsnip::set_pred(
    model = "maxent",
    eng = "maxnet",
    mode = "classification",
    type = "class",
    value = class_info
  )

  prob_info <-
    parsnip::pred_value_template(
      post = function(x, object) {
        tibble::tibble("{object$lvl[1]}" := as.vector(x),
                       "{object$lvl[2]}" := 1-as.vector(x))
      },
      func = c(pkg="tidysdm", fun = "maxnet_predict"),
      # Now everything else is put into the `args` slot
      object = quote(object$fit),
      newdata = quote(new_data),
      type = "prob"
      #TODO do we add here maxent_type and clamp???
    )

  parsnip::set_pred(
    model = "maxent",
    eng = "maxnet",
    mode = "classification",
    type = "prob",
    value = prob_info
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
