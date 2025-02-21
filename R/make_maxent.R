# function to register the parnsip model for maxent
# no coverage is computed for these function, as it is registered before the
# package is loaded
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
    func = list(pkg = "tidysdm", fun = "feature_classes"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "maxent",
    eng = "maxnet",
    parsnip = "regularization_multiplier",
    original = "regmult",
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
      post = function(x, object) {
        class_vect <- rep(object$lvl[2], length(x))
        class_vect[x == 1] <- object$lvl[1]
        class_vect <- stats::relevel(factor(class_vect), ref = object$lvl[1])
        tibble::tibble(class = class_vect)
      },
      func = c(pkg = "tidysdm", fun = "maxnet_predict"),
      args = list(
        # We don't want the first two arguments evaluated right now
        # since they don't exist yet. `type` is a simple object that
        # doesn't need to have its evaluation deferred.
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "class"
      )
      # These lists should be of the form:
      # {predict.maxnet argument name} = {values provided from parsnip objects}

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
        tibble::tibble(
          "{object$lvl[1]}" := as.vector(x),
          "{object$lvl[2]}" := 1 - as.vector(x)
        )
      },
      func = c(pkg = "tidysdm", fun = "maxnet_predict"),
      # Now everything else is put into the `args` slot
      object = quote(object$fit),
      newdata = quote(new_data),
      type = "prob"
      # TODO do we add here maxent_type and clamp???
    )

  parsnip::set_pred(
    model = "maxent",
    eng = "maxnet",
    mode = "classification",
    type = "prob",
    value = prob_info
  )
}
