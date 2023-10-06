#' Create explainer from your tidysdm ensembles.
#'
#' DALEX is designed to explore and explain the behaviour of Machine Learning
#' methods. This function creates a DALEX explainer (see [DALEX::explain()]), which can then be queried
#' by multiple function to create explanations of the model.
#' @inheritParams DALEX::explain
#' @param by_workflow boolean determing whether a list of explainer, one per model,
#' should be returned instead of a single explainer for the ensemble
#' @return explainer object [`DALEX::explain`] ready to work with DALEX
#' @export
#' @examples
#' # using the whole ensemble
#' lacerta_explainer <- explain_tidysdm(tidysdm::lacerta_ensemble)
#' # by workflow
#' explainer_list <- explain_tidysdm(tidysdm::lacerta_ensemble,
#'   by_workflow = TRUE
#' )
#'
explain_tidysdm <- function(model,
                            data,
                            y,
                            predict_function,
                            predict_function_target_column,
                            residual_function,
                            ...,
                            label,
                            verbose,
                            precalculate,
                            colorize,
                            model_info,
                            type,
                            by_workflow) {
  UseMethod("explain_tidysdm", object = model)
}

#' @rdname explain_tidysdm
#' @export
explain_tidysdm.default <- function(
    model,
    data = NULL,
    y = NULL,
    predict_function = NULL,
    predict_function_target_column = NULL,
    residual_function = NULL,
    ...,
    label = NULL,
    verbose = TRUE,
    precalculate = TRUE,
    colorize = !isTRUE(getOption("knitr.in.progress")),
    model_info = NULL,
    type = "classification",
    by_workflow = FALSE) {
  stop("no method defined for this object type")
}

#' @rdname explain_tidysdm
#' @export
explain_tidysdm.simple_ensemble <- function(
    model,
    data = NULL,
    y = NULL,
    predict_function = NULL,
    predict_function_target_column = NULL,
    residual_function = NULL,
    ...,
    label = NULL,
    verbose = TRUE,
    precalculate = TRUE,
    colorize = !isTRUE(getOption("knitr.in.progress")),
    model_info = NULL,
    type = "classification",
    by_workflow = FALSE) {
  if (by_workflow) {
    explain_simple_ensemble_by_workflow(
      model = model,
      data = data,
      y = y,
      predict_function = predict_function,
      predict_function_target_column = predict_function_target_column,
      residual_function = residual_function,
      weights = NULL,
      label = label,
      verbose = verbose,
      precalculate = precalculate,
      colorize = colorize,
      model_info = model_info,
      type = type
    )
  } else {
    explain_simple_ensemble(
      model = model,
      data = data,
      y = y,
      predict_function = predict_function,
      predict_function_target_column = predict_function_target_column,
      residual_function = residual_function,
      weights = NULL,
      label = label,
      verbose = verbose,
      precalculate = precalculate,
      colorize = colorize,
      model_info = model_info,
      type = type
    )
  }
}

#' @rdname explain_tidysdm
#' @export
explain_tidysdm.repeat_ensemble <- function(
    model,
    data = NULL,
    y = NULL,
    predict_function = NULL,
    predict_function_target_column = NULL,
    residual_function = NULL,
    ...,
    label = NULL,
    verbose = TRUE,
    precalculate = TRUE,
    colorize = !isTRUE(getOption("knitr.in.progress")),
    model_info = NULL,
    type = "classification",
    by_workflow = FALSE) {
  # we change the names of the workflows to combine with the repeat ids
  model$workflow_id <- paste(model$rep_id, model$wflow_id, sep = ".")
  class(model)[1] <- "simple_ensemble"
  explain_tidysdm(
    model = model,
    data = data,
    y = y,
    predict_function = predict_function,
    predict_function_target_column = predict_function_target_column,
    residual_function = residual_function,
    ... = ...,
    label = label,
    verbose = verbose,
    precalculate = precalculate,
    colorize = colorize,
    model_info = model_info,
    type = type,
    by_workflow = by_workflow
  )
}

explain_simple_ensemble <- function(
    model,
    data = NULL,
    y = NULL,
    predict_function = NULL,
    predict_function_target_column = NULL,
    residual_function = NULL,
    ...,
    label = NULL,
    verbose = TRUE,
    precalculate = TRUE,
    colorize = !isTRUE(getOption("knitr.in.progress")),
    model_info = NULL,
    type = "classification") {
  if (type != "classification") {
    stop("type has to be classification for a tidysdm ensemble")
  }
  if (is.null(data)) {
    data <- workflowsets::extract_mold(model$workflow[[1]])$predictors
  }
  if (is.null(y)) {
    # note that we need presences to be 1 and absences to be zero
    y <- (as.numeric(workflowsets::extract_mold(model$workflow[[1]])$outcomes %>% dplyr::pull()) - 2) * -1
  } else {
    if (!is.factor(y)) {
      stop("y should be a factor with presences as reference levels")
    } else {
      y <- (as.numeric(y) - 2) * -1
    }
  }
  if (is.null(predict_function)) {
    predict_function <- function(model, newdata) {
      stats::predict(model, newdata)$mean
    }
  }

  DALEX::explain(
    model = model,
    data = data,
    y = y,
    predict_function = predict_function,
    predict_function_target_column = predict_function_target_column,
    residual_function = residual_function,
    weights = NULL,
    label = label,
    verbose = verbose,
    precalculate = precalculate,
    colorize = colorize,
    model_info = NULL,
    type = type
  )
}

# method for model info to work on simple ensemble
#' @importFrom DALEX model_info
#' @export
#' @method model_info simple_ensemble
model_info.simple_ensemble <- function(model, is_multiclass = FALSE, ...) {
  if (is_multiclass) {
    stop("tidysdm simple_ensembles can not be multiclass")
  }
  package <- "tidysdm"
  type <- "classification"
  ver <- try(as.character(utils::packageVersion(package)),
    silent = TRUE
  )
  if (inherits(ver, "try-error")) {
    ver <- "Unknown"
  }
  model_info <- list(package = package, ver = ver, type = type)
  class(model_info) <- "model_info"
  model_info
}

# method for model info to work on simple ensemble
#' @importFrom DALEX model_info
#' @export
#' @method model_info repeat_ensemble
model_info.repeat_ensemble <- function(model, is_multiclass = FALSE, ...) {
  if (is_multiclass) {
    stop("tidysdm repeat_ensembles can not be multiclass")
  }
  package <- "tidysdm"
  type <- "classification"
  ver <- try(as.character(utils::packageVersion(package)),
    silent = TRUE
  )
  if (inherits(ver, "try-error")) {
    ver <- "Unknown"
  }
  model_info <- list(package = package, ver = ver, type = type)
  class(model_info) <- "model_info"
  model_info
}


explain_simple_ensemble_by_workflow <- function(
    model,
    data = NULL,
    y = NULL,
    predict_function = NULL,
    predict_function_target_column = NULL,
    residual_function = NULL,
    ...,
    label = NULL,
    verbose = TRUE,
    precalculate = TRUE,
    colorize = !isTRUE(getOption("knitr.in.progress")),
    model_info = NULL,
    type = "classification") {
  if (type != "classification") {
    stop("type has to be classification for a tidysdm ensemble")
  }
  explainer_list <- list()
  for (i in 1:nrow(model)) {
    if (is.null(data)) {
      data_train <- workflowsets::extract_mold(model$workflow[[i]])$predictors
    } else {
      data_train <- data
    }
    if (is.null(y)) {
      data_response <- as.numeric(workflowsets::extract_mold(model$workflow[[i]])$outcomes %>% dplyr::pull()) - 1
    } else {
      data_response <- y
    }

    explainer_list[[i]] <-
      DALEXtra::explain_tidymodels(
        model$workflow[[i]],
        data = data_train,
        y = data_response,
        predict_function = predict_function,
        predict_function_target_column = predict_function_target_column,
        residual_function = residual_function,
        label = model$wflow_id[[i]],
        verbose = verbose,
        precalculate = precalculate,
        colorize = colorize,
        model_info = model_info,
        type = "classification"
      )
  }
  return(explainer_list)
}
