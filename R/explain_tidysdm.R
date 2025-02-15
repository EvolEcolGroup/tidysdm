#' Create explainer from your tidysdm ensembles.
#'
#' DALEX is designed to explore and explain the behaviour of Machine Learning
#' methods. This function creates a DALEX explainer (see [DALEX::explain()]), which can then be queried
#' by multiple functions from the DALEX package to create explanations of the model.
#'
#' By default, the response variable is extracted form the ensemble object. Note that, if 
#' the response variable is passed directly,
#' `y` should be a factor with presence as a reference level. To check that `y` is formatted correctly,
#' use [check_sdm_presence()].
#' @inheritParams DALEX::explain
#' @param by_workflow boolean determining whether a list of explainer, one per model,
#' should be returned instead of a single explainer for the ensemble
#' @return explainer object [`DALEX::explain`] ready to work with DALEX
#' @export
#' @examples
#' \donttest{
#' # using the whole ensemble
#' lacerta_explainer <- explain_tidysdm(tidysdm::lacerta_ensemble)
#' # by workflow
#' explainer_list <- explain_tidysdm(tidysdm::lacerta_ensemble,
#'   by_workflow = TRUE
#' )
#' }
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
    if (is.null(model$workflow[[1]]$pre$actions$recipe$recipe$steps)){
    data <- workflowsets::extract_mold(model$workflow[[1]])$predictors
    } else {
      stop("your recipe contains steps; please provide a copy of the original dataset as 'data' argument")
    }
  }
  if (is.null(y)) {
    # note that, for DALEX, we need presences to be 1 and absences to be zero
    # that's the opposite of what we usually have in tidymodels, where presence is the reference
    y <- (as.numeric(workflowsets::extract_mold(model$workflow[[1]])$outcomes %>% dplyr::pull()) - 2) * -1
  } else {
    # ideally we would use check_sdm_presence to make sure that the
    # response variable is properly formatted (and not just a factor)
    # the error message suggests as much. However, this would require passing
    # info on column and presence level, which leads to a proliferation of parameters
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

  if (!requireNamespace("DALEXtra", quietly = TRUE)) {
    stop(
      "to use this function, first install package 'DALEXtra' with\n",
      "install.packages('DALEXtra')")
  }

  explainer_list <- list()
  for (i in seq_len(nrow(model))) {
    if (is.null(data)) {
      if (is.null(model$pre$actions$recipe$recipe$steps)){
      data_train <- workflowsets::extract_mold(model$workflow[[i]])$predictors
      } else {
        stop("your recipe contains steps; please provide a copy of the original dataset as data argument")
      }
    } else {
      data_train <- data
    }
    if (is.null(y)) {
      data_response <- (as.numeric(workflowsets::extract_mold(model$workflow[[i]])$outcomes %>% dplyr::pull()) - 2) * -1
     } else {
      data_response <- (as.numeric(y) - 2) * -1
    }

    # browser()
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
