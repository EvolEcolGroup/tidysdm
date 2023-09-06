#' Create explainer from your tidysdm ensembles.
#'
#' DALEX is designed to explore and explain the behaviour of Machine Learning
#' methods. This function creates a DALEX explainer (see [DALEX::explain()]), which can then be queried
#' by multiple function to create explanations of the model.
#' @inheritParams DALEX::explain
#' @return explainer object [`DALEX::explain`] ready to work with DALEX
#' @export
#' @examples
#' lacerta_explainer <- explain_tidysdm(lacerta_ensemble)
#' 

explain_tidysdm <- function(
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
    type = "classification"
    #,
    #by_workflow = FALSE
) {
  if (!inherits(model,"simple_ensemble")){
    stop("this function currently only works with simple_ensembles from tidysdm")
  }
  if (is.null(data)){
    data = extract_mold(lacerta_ensemble$workflow[[1]])$predictors
  }
  if (is.null(y)){
    # note that we need presences to be 1 and absences to be zero
    y <- (as.numeric(extract_mold(lacerta_ensemble$workflow[[1]])$outcomes %>% pull())-2)*-1
  } else {
    if (!is.factor(y)){
      stop("y should be a factor with presences as reference levels")
    } else {
      y <- (as.numeric(y)-2)*-1
    }
  }
  if (type!="classification"){
    stop("type has to be classification for a tidysdm ensemble")
  }
  if (is.null(predict_function)){
    predict_function <- function(model, newdata) {
      predict(model, newdata)$mean
    }
  }
  
#  model_info <- list(package = "tidysdm",
#                     ver = utils::packageVersion("tidysdm"),
#                     type = "classification")
  
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
model_info.simple_ensemble<- function(model, is_multiclass=FALSE,...){
  if (is_multiclass){
    stop("tidysdm simple_ensembles can not be multiclass")
  }
  package <- "tidysdm"
  type <- "classification"
  ver <- try(as.character(utils::packageVersion(package)), 
             silent = TRUE)
  if (inherits(ver, "try-error")) {
    ver <- "Unknown"
  }
  model_info <- list(package = package, ver = ver, type = type)
  class(model_info) <- "model_info"
  model_info
}
