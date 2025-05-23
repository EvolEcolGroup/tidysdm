% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explain_tidysdm.R
\name{explain_tidysdm}
\alias{explain_tidysdm}
\alias{explain_tidysdm.default}
\alias{explain_tidysdm.simple_ensemble}
\alias{explain_tidysdm.repeat_ensemble}
\title{Create explainer from your tidysdm ensembles.}
\usage{
explain_tidysdm(
  model,
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
  by_workflow
)

\method{explain_tidysdm}{default}(
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
  by_workflow = FALSE
)

\method{explain_tidysdm}{simple_ensemble}(
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
  by_workflow = FALSE
)

\method{explain_tidysdm}{repeat_ensemble}(
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
  by_workflow = FALSE
)
}
\arguments{
\item{model}{object - a model to be explained}

\item{data}{data.frame or matrix - data which will be used to calculate the explanations. If not provided, then it will be extracted from the model. Data should be passed without a target column (this shall be provided as the \code{y} argument). NOTE: If the target variable is present in the \code{data}, some of the functionalities may not work properly.}

\item{y}{numeric vector with outputs/scores. If provided, then it shall have the same size as \code{data}}

\item{predict_function}{function that takes two arguments: model and new data and returns a numeric vector with predictions.   By default it is \code{yhat}.}

\item{predict_function_target_column}{Character or numeric containing either column name or column number in the model prediction object of the class that should be considered as positive (i.e. the class that is associated with probability 1). If NULL, the second column of the output will be taken for binary classification. For a multiclass classification setting, that parameter cause switch to binary classification mode with one vs others probabilities.}

\item{residual_function}{function that takes four arguments: model, data, target vector y and predict function (optionally). It should return a numeric vector with model residuals for given data. If not provided, response residuals (\eqn{y-\hat{y}}) are calculated. By default it is \code{residual_function_default}.}

\item{...}{other parameters}

\item{label}{character - the name of the model. By default it's extracted from the 'class' attribute of the model}

\item{verbose}{logical. If TRUE (default) then diagnostic messages will be printed}

\item{precalculate}{logical. If TRUE (default) then \code{predicted_values} and \code{residual} are calculated when explainer is created.
This will happen also if \code{verbose} is TRUE. Set both \code{verbose} and \code{precalculate} to FALSE to omit calculations.}

\item{colorize}{logical. If TRUE (default) then \code{WARNINGS}, \code{ERRORS} and \code{NOTES} are colorized. Will work only in the R console. Now by default it is \code{FALSE} while knitting and \code{TRUE} otherwise.}

\item{model_info}{a named list (\code{package}, \code{version}, \code{type}) containing information about model. If \code{NULL}, \code{DALEX} will seek for information on it's own.}

\item{type}{type of a model, either \code{classification} or \code{regression}. If not specified then \code{type} will be extracted from \code{model_info}.}

\item{by_workflow}{boolean determining whether a list of explainer, one per
model, should be returned instead of a single explainer for the ensemble}
}
\value{
explainer object \code{\link[DALEX:explain]{DALEX::explain}} ready to work with DALEX
}
\description{
DALEX is designed to explore and explain the behaviour of Machine Learning
methods. This function creates a DALEX explainer (see \code{\link[DALEX:explain]{DALEX::explain()}}),
which can then be queried by multiple functions from the DALEX package to
create explanations of the model.
}
\details{
By default, the response variable is extracted form the ensemble object. Note
that, if the response variable is passed directly, \code{y} should be a factor
with presence as a reference level. To check that \code{y} is formatted correctly,
use \code{\link[=check_sdm_presence]{check_sdm_presence()}}.
}
\examples{
\dontshow{data.table::setDTthreads(2)}
\donttest{
# using the whole ensemble
lacerta_explainer <- explain_tidysdm(tidysdm::lacerta_ensemble)
# by workflow
explainer_list <- explain_tidysdm(tidysdm::lacerta_ensemble,
  by_workflow = TRUE
)
}
}
