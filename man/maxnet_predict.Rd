% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maxnet_fit.R
\name{maxnet_predict}
\alias{maxnet_predict}
\title{Wrapper to predict maxnet models}
\usage{
maxnet_predict(
  object,
  newdata,
  type = c("class", "prob"),
  maxnet_type = c("cloglog", "link", "exponential", "logistic"),
  clamp = TRUE
)
}
\arguments{
\item{object}{the \link[maxnet:maxnet]{maxnet::maxnet} object}

\item{newdata}{the dataframe of new data}

\item{type}{either "prob" or "class"}

\item{maxnet_type}{the transformation used for the prediction}

\item{clamp}{logical, defining whether clamping to observed ranges should
be used}
}
\value{
a tibble of predictions
}
\description{
This function is a wrapper around the \code{predict} method for \link[maxnet:maxnet]{maxnet::maxnet},
making the function compatible with
\code{parsnip}. Users are unlikely to use this function directly.  For the
\code{parsnip} model specification for MaxEnt, see \code{\link[=maxent]{maxent()}}.
}
\keyword{internal}
