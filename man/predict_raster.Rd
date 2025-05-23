% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_raster.R
\name{predict_raster}
\alias{predict_raster}
\alias{predict_raster.default}
\title{Make predictions for a whole raster}
\usage{
predict_raster(object, raster, ...)

\method{predict_raster}{default}(object, raster, ...)
}
\arguments{
\item{object}{the \code{tidymodels} object of interest}

\item{raster}{the \code{\link[terra:SpatRaster-class]{terra::SpatRaster}} or \code{stars} with the input data. It
has to include levels with the same names as the variables used in \code{object}}

\item{...}{parameters to be passed to the standard \code{predict()} function for
the appropriate object type (e.g. \code{metric_thresh} or \code{class_thresh}).}
}
\value{
a \code{\link[terra:SpatRaster-class]{terra::SpatRaster}} (or \code{stars} if that is the input) with the
predictions
}
\description{
This function allows to use a raster as data to make predictions from a
variety of \code{tidymodels} objects, such as \code{\link{simple_ensemble}} or
\code{\link[stacks:stacks]{stacks::stacks}}
}
\keyword{predict}
