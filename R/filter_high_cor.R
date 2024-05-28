#' Deprecated: Filter to retain only variables below a given correlation threshold
#'
#' THIS FUNCTION IS DEPRECATED. USE `filter_collinear` with `method=cor_caret` instead
#'
#' This method finds a subset of variable such that all have a correlation
#' below a certain cutoff. There are methods for [`terra::SpatRaster`],
#' [`data.frame`], and to work directly on a correlation matrix that was
#' previously estimated. For `data.frame`, only numeric variables will be
#' considered.
#' The algorithm is based on `caret::findCorrelation`, using the `exact` option.
#' The absolute values of pair-wise correlations are considered. If two
#' variables have a high correlation, the function looks at the mean absolute
#' correlation of each variable and removes the variable with the largest mean
#' absolute correlation.
#'
#' There are several function in the package `subselect`
#' that can also be used to accomplish
#' the same goal but tend to retain more predictors.
#'
#' @param x A [`terra::SpatRaster`] object, a data.frame (with only numeric
#' variables), or a correlation matrix
#' @param cutoff A numeric value for the pair-wise absolute correlation cutoff
#' @param verbose A boolean for printing the details
#' @param names a logical; should the column names be returned `TRUE` or
#' the column index `FALSE`)?
#' @param to_keep A vector of variable names that we want to force in the set
#' (note that the function will return an error if the correlation among any of
#' those variables is higher than the cutoff).
#' @returns A vector of names of columns that are below the correlation threshold
#' (when \code{names = TRUE}), otherwise a vector of indices. Note that the indices
#' are only for numeric variables (i.e. if factors are present, the indices do
#' not take them into account).
#'
#' @export

filter_high_cor <- function(x,
                            cutoff = 0.7,
                            verbose = FALSE,
                            names = TRUE,
                            to_keep = NULL) {
  UseMethod("filter_high_cor", object = x)
}

#' @rdname filter_high_cor
#' @export
filter_high_cor.default <- function(x, cutoff = 0.7,
                                    verbose = FALSE,
                                    names = TRUE,
                                    to_keep = NULL) {
  stop("no method available for this object type")
}


#' @rdname filter_high_cor
#' @export
filter_high_cor.SpatRaster <-
  function(x,
           cutoff = 0.7,
           verbose = FALSE,
           names = TRUE,
           to_keep = NULL) {
    warning("this function is deprecated, use `filter_collinear` instead")
    filter_collinear(x, method="cor_caret", cutoff=cutoff, verbose=verbose, names = names, to_keep= to_keep)
  }


#' @rdname filter_high_cor
#' @export
filter_high_cor.data.frame <-
  function(x,
           cutoff = 0.7,
           verbose = FALSE,
           names = TRUE,
           to_keep = NULL) {
    warning("this function is deprecated, use `filter_collinear` instead")
    filter_collinear(x, method="cor_caret", cutoff=cutoff, verbose=verbose, names = names, to_keep= to_keep)
  }

#' @rdname filter_high_cor
#' @export
filter_high_cor.matrix <-
  function(x,
           cutoff = 0.7,
           verbose = FALSE,
           names = TRUE,
           to_keep = NULL) {
    stop("this method is no longer available. Use `filter_collinear` on the SpatRaster or data.frame")
  }