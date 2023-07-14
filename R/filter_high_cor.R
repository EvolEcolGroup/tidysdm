#' Filter to retain only variables below a given correlation threshold
#'
#' This function estimates the correlation matrix among variables in a
#' [terra::SpatRaster] and then returns a vector of variable names or indeces
#' that are below a certain cutoff.
#' The algorithm is based on `caret::findCorrelation`, using the `exact` option.
#' The absolute values of pair-wise correlations are considered. If two
#' variables have a high correlation, the function looks at the mean absolute
#' correlation of each variable and removes the variable with the largest mean
#' absolute correlation.
#'
#' There are several function in the `subselect`
#' that can also be used to accomplish
#' the same goal but tend to retain more predictors.
#'
#' @param x A [`terra::SpatRaster`] object
#' @param cutoff A numeric value for the pair-wise absolute correlation cutoff
#' @param verbose A boolean for printing the details
#' @param names a logical; should the column names be returned `TRUE` or
#' the column index `FALSE`)?
#' @returns A vector of names of columns that are below the correlation threhold
#' (when \code{names = TRUE}), otherwise a vector of indeces.
#'
#' @export

## TODO create methods for data.frame, and for using directly the correlation matrix

filter_high_cor <-
  function (x,
            cutoff = 0.7,
            verbose = FALSE,
            names = TRUE) {
    if (inherits(x, "SpatRaster")) {
      cor_matrix <- terra::layerCor(x, "pearson", na.rm = TRUE)$pearson
      dimnames(cor_matrix) <- list(names(x), names(x))
    } # we could do an else for data.frames

    varnum <- dim(cor_matrix)[1]

    if (!isTRUE(all.equal(cor_matrix, t(cor_matrix))))
      stop("correlation matrix is not symmetric")
    if (varnum == 1)
      stop("only one variable given")

    cor_matrix <- abs(cor_matrix)

    # re-ordered columns based on max absolute correlation
    originalOrder <- 1:varnum

    averageCorr <-
      function(cor_matrix)
        mean(cor_matrix, na.rm = TRUE)
    tmp <- cor_matrix
    diag(tmp) <- NA

    maxAbsCorOrder <-
      order(apply(tmp, 2, averageCorr), decreasing = TRUE)
    cor_matrix <- cor_matrix[maxAbsCorOrder, maxAbsCorOrder]
    newOrder <- originalOrder[maxAbsCorOrder]
    rm(tmp)

    deletecol <- rep(FALSE, varnum)

    x2 <- cor_matrix
    diag(x2) <- NA

    for (i in 1:(varnum - 1)) {
      if (!any(x2[!is.na(x2)] > cutoff)) {
        if (verbose)
          cat("All correlations <=", cutoff, "\n")
        break()
      }
      if (deletecol[i])
        next
      for (j in (i + 1):varnum) {
        if (!deletecol[i] & !deletecol[j]) {
          if (cor_matrix[i, j] > cutoff) {
            mn1 <- mean(x2[i, ], na.rm = TRUE)
            mn2 <- mean(x2[-j, ], na.rm = TRUE)
            if (verbose)
              cat(
                "Compare row",
                newOrder[i],
                " and column ",
                newOrder[j],
                "with corr ",
                round(cor_matrix[i, j], 3),
                "\n"
              )
            if (verbose)
              cat("  Means: ", round(mn1, 3), "vs", round(mn2, 3))
            if (mn1 > mn2) {
              deletecol[i] <- TRUE
              x2[i,] <- NA
              x2[, i] <- NA
              if (verbose)
                cat(" so flagging column", newOrder[i], "\n")
            }
            else {
              deletecol[j] <- TRUE
              x2[j,] <- NA
              x2[, j] <- NA
              if (verbose)
                cat(" so flagging column", newOrder[j], "\n")
            }
          }
        }
      }
    }
    # should we sort these variables?
    if (names) {
      # return variable names
      passed_filter <- names(x)[newOrder][!deletecol]
      attr(passed_filter, "to_remove")<- names(x)[!names(x) %in% passed_filter]
    } else {
      # return their indeces
      passed_filter <- newOrder[!deletecol]
      attr(passed_filter, "to_remove")<- (1:length(newOrder))[!(1:length(newOrder)) %in% passed_filter]
    }

    return(passed_filter)
  }
