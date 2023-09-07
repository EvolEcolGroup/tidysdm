#' Filter to retain only variables below a given correlation threshold
#'
#' This method finds a subset of variable such that all have a correlation 
#' below a certain cutoff. There are methods for [`terra::SpatRaster`],
#' [`data.frame`], and to work directly on a correlation matrix that was
#' previously estimated.
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
#' @returns A vector of names of columns that are below the correlation threshold
#' (when \code{names = TRUE}), otherwise a vector of indices.
#'
#' @export

filter_high_cor <- function (x, 
                             cutoff = 0.7,
                             verbose = FALSE,
                             names = TRUE) {
  UseMethod("filter_high_cor", object = x)
}

#' @rdname filter_high_cor
#' @export
filter_high_cor.default <- function(x, cutoff = 0.7,
                                    verbose = FALSE,
                                    names = TRUE){
  stop("no method available for this object type")
}


#' @rdname filter_high_cor
#' @export
filter_high_cor.SpatRaster <-
  function (x,
            cutoff = 0.7,
            verbose = FALSE,
            names = TRUE) {
      cor_matrix <- terra::layerCor(x, "pearson", na.rm = TRUE)$pearson
      dimnames(cor_matrix) <- list(names(x), names(x))
      filter_high_cor(x = cor_matrix,
                      cutoff = cutoff,
                      verbose = verbose,
                      names = names) 
}    
    
#' @rdname filter_high_cor
#' @export
filter_high_cor.matrix <-
      function (x,
                cutoff = 0.7,
                verbose = FALSE,
                names = TRUE) {    

    var_num <- dim(x)[1]
    var_names <- dimnames(x)[[1]]

    if (!isTRUE(all.equal(x, t(x))))
      stop("correlation matrix is not symmetric")
    if (var_num == 1)
      stop("only one variable given")

    x <- abs(x)

    # re-ordered columns based on max absolute correlation
    original_order <- 1:var_num

    average_corr <-
      function(x)
        mean(x, na.rm = TRUE)
    tmp <- x
    diag(tmp) <- NA

    max_abs_cor_order <-
      order(apply(tmp, 2, average_corr), decreasing = TRUE)
    x <- x[max_abs_cor_order, max_abs_cor_order]
    newOrder <- original_order[max_abs_cor_order]
    rm(tmp)

    col_to_delete <- rep(FALSE, var_num)

    x2 <- x
    diag(x2) <- NA

    for (i in 1:(var_num - 1)) {
      if (!any(x2[!is.na(x2)] > cutoff)) {
        if (verbose)
          cat("All correlations <=", cutoff, "\n")
        break()
      }
      if (col_to_delete[i])
        next
      for (j in (i + 1):var_num) {
        if (!col_to_delete[i] & !col_to_delete[j]) {
          if (x[i, j] > cutoff) {
            mn1 <- mean(x2[i, ], na.rm = TRUE)
            mn2 <- mean(x2[-j, ], na.rm = TRUE)
            if (verbose)
              cat(
                "Compare row",
                newOrder[i],
                " and column ",
                newOrder[j],
                "with corr ",
                round(x[i, j], 3),
                "\n"
              )
            if (verbose)
              cat("  Means: ", round(mn1, 3), "vs", round(mn2, 3))
            if (mn1 > mn2) {
              col_to_delete[i] <- TRUE
              x2[i,] <- NA
              x2[, i] <- NA
              if (verbose)
                cat(" so flagging column", newOrder[i], "\n")
            }
            else {
              col_to_delete[j] <- TRUE
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
      passed_filter <- var_names[newOrder][!col_to_delete]
      attr(passed_filter, "to_remove")<- var_names[!var_names %in% passed_filter]
    } else {
      # return their indices
      passed_filter <- newOrder[!col_to_delete]
      attr(passed_filter, "to_remove")<- (1:length(newOrder))[!(1:length(newOrder)) %in% passed_filter]
    }

    return(passed_filter)
      }

#' @rdname filter_high_cor
#' @export
filter_high_cor.data.frame <-
  function (x,
            cutoff = 0.7,
            verbose = FALSE,
            names = TRUE) {
    cor_matrix <- stats::cor(x)
    filter_high_cor(x = cor_matrix,
                    cutoff = cutoff,
                    verbose = verbose,
                    names = names)
  }    
