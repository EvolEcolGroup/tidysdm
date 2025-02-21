#' Filter to retain only variables below a given correlation threshold
#'
#' This method finds a subset of variable such that all have a correlation
#' below a certain cutoff.
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
#' @param x a matrix
#' @param cutoff A numeric value for the pair-wise absolute correlation cutoff
#' @param verbose A boolean for printing the details
#' @param to_keep A vector of variable names that we want to force in the set
#'   (note that the function will return an error if the correlation among any
#'   of those variables is higher than the cutoff).
#' @returns A vector of names of columns that are below the correlation
#'   threshold (when \code{names = TRUE}), otherwise a vector of indices. Note
#'   that the indices are only for numeric variables (i.e. if factors are
#'   present, the indices do not take them into account).
#'
#' @keywords internal
#' @noRd

filter_cor_caret <- function(x,
                             cutoff = NULL,
                             verbose = FALSE,
                             cor_type = "pearson",
                             to_keep = NULL) {
  if (is.null(cutoff)) {
    cutoff <- 0.7
  }
  var_names <- colnames(x)
  # create a correlation matrix
  x <- stats::cor(x, method = cor_type)
  diag(x) <- NA
  #  x <- abs(x) # nolint
  if (!is.null(to_keep)) {
    if (!any(to_keep %in% var_names)) {
      stop("to_keep should only include numeric variables in x")
    }
    # check that the variables to keep are not too highly correlated to start
    # with
    if (length(to_keep) > 1) {
      x_keep <- x[to_keep, to_keep]
      # diag(x_keep)<-NA #nolint
      if (any(x_keep > cutoff, na.rm = TRUE)) {
        stop("some variables in `to_keep` have a correlation higher ",
             "than the `cutoff`")
      }
      max_cor_vs_keep <-
        apply(abs(x[, to_keep]), 1, max, na.rm = TRUE)
    } else {
      # if only 1 var to keep, we just need to take the abs value of
      # correlations
      max_cor_vs_keep <- abs(x[, to_keep])
    }
    # remove variables that are too highly correlated with variables to keep
    x <- x[!var_names %in%
             names(which(max_cor_vs_keep > cutoff)),
           !var_names %in% names(which(max_cor_vs_keep > cutoff))]
    x <-
      x[!dimnames(x)[[1]] %in% to_keep, !dimnames(x)[[1]] %in% to_keep]
  }
  filter_output <- filter_caret_algorithm(
    x = x,
    cutoff = cutoff,
    verbose = verbose
  )
  if (!is.null(to_keep)) {
    filter_output <- c(to_keep, filter_output)
  }

  return(filter_output)
}

#' @keywords internal
#' @noRd
# this funciton is a modified version of the caret algorithm
filter_caret_algorithm <-
  function(x,
           cutoff = 0.7,
           verbose = FALSE) {
    var_num <- dim(x)[1]
    var_names <- dimnames(x)[[1]]

    x <- abs(x)

    # re-ordered columns based on max absolute correlation
    original_order <- 1:var_num

    average_corr <-
      function(x) {
        mean(x, na.rm = TRUE)
      }
    tmp <- x
    diag(tmp) <- NA

    max_abs_cor_order <-
      order(apply(tmp, 2, average_corr), decreasing = TRUE)
    x <- x[max_abs_cor_order, max_abs_cor_order]
    new_order <- original_order[max_abs_cor_order]
    rm(tmp)

    col_to_delete <- rep(FALSE, var_num)

    x2 <- x
    diag(x2) <- NA

    for (i in 1:(var_num - 1)) {
      if (!any(x2[!is.na(x2)] > cutoff)) {
        if (verbose) {
          message("All correlations <=", cutoff, "\n")
        }
        break()
      }
      if (col_to_delete[i]) {
        next
      }
      for (j in (i + 1):var_num) {
        if (!col_to_delete[i] && !col_to_delete[j]) {
          if (x[i, j] > cutoff) {
            mn1 <- mean(x2[i, ], na.rm = TRUE)
            mn2 <- mean(x2[-j, ], na.rm = TRUE)
            if (verbose) {
              message(
                "Compare row",
                new_order[i],
                " and column ",
                new_order[j],
                "with corr ",
                round(x[i, j], 3),
                "\n"
              )
            }
            if (verbose) {
              message("  Means: ", round(mn1, 3), "vs", round(mn2, 3))
            }
            if (mn1 > mn2) {
              col_to_delete[i] <- TRUE
              x2[i, ] <- NA
              x2[, i] <- NA
              if (verbose) {
                message(" so flagging column", new_order[i], "\n")
              }
            } else {
              col_to_delete[j] <- TRUE
              x2[j, ] <- NA
              x2[, j] <- NA
              if (verbose) {
                message(" so flagging column", new_order[j], "\n")
              }
            }
          }
        }
      }
    }

    # return variable names
    passed_filter <- var_names[new_order][!col_to_delete]
    attr(passed_filter, "to_remove") <-
      var_names[!var_names %in% passed_filter]

    return(passed_filter)
  }
