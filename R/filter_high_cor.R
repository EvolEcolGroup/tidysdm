#' Filter to retain only variables below a given correlation threshold
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

    cor_matrix <- terra::layerCor(x, "pearson", na.rm = TRUE)
    if ("pearson" %in% names(cor_matrix)){
      cor_matrix <- cor_matrix$pearson
    } else {
      cor_matrix <- cor_matrix$correlation
    }
    dimnames(cor_matrix) <- list(names(x), names(x))
    filter_high_cor(
      x = cor_matrix,
      cutoff = cutoff,
      verbose = verbose,
      names = names,
      to_keep
    )
  }


#' @rdname filter_high_cor
#' @export
filter_high_cor.data.frame <-
  function(x,
           cutoff = 0.7,
           verbose = FALSE,
           names = TRUE,
           to_keep = NULL) {
    x <- x %>%
      dplyr::select(dplyr::where(is.numeric)) %>%
      sf::st_drop_geometry()
    cor_matrix <- stats::cor(x)
    filter_high_cor(
      x = cor_matrix,
      cutoff = cutoff,
      verbose = verbose,
      names = names,
      to_keep = to_keep
    )
  }

#' @rdname filter_high_cor
#' @export
filter_high_cor.matrix <-
  function(x,
           cutoff = 0.7,
           verbose = FALSE,
           names = TRUE,
           to_keep = NULL) {
    if (!isTRUE(all.equal(x, t(x)))) {
      stop("correlation matrix is not symmetric")
    }
    if (dim(x)[1] == 1) {
      stop("only one variable given")
    }

    var_names <- dimnames(x)[[1]]
    diag(x) <- NA
    # if we have some variables to force in
    if (!is.null(to_keep)) {
      if (!any(to_keep %in% var_names)) {
        stop("to_keep should only include numeric variables in x")
      }
      if (length(to_keep) > 1) {
        x_keep <- x[to_keep, to_keep]
        # diag(x_keep)<-NA
        if (any(x_keep > cutoff, na.rm = TRUE)) {
          stop("some variables in `to_keep` have a correlation higher than the `cutoff`")
        }
        max_cor_vs_keep <- apply(abs(x[, to_keep]), 1, max, na.rm = TRUE)
      } else { # if only 1 var to keep, we just need to take the abs value of correlations
        max_cor_vs_keep <- abs(x[, to_keep])
      }
      # remove variables that are too highly correlated with variables to keep

      x <- x[
        !var_names %in% names(which(max_cor_vs_keep > cutoff)),
        !var_names %in% names(which(max_cor_vs_keep > cutoff))
      ]
      x <- x[!dimnames(x)[[1]] %in% to_keep, !dimnames(x)[[1]] %in% to_keep]
    }
    filter_output <- filter_high_cor_algorithm(
      x = x,
      cutoff = cutoff,
      verbose = verbose
    )
    if (!is.null(to_keep)) {
      to_remove <- attr(filter_output, "to_remove")
      filter_output <- c(to_keep, filter_output)
      attr(filter_output, "to_remove") <- to_remove
    }


    if (!names) {
      # return their indices
      to_remove <- match(attr(filter_output, "to_remove"), var_names)
      filter_output <- match(filter_output, var_names)
      attr(filter_output, "to_remove") <- to_remove
    }
    return(filter_output)
  }

#' @rdname filter_high_cor
#' @export
# this funciton is only ever called from the cor.matrix method
filter_high_cor_algorithm <-
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
    newOrder <- original_order[max_abs_cor_order]
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
        if (!col_to_delete[i] & !col_to_delete[j]) {
          if (x[i, j] > cutoff) {
            mn1 <- mean(x2[i, ], na.rm = TRUE)
            mn2 <- mean(x2[-j, ], na.rm = TRUE)
            if (verbose) {
              message(
                "Compare row",
                newOrder[i],
                " and column ",
                newOrder[j],
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
                message(" so flagging column", newOrder[i], "\n")
              }
            } else {
              col_to_delete[j] <- TRUE
              x2[j, ] <- NA
              x2[, j] <- NA
              if (verbose) {
                message(" so flagging column", newOrder[j], "\n")
              }
            }
          }
        }
      }
    }

    # return variable names
    passed_filter <- var_names[newOrder][!col_to_delete]
    attr(passed_filter, "to_remove") <- var_names[!var_names %in% passed_filter]

    return(passed_filter)
  }
