#' @keywords internal
#' @noRd
filter_vif_step <- function(x, cutoff = 10, verbose = FALSE, to_keep = NULL,
                            size, cor_type = "pearson") {
  if (is.null(cutoff)) {
    cutoff <- 10
  }
  # names of all variables (from the full matrix)
  var_names <- colnames(x)

  vars_to_remove <- c()
  # remove the variable with the largest vif, one at a time
  while (TRUE) {
    i_cols <- (seq_len(ncol(x)))[!colnames(x) %in% to_keep]
    vif_vector <- vif_fast(x, cols = i_cols)
    if (max(vif_vector) >= cutoff) {
      target_var <- names(vif_vector)[which.max(vif_vector)]
      vars_to_remove <- c(vars_to_remove, target_var)
      x <- x[, -which(colnames(x) == target_var)]
    } else {
      break
    }
    # break if we only have one variable left
    if (ncol(x) == 1) {
      break
    }
  }
  vars_kept <- var_names[!var_names %in% vars_to_remove]
  if (verbose) {
    message("vif of retained variables")
    print(vif_fast(x))
    message("correlation matrix of retained variables")
    print(stats::cor(x, method = cor_type))
    print("----")
  }
  vars_kept
}

#' @keywords internal
#' @noRd
filter_vif_cor <- function(x, cutoff = 10, verbose = FALSE,
                           to_keep = NULL, size, cor_type = "pearson") {
  if (is.null(cutoff)) {
    cutoff <- 0.7
  }
  # names of all variables (from the full matrix)
  var_names <- colnames(x)
  x_cor <- stats::cor(x, method = cor_type)
  x_cor <- abs(x_cor)
  # create a dataframe
  x_cor <- data.frame(
    row = rownames(x_cor)[row(x_cor)[upper.tri(x_cor)]],
    col = colnames(x_cor)[col(x_cor)[upper.tri(x_cor)]],
    cor = x_cor[upper.tri(x_cor)]
  )
  # order it by correlation coefficient
  x_cor <- x_cor %>%
    dplyr::arrange(dplyr::desc(.data$cor)) %>%
    # remove comparisons among variables to keep
    dplyr::filter(!(row %in% to_keep & col %in% to_keep))

  vars_to_remove <- c()
  # work down the highest correlations above cutoff
  while (TRUE) {
    if (x_cor$cor[1] > cutoff) {
      # target variables to consider
      target_vars <- c(x_cor$row[1], x_cor$col[1])
      target_vars <- target_vars[!target_vars %in% to_keep]
      # if we still have two variables (i.e. neither is in to_keep)
      if (length(target_vars) > 1) {
        vif_vector <- vif_fast(x, match(target_vars, colnames(x)))
        target_var <- names(vif_vector)[which.max(vif_vector)]
      } else {
        target_var <- target_vars
      }

      # choose vars with larger vif
      x_cor <- x_cor %>% dplyr::filter(!(col == target_var | row == target_var))
      vars_to_remove <- c(vars_to_remove, target_var)
      x <- x[, -which(colnames(x) == target_var)]
    } else {
      break
    }
    # break if we don't have any correlations left to investigate
    if (nrow(x_cor) == 0) {
      break
    }
  }
  vars_kept <- var_names[!var_names %in% vars_to_remove]
  if (verbose) {
    message("vif of retained variables")
    print(vif_fast(x))
    message("correlation matrix of retained variables")
    print(stats::cor(x, method = cor_type))
    print("----")
  }
  vars_kept
}


# estimate the variance inflation factor
# vif_fast only works with continuous variables
# note that cols are indices of the columns of interest
vif_fast <- function(data_mat, cols = NULL) {
  # in case we passed a data.frame, fix it and transform it into a matrix
  if (inherits(data_mat, "data.frame")) {
    data_mat <- as.matrix(data_mat)
  }
  if (is.null(cols)) {
    cols <- seq.int(1, ncol(data_mat))
  }
  var_names <- colnames(data_mat)
  data_mat <- cbind(data_mat, 1) ## convert to a design matrix with an intercept
  vif_one_col <- function(i_col, data_mat) {
    this_resid <- stats::.lm.fit(data_mat[, -i_col],
      y = data_mat[, i_col]
    )$residuals
    return(1 / (sum(this_resid^2) /
      sum((data_mat[, i_col] - mean(data_mat[, i_col]))^2))) # nolint
  }
  vif_vector <- sapply(cols, FUN = vif_one_col, data_mat = data_mat)
  names(vif_vector) <- var_names[cols]
  vif_vector
}
