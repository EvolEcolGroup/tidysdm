#' Filter to retain only variables that have low collinearity
#'
#' This method finds a subset of variables that have low collinearity. It provides
#' three methods: `cor_caret`, a stepwise approach to remove variables with a pairwise correlation
#' above a given cutoff, choosing the variable with the greatest mean correlation (based on the algorithm in `caret::findCorrelation`);
#' `vif_step`, a stepwise approach to remove variables with an variance inflation factor
#' above a given cutoff (based on the algorithm in `usdm::vifstep`), and `vif_cor`, a stepwise
#' approach that, at each step, find the pair of variables with the highest correlation above the cutoff and removes the
#' one with the largest vif.
#' such that all have a correlation
#' below a certain cutoff. There are methods for [`terra::SpatRaster`], and
#' [`data.frame`]. Only numeric variables will be considered. For `data.frame`, only numeric variables will be
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
#' variables)
#' @param cutoff A numeric value used as a threshold to remove variables. For, "caret",
#' it is the pair-wise absolute correlation cutoff, which defaults to 0.7. For "vifstep"
#' and "vifcor", it is the variable inflation factor, which defaults to 10
#' @param verbose A boolean whether additional information should be provided
#' on the screen
#' @param names a logical; should the column names be returned `TRUE` or
#' the column index `FALSE`)?
#' @param to_keep A vector of variable names that we want to force in the set
#' (note that the function will return an error if the correlation among any of
#' those variables is higher than the cutoff).
#' @param method character. One of "cor_caret", "vif_cor" or "vif_step".
#' @param cor_type character. For methods that use correlation, which type of correlation: "pearson", "kendall", or "spearman"
#' @param maxcell positive integer. The maximum number of cells to be used. If this is smaller than ncell(x), a regular sample of x is used
#' @returns A vector of names of columns that are below the correlation threshold
#' (when \code{names = TRUE}), otherwise a vector of indices. Note that the indices
#' are only for numeric variables (i.e. if factors are present, the indices do
#' not take them into account).
#'
#' @export

filter_collinear <- function(x,
                            cutoff = NULL,
                            verbose = FALSE,
                            names = TRUE,
                            to_keep = NULL,
                            method = "caret",
                            maxcell = Inf) {
  UseMethod("filter_collinear", object = x)
}

#' @rdname filter_collinear
#' @export
filter_collinear.default <- function(x,
                                    cutoff = NULL,
                                    verbose = FALSE,
                                    names = TRUE,
                                    to_keep = NULL,
                                    method = "caret",
                                    max_cells = Inf) {
  stop("no method available for this object type")
}


#' @rdname filter_collinear
#' @export
filter_collinear.SpatRaster <-
  function(x,
           cutoff = 0.7,
           verbose = FALSE,
           names = TRUE,
           to_keep = NULL,
           method = "caret",
           max_cells = Inf) {
    # special case for method "caret", where we avoid sampling the raster and use
    # the fast layerCor function in terra
    if (method == "caret") {
      filter_caret_SpatRaster(
        x,
        cutoff = cutoff,
        verbose = verbose,
        names = names,
        to_keep = to_keep,
        max_cells = max_cells
      )
    } else {
      # if max_cells > ncell, then sample
      if (max_cells >= terra::ncell(x)) {
        x_df <- terra::spatSample(x, size = max_cells, na.rm = TRUE)
      } else {
        x_df <- terra::as.data.frame(x, na.rm = TRUE)
      }
      # now dispatch to the data.frame method
      filter_collinear(
        x,
        cutoff = cutoff,
        verbose = verbose,
        names = names,
        to_keep = to_keep,
        method = method,
        max_cells = max_cells
      )
    }
  }


#' @rdname filter_collinear
#' @export
filter_collinear.data.frame <-
  function(x,
           cutoff = NULL,
           verbose = FALSE,
           names = TRUE,
           to_keep = NULL,
           method = "caret",
           max_cells = Inf) {
    x <- x %>%
      # do we need this?!? check vif cor and vif step
      dplyr::select(dplyr::where(is.numeric)) %>%
      sf::st_drop_geometry()
   # sample rows if we have too many
   if (max_cells < nrow(x)){
     ##sample rows
     x <- x %>% slice_sample(n=max_cells)
   }
    # now dispatch to the df version of each method
  if (method == "caret") {
    filter_caret_df(
      x,
      cutoff = cutoff,
      verbose = verbose,
      names = names,
      to_keep = to_keep,
      max_cells = max_cells
    )
  } else if (method == "vifstep") {
    filter_vifstep_df(
      x,
      cutoff = cutoff,
      verbose = verbose,
      names = names,
      to_keep = to_keep,
      max_cells = max_cells
    )    
  } else if (method == "vifcor") {
    filter_vifcor_df(
      x,
      cutoff = cutoff,
      verbose = verbose,
      names = names,
      to_keep = to_keep,
      max_cells = max_cells
    )    
  } else {
    stop (
      "the selected method is not valid: only options 'caret', 'vifstep' and 'vifcor' are accepted."
    )
  }
}

.test_keep_cor <- function(x, to_keep){
  
  
  
  
}