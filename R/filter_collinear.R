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
#' below a certain cutoff. There are methods for [`terra::SpatRaster`],
#' [`data.frame`] and [`matrix`]. For [`terra::SpatRaster`] and `data.frame`, only numeric variables will be
#' considered.

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
#' @param max_cell positive integer. The maximum number of cells to be used. If this is smaller than ncell(x), a regular sample of x is used
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
                            method = "cor_caret",
                            cor_type = "pearson",
                            max_cells = Inf) {
  UseMethod("filter_collinear", object = x)
}

#' @rdname filter_collinear
#' @export
filter_collinear.default <- function(x,
                                    cutoff = NULL,
                                    verbose = FALSE,
                                    names = TRUE,
                                    to_keep = NULL,
                                    method = "cor_caret",
                                    cor_type = "pearson",
                                    max_cells = Inf) {
  stop("no method available for this object type")
}


#' @rdname filter_collinear
#' @param exhaustive boolean. Used only for [`terra::SpatRaster`] when downsampling
#' to `max_cells`, if we require the `exhaustive` approach in [terra::spatSample()].
#' This is only needed for rasters that are very sparse and not too large, see the help
#' page of [terra::spatSample()] for details.
#' @export
filter_collinear.SpatRaster <-
  function(x,
           cutoff = 0.7,
           verbose = FALSE,
           names = TRUE,
           to_keep = NULL,
           method = "cor_caret",
           cor_type = "pearson",
           max_cells = Inf,
           exhaustive = FALSE) {
      # if max_cells > ncell, then sample
      if (max_cells >= terra::ncell(x)) {
        x_df <- terra::spatSample(x, size = max_cells, na.rm = TRUE, as.df = FALSE,
                                  exhaustive = exhaustive)
      } else {
        x_df <- terra::as.matrix(x, na.rm = TRUE)
      }
      # now dispatch to the matrix method
      filter_collinear(
        x,
        cutoff = cutoff,
        verbose = verbose,
        names = names,
        to_keep = to_keep,
        method = method,
        cor_type = cor_type,
        max_cells = max_cells
      )
    }



#' @rdname filter_collinear
#' @export
filter_collinear.data.frame <-
  function(x,
           cutoff = NULL,
           verbose = FALSE,
           names = TRUE,
           to_keep = NULL,
           method = "cor_caret",
           cor_type = "pearson",
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
   x <- as.matrix(x)
   # now dispatch to the matrix method
   filter_collinear(
     x,
     cutoff = cutoff,
     verbose = verbose,
     names = names,
     to_keep = to_keep,
     method = method,
     cor_type = cor_type,
     max_cells = max_cells
   )
}
    
    
#' @rdname filter_collinear
#' @export
filter_collinear.matrix <- function(x,
                                    cutoff = NULL,
                                    verbose = FALSE,
                                    names = TRUE,
                                    to_keep = NULL,
                                    method = "cor_caret",
                                    cor_type = "pearson",
                                    max_cells = Inf) {
  if (ncol(x) <2) {
    stop("at least 2 numeric variables are needed")
  }
  
  # check that to_keep is valid
  if (!is.null(to_keep)){
    if (!any(to_keep %in% colnames(x))) {
      stop("to_keep includes variables that are not present in x")
    }    
  }

  
  # sample rows if needed
  if (max_cells < nrow(x)){
    x <- x [sample(1:nrwo(x),max_cells),]
  }

  # now dispatch to the correct method
  if (method == "cor_caret") {
    filter_cor_caret(
      x,
      cutoff = cutoff,
      verbose = verbose,
      names = names,
      to_keep = to_keep,
      cor_type = cor_type
    )
  } else if (method == "vif_step") {
    filter_vifstep(
      x,
      cutoff = cutoff,
      verbose = verbose,
      names = names,
      to_keep = to_keep
    )    
  } else if (method == "vif_cor") {
    filter_vifcor(
      x,
      cutoff = cutoff,
      verbose = verbose,
      names = names,
      to_keep = to_keep,
      cor_type = cor_type
    )    
  } else {
    stop (
      "the selected method is not valid: only options 'cor_caret', 'vif_step' and 'vif_cor' are accepted."
    )
  }
}

#.check_keep_cor <- function(x, to_keep){
  
  
  
  
#}

