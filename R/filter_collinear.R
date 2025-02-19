#' Filter to retain only variables that have low collinearity
#'
#' This method finds a subset of variables that have low collinearity. It
#' provides three methods: `cor_caret`, a stepwise approach to remove variables
#' with a pairwise correlation above a given cutoff, choosing the variable with
#' the greatest mean correlation (based on the algorithm in
#' `caret::findCorrelation`); `vif_step`, a stepwise approach to remove
#' variables with an variance inflation factor above a given cutoff (based on
#' the algorithm in `usdm::vifstep`), and `vif_cor`, a stepwise approach that,
#' at each step, find the pair of variables with the highest correlation above
#' the cutoff and removes the one with the largest vif. such that all have a
#' correlation below a certain cutoff. There are methods for
#' [`terra::SpatRaster`], [`data.frame`] and [`matrix`]. For
#' [`terra::SpatRaster`] and `data.frame`, only numeric variables will be
#' considered.
#'
#' @param x A [`terra::SpatRaster`] or `stars` object, a data.frame (with only
#'   numeric variables)
#' @param cutoff A numeric value used as a threshold to remove variables. For,
#'   "cor_caret" and "vif_cor", it is the pair-wise absolute correlation cutoff,
#'   which defaults to 0.7. For "vif_step", it is the variable inflation factor,
#'   which defaults to 10
#' @param verbose A boolean whether additional information should be provided on
#'   the screen
#' @param names a logical; should the column names be returned `TRUE` or the
#'   column index `FALSE`)?
#' @param to_keep A vector of variable names that we want to force in the set
#'   (note that the function will return an error if the correlation among any
#'   of those variables is higher than the cutoff).
#' @param method character. One of "cor_caret", "vif_cor" or "vif_step".
#' @param cor_type character. For methods that use correlation, which type of
#'   correlation: "pearson", "kendall", or "spearman". Defaults to "pearson"
#' @param max_cells positive integer. The maximum number of cells to be used. If
#'   this is smaller than ncell(x), a regular sample of x is used
#' @param ... additional arguments specific to a given object type
#' @returns A vector of names of columns that are below the correlation
#'   threshold (when \code{names = TRUE}), otherwise a vector of indices. Note
#'   that the indices are only for numeric variables (i.e. if factors are
#'   present, the indices do not take them into account).
#' @author for `cor_caret`: Original R code by Dong Li, modified by Max Kuhn and
#'   Andrea Manica; for `vif_step` and `vif_cor`, original algorithm by Babak
#'   Naimi, rewritten by Andrea Manica for `tidysdm`
#' @references Naimi, B., Hamm, N.A.S., Groen, T.A., Skidmore, A.K., and
#'   Toxopeus, A.G. 2014. Where is positional uncertainty a problem for species
#'   distribution modelling?, Ecography 37 (2): 191-203.
#' @export

filter_collinear <- function(x,
                             cutoff = NULL,
                             verbose = FALSE,
                             names = TRUE,
                             to_keep = NULL,
                             method = "cor_caret",
                             cor_type = "pearson",
                             max_cells = Inf,
                             ...) {
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
                                     max_cells = Inf,
                                     ...) {
  stop("no method available for this object type")
}

#' @rdname filter_collinear
#' @param exhaustive boolean. Used only for [`terra::SpatRaster`] when
#'   downsampling to `max_cells`, if we require the `exhaustive` approach in
#'   [terra::spatSample()]. This is only needed for rasters that are very sparse
#'   and not too large, see the help page of [terra::spatSample()] for details.
#' @export
filter_collinear.stars <-
  function(x,
           cutoff = NULL,
           verbose = FALSE,
           names = TRUE,
           to_keep = NULL,
           method = "cor_caret",
           cor_type = "pearson",
           max_cells = Inf,
           exhaustive = FALSE,
           ...) {
    N <- prod(dim(x))
    maxcells <- pmin(N, max_cells)
    ix <- sample(N, maxcells, replace = FALSE)
    x_matrix <- sapply(names(x),
      function(name, x = NULL, index = NULL) {
        x[[name]][index]
      },
      x = x, index = ix, simplify = FALSE
    ) %>%
      as.data.frame() %>%
      stats::na.omit() %>%
      as.matrix()

    # now dispatch to the matrix method
    filter_collinear(
      x_matrix,
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
#' @param exhaustive boolean. Used only for [`terra::SpatRaster`] when
#'   downsampling to `max_cells`, if we require the `exhaustive` approach in
#'   [terra::spatSample()]. This is only needed for rasters that are very sparse
#'   and not too large, see the help page of [terra::spatSample()] for details.
#' @export
filter_collinear.SpatRaster <-
  function(x,
           cutoff = NULL,
           verbose = FALSE,
           names = TRUE,
           to_keep = NULL,
           method = "cor_caret",
           cor_type = "pearson",
           max_cells = Inf,
           exhaustive = FALSE,
           ...) {
    # if raster is bigger than max_cells, then sample
    if (max_cells < terra::ncell(x)) {
      x_matrix <- terra::spatSample(x,
        size = max_cells,
        method = "random", na.rm = TRUE, as.df = FALSE,
        exhaustive = exhaustive
      )
    } else {
      x_matrix <- stats::na.omit(terra::as.matrix(x))
    }
    # now dispatch to the matrix method
    filter_collinear(
      x_matrix,
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
           max_cells = Inf,
           ...) {
    x <- x %>%
      # do we need this?!? check vif cor and vif step
      dplyr::select(dplyr::where(is.numeric)) %>%
      sf::st_drop_geometry() %>%
      stats::na.omit()
    # sample rows if we have too many
    if (max_cells < nrow(x)) {
      ## sample rows
      x <- x %>% dplyr::slice_sample(n = max_cells)
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
                                    max_cells = Inf,
                                    ...) {
  if (ncol(x) < 2) {
    stop("at least 2 numeric variables are needed")
  }

  # check that to_keep is valid
  if (!is.null(to_keep)) {
    if (!any(to_keep %in% colnames(x))) {
      stop("to_keep includes variables that are not present in x")
    }
  }

  # sample rows if needed
  if (max_cells < nrow(x)) {
    x <- x[sample(1:nrow(x), max_cells), ]
  }

  # now dispatch to the correct method
  if (method == "cor_caret") {
    vars_kept <- filter_cor_caret(
      x,
      cutoff = cutoff,
      verbose = verbose,
      to_keep = to_keep,
      cor_type = cor_type
    )
  } else if (method == "vif_step") {
    vars_kept <- filter_vif_step(
      x,
      cutoff = cutoff,
      verbose = verbose,
      to_keep = to_keep
    )
  } else if (method == "vif_cor") {
    vars_kept <- filter_vif_cor(
      x,
      cutoff = cutoff,
      verbose = verbose,
      to_keep = to_keep,
      cor_type = cor_type
    )
  } else {
    stop(
      "the selected method is not valid: only options 'cor_caret', 'vif_step'",
      " and 'vif_cor' are accepted."
    )
  }
  # format the output of the function
  var_names <- colnames(x)
  attr(vars_kept, "to_remove") <- var_names[!var_names %in% vars_kept]

  if (!names) {
    # return their indices
    vars_kept <- match(vars_kept, var_names)
    attr(vars_kept, "to_remove") <- match(var_names[!var_names %in% vars_kept], var_names)
  }
  return(vars_kept)
}
