#' Convert an object created with `blockCV` to an `rsample` object
#'
#' This function converts objects created with `blockCV` to `rsample` objects
#' that can be used by `tidysdm`. BlockCV provides more sophisticated sampling
#' options than the `spatialsample` library. For example, it is possible to
#' stratify the sampling to ensure that presences and absences are evenly
#' distributed among the folds (see the example below).
#'
#' Note that currently only objects of type `cv_spatial` and `cv_cluster` are
#' supported.
#'
#' @param x a object created with a `blockCV` function
#' @param data the `sf` object used to create `x`
#' @returns an `rsample` object
#' @export
#' @examplesIf rlang::is_installed("blockCV")
#' library(blockCV)
#' points <- read.csv(system.file("extdata/", "species.csv",
#'   package = "blockCV"
#' ))
#' pa_data <- sf::st_as_sf(points, coords = c("x", "y"), crs = 7845)
#' sb1 <- cv_spatial(
#'   x = pa_data,
#'   column = "occ", # the response column to balance the folds
#'   k = 5, # number of folds
#'   size = 350000, # size of the blocks in metres
#'   selection = "random", # random blocks-to-fold
#'   iteration = 10
#' ) # find evenly dispersed folds
#' sb1_rsample <- blockcv2rsample(sb1, pa_data)
#' class(sb1_rsample)
#' autoplot(sb1_rsample)
blockcv2rsample <- function(x, data) {
  if (!requireNamespace("blockCV", quietly = TRUE)) {
    stop(
      "to use this function, first install package 'blockCV' with\n",
      "install.packages('blockCV')"
    )
  }
  if (!(any(
    inherits(x, "cv_spatial"),
    inherits(x, "cv_cluster")
  ))) {
    stop(
      "this function does not support this object type\n",
      "only objects of class cv_spatial or cv_cluster are supported."
    )
  }
  if (inherits(data, "SpatialPointsDataFrame")) {
    stop(
      "data is a `SpatialPointsDataFrame`; this object type is deprecated\n",
      "convert your data to `sf` and rebuild your `blockCV` object with it"
    )
  }
  splits <- lapply(
    x$folds_list,
    function(this_fold) {
      names(this_fold) <- c("analysis", "assessment")
      rsample::make_splits(this_fold, data = data, class = "spatial_rsplit")
    }
  )
  rsample::new_rset(splits,
    ids = paste0("Fold", seq_along(splits)),
    attrib = NULL, subclass = c("spatial_rset", "rset")
  )
}
