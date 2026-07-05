#' Convert an object created with `blockCV` to an `rsample` object
#'
#' This function converts objects created with `blockCV` to `rsample` objects
#' that can be used by `tidysdm`. BlockCV provides more sophisticated sampling
#' options than the `spatialsample` library. For example, it is possible to
#' stratify the sampling to ensure that presences and absences are evenly
#' distributed among the folds (see the example below).
#'
#' Note that currently only objects of type `cv_spatial`, `cv_cluster`,
#' `cv_nndm` and `cv_buffer` are supported. The latter two are one-out-cross
#' validation methods, so the resulting `rsample` object will have `n` splits,
#' where `n` is the number of folds in the original `blockCV` object (which can
#' be very large!).
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
#' )
#'
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
    inherits(x, "cv_cluster"),
    inherits(x, "cv_nndm"),
    inherits(x, "cv_buffer")
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
  # Build splits (common to both cv_spatial and cv_cluster)
  splits <- lapply(
    x$folds_list,
    function(this_fold) {
      names(this_fold) <- c("analysis", "assessment")
      # split data into two dataframes (to avoid issues with leave one croosval
      # since make_split fails if x is a list and assesment is of length 1)
      rsample::make_splits(data[this_fold$analysis, ],
        assessment = data[this_fold$assessment, ,
          drop = FALSE
        ],
        class = "spatial_rsplit"
      )
    }
  )

  # Determine subclass based on object type
  subclass_type <- if (inherits(x, "cv_spatial")) {
    "cv_spatial"
  } else if (inherits(x, "cv_cluster")) {
    "cv_cluster"
  } else if (inherits(x, "cv_nndm")) {
    "cv_nndm"
  } else if (inherits(x, "cv_buffer")) {
    "cv_buffer"
  }

  rsample::new_rset(splits,
    ids = paste0("Fold", seq_along(splits)),
    attrib = NULL,
    subclass = c(subclass_type, "spatial_rset", "rset")
  )
}
