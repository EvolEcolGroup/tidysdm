#' Convert an object created with `blockCV` to an `rsample` object
#'
#' This function creates objects created with `blockCV` to `rsample` objects
#' that can be used by `tidysdm`
#' @param x a object created with a `blockCV` function
#' @param data the `sf` object used to create `x`
#' @returns an `rsample` object
#' @export
#' @examples
#' \donttest{
#' library(blockCV)
#' points <- read.csv(system.file("extdata/", "species.csv", package = "blockCV"))
#' pa_data <- sf::st_as_sf(points, coords = c("x", "y"), crs = 7845)
#' sb1 <- cv_spatial(
#'   x = pa_data,
#'   column = "occ", # the response column (binary or multi-class)
#'   k = 5, # number of folds
#'   size = 350000, # size of the blocks in metres
#'   selection = "random", # random blocks-to-fold
#'   iteration = 10
#' ) # find evenly dispersed folds
#' sb1_rsample <- blockcv2rsample(sb1, pa_data)
#' class(sb1_rsample)
#' }
blockcv2rsample <- function(x, data) {
  splits <- lapply(
    x$folds_list,
    function(this_fold) {
      names(this_fold) <- c("analysis", "assessment")
      rsample::make_splits(this_fold, data = data)
    }
  )
  rsample::manual_rset(splits, ids = paste0("Fold", seq(1:length(splits))))
}
