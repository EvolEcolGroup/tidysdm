#' Create an initial train/test split from a blockCV object
#'
#' Converts a `blockCV` object to an `rsample` object and randomly selects one fold
#' as the assessment (test) set, with the remaining folds used as the analysis
#' (training) set.
#'
#' @param x A blockCV object (currently we only support classes `cv_spatial`,
#'   `cv_cluster`, `cv_nndm` and `cv_buffer`).
#' @param data The sf object used to create `x`.
#'
#' @return An `rsplit` object that can be used with [rsample::training()] and
#'   [rsample::testing()].
#'
#' @export
#' @examplesIf rlang::is_installed("blockCV")
#' library(blockCV)
#' # import presence-absence species data
#' points <- read.csv(system.file("extdata/", "species.csv", package = "blockCV"))
#' # make an sf object from data.frame
#' pa_data <- sf::st_as_sf(points, coords = c("x", "y"), crs = 7845)
#' sb1 <- blockCV::cv_spatial(
#'   x = pa_data,
#'   column = "occ",
#'   k = 5,
#'   size = 350000
#' )
#'
#' split <- blockcv2initial_split(sb1, pa_data)
#' training(split)
#' testing(split)
blockcv2initial_split <- function(x, data) {
  # Convert blockCV object to an rsample rset
  rset_obj <- blockcv2rsample(x, data)

  # Randomly select one fold
  rsplit_initial <- rsample::get_rsplit(
    rset_obj,
    sample(nrow(rset_obj), 1)
  )

  # Assign classes so rsample methods recognize it as an initial split
  class(rsplit_initial) <- c(
    "blockcv_initial_split",
    "spatial_initial_split",
    "initial_split",
    class(rsplit_initial)
  )

  rsplit_initial
}
