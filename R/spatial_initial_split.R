#' Simple Training/Test Set Splitting for spatial data
#'
#' `spatial_initial_split` creates a single binary split of the data into a training
#'  set and testing set. All strategies from the package [spatialsample] are available;
#'  a random split from that strategy will be used to generate the initial split.
#' @param data A dataset (data.frame or tibble)
#' @param prop The proportion of data to be retained for modelling/analysis.
#' @param strategy A sampling strategy from [spatialsample]
#' @param ... parameters to be passed to the `strategy`
#' @returns An `rsplit` object that can be used with the [rsample::training] and [rsample::testing]
#'  functions to extract the data in each split.
#' @examples
#' set.seed(123)
#' block_initial <- spatial_initial_split(boston_canopy, prop = 1 / 5, spatial_block_cv)
#' testing(block_initial)
#' training(block_initial)
#' @export


spatial_initial_split <- function(data, prop, strategy, ...) {
  # load spatialsample if we need it
  if (!isNamespaceLoaded("spatialsample")) {
    attachNamespace("spatialsample")
  }

  # check that strategy is a spatialsample function
  if (!exists(deparse(substitute(strategy)),
    where = "package:spatialsample",
    mode = "function"
  )) {
    stop(deparse(substitute(strategy)), " is not a function in spatialsample")
  }

  if (!is.numeric(prop) | prop >= 1 | prop <= 0) {
    rlang::abort("`prop` must be a number on (0, 1).")
  } else {
    v <- round(1 / prop, digits = 0)
  }

  # use the requested strategy to create a spatial rset
  initial_rset <- strategy(data, v = v, ...)
  ## create an initial split from the one of the splits in the spatial set
  rsplit_initial <- rsample::get_rsplit(initial_rset, sample(nrow(initial_rset), 1))
  ## assign it the correct classes
  class(rsplit_initial) <- c("spatial_initial_split", "initial_split", class(rsplit_initial))
  return(rsplit_initial)
}
