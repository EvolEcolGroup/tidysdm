#' Simple Training/Test Set Splitting for spatial data
#'
#' `spatial_initial_split` creates a single binary split of the data into a
#' training set and testing set. All strategies from the package `spatialsample`
#' are available; a random split from that strategy will be used to generate the
#' initial split.
#' @param data A dataset (data.frame or tibble)
#' @param prop The proportion of data to be retained for modelling/analysis.
#'   This parameter is used to define the appropriate number of partitions for
#'   the selected `strategy`. For example, if `prop = 0.2`, then 5 partitions
#'   will be created and one of these will be used as the testing set. Set to
#'   NULL for leave-one-out crossvalidation.
#' @param strategy A sampling strategy from `spatialsample`, given either as a
#'   function (e.g. `spatial_block_cv` or `spatialsample::spatial_block_cv`) or
#'   as the name of that function (e.g. `"spatial_block_cv"`). A name is
#'   convenient when the strategy is chosen programmatically, for example from
#'   a configuration file.
#' @param ... parameters to be passed to the `strategy` function (with the
#' exception of `v` which is calculated internally based on `prop`)
#' @returns An `rsplit` object that can be used with the [rsample::training] and
#'   [rsample::testing] functions to extract the data in each split.
#' @examples
#' set.seed(123)
#' block_initial <- spatial_initial_split(boston_canopy,
#'   prop = 1 / 5, strategy = spatial_block_cv
#' )
#' testing(block_initial)
#' training(block_initial)
#'
#' # the strategy can also be given by name, e.g. from a configuration file
#' cfg <- list(prop = 1 / 5, strategy = "spatial_block_cv")
#' block_initial <- spatial_initial_split(boston_canopy,
#'   prop = cfg$prop, strategy = cfg$strategy
#' )
#' @export


spatial_initial_split <- function(data, prop, strategy, ...) {
  # the strategies are the functions exported by spatialsample that create a
  # spatial resampling set (their names start with "spatial_")
  strategy_names <- sort(grep("^spatial_",
    getNamespaceExports("spatialsample"),
    value = TRUE
  ))
  strategy_funs <- Filter(
    is.function,
    mget(strategy_names, envir = asNamespace("spatialsample"))
  )
  if (is.character(strategy)) {
    # a strategy given by name: match it and get the function
    strategy <- rlang::arg_match(strategy, names(strategy_funs))
    strategy <- strategy_funs[[strategy]]
  } else if (!any(vapply(strategy_funs, identical, logical(1), y = strategy))) {
    # a function must be one of the strategies itself, however it was given
    # (bare name, with its namespace, or stored in a variable)
    rlang::abort(paste0(
      "`strategy` must be a spatialsample strategy (a function or its ",
      "name), one of: ", paste(names(strategy_funs), collapse = ", ")
    ))
  }

  if (is.null(prop)) {
    v <- NULL
  } else if (!is.numeric(prop) || prop >= 1 || prop <= 0) {
    rlang::abort("`prop` must be a number between 0 and 1")
  } else {
    v <- round(1 / prop, digits = 0)
  }

  # use the requested strategy to create a spatial rset
  initial_rset <- strategy(data, v = v, ...)
  ## create an initial split from the one of the splits in the spatial set
  rsplit_initial <- rsample::get_rsplit(
    initial_rset,
    sample(nrow(initial_rset), 1)
  )
  ## assign it the correct classes
  class(rsplit_initial) <- c(
    "spatial_initial_split",
    "initial_split", class(rsplit_initial)
  )
  return(rsplit_initial)
}
