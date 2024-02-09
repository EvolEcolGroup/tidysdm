#' Clamp the predictors to match values in training set
#'
#' This function clamps the environmental variables in a [`terra::SpatRaster`]
#' or [`terra::SpatRasterDataset`] so that their minimum and maximum values
#' do not exceed the range in the training dataset. 
#'
#' @param x a [`terra::SpatRaster`] or [`terra::SpatRasterDataset`] to clamp.
#' @param training the training dataset (a [`data.frame`] or a [`sf::sf`] object.
#' @param use_na a boolean determining whether values outside the range
#' of the training dataset are removed (set to NA). If FALSE (the default),
#' values outside the training range are replaced with the extremes of the training
#' range.
#' @returns a [`terra::SpatRaster`] or [`terra::SpatRasterDataset`] clamped to
#' the ranges in `training`
#' @keywords export


clamp_predictors <- function(x, training, use_na) {
  UseMethod("clamp_predictors", object = x)
}

#' @rdname clamp_predictors
#' @export
clamp_predictors.default <- function(x, training, use_na) {
  stop("no method available for this object type")
}

#' @rdname clamp_predictors
#' @export
clamp_predictors.SpatRaster <- function(x, training, use_na = FALSE) {
  # check that all variables are present in the raster
  if (!all(names(training) %in% names(x))){
    stop("`x` is missing the following variables (which are present in `training`): ",
         paste(names(training)[!names(training) %in% names(x)]))
  }
  # subset to the variables in training
  extremes <- apply(training, 2, range, na.rm=TRUE)
  x <- terra::clamp(x, lower = extremes[1,], upper = extremes[2,], values = !use_na)
  return(x)
}

#' @rdname clamp_predictors
#' @export
clamp_predictors.SpatRasterDataset <- function(x, training, use_na = FALSE) {
  # check that all variables are present in the raster
  if (!all(names(training) %in% names(x))){
    stop("`x` is missing the following variables (which are present in `training`): ",
         paste(names(training)[!names(training) %in% names(x)]))
  }
  # subset to the variables in training
  extremes <- apply(training, 2, range, na.rm=TRUE)
  for (i_name in names(x)){
   x[[i_name]]<-terra::clamp(x[[i_name]], lower = extremes[1,i_name], upper = extremes[2,i_name], values = !use_na)
  }
  return(x)
}
