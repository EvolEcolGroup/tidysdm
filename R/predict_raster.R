#' Make predictions for a whole raster
#'
#' This function allows to use a raster as data to make predictions from a
#' variety of `tidymodels` objects, such as [`simple_ensemble`] or
#' [`stacks::stacks`]
#' @param object the `tidymodels` object of interest
#' @param raster the [`terra::SpatRaster`] or `stars` with the input data. It
#'   has to include levels with the same names as the variables used in `object`
#' @param ... parameters to be passed to the standard `predict()` function for
#'   the appropriate object type (e.g. `metric_thresh` or `class_thresh`).
#' @returns a [`terra::SpatRaster`] (or `stars` if that is the input) with the
#'   predictions
#' @export
#' @keywords predict
#'
predict_raster <- function(object, raster, ...) {
  UseMethod("predict_raster", object)
}


#' @rdname predict_raster
#' @export
predict_raster.default <- function(object, raster, ...) {
  if (inherits(raster, "stars")) {
    is_stars <- TRUE
    raster <- as(raster, "SpatRaster")
  } else {
    is_stars <- FALSE
  }
  # create a dataframe
  raster_df <- terra::as.data.frame(raster, cell = TRUE, na.rm = TRUE)
  # create a vector of predictions by dispatching to the predict generics
  pred_df <- stats::predict(object, raster_df, ...)

  # create an empty raster where to put the predictions (using the original
  # raster as a template)
  pred_raster <- terra::rast(raster[[1]])

  # and now fill in the values, adding a layer for each aggregating function we
  # used
  pred_raster[raster_df$cell] <- pred_df %>% dplyr::pull(1)
  if (is.factor(pred_df %>% dplyr::pull(1))) {
    # nolint start
    # old code, kept for reference
    # levels(pred_raster) <-
    #          data.frame(id = 1:2, class = levels(pred_df %>% dplyr::pull(1)))
    # nolint end
    # make predict_raster work with multilevel predictions
    # edit by @piabenaud
    levels(pred_raster) <- data.frame(
      id = seq_along(dplyr::pull(unique(pred_df))),
      class = levels(pred_df %>% dplyr::pull(1))
    )
  }


  if (ncol(pred_df) > 1) {
    for (i_col in 2:ncol(pred_df)) {
      pred_raster2 <- terra::rast(raster[[1]])
      pred_raster2[raster_df$cell] <- pred_df %>% dplyr::pull(i_col)
      terra::add(pred_raster) <- pred_raster2
    }
  }
  names(pred_raster) <- names(pred_df)
  if (is.factor(pred_df %>% dplyr::pull(1))) {
    names(pred_raster) <- paste0("binary_", names(pred_raster))
  }
  if (is_stars) {
    pred_raster <- stars::st_as_stars(pred_raster, as_attributes = TRUE)
  }
  pred_raster
}
