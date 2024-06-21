#' Make predictions for a whole raster
#'
#' This function allows to use a raster as data to make predictions from a
#' variety of [tidymodels] objects, such as [`simple_ensemble`] or `stacks::linear_stack`
#' @param object the [`tidymodels`] object of interest
#' @param raster the [`terra::SpatRaster`] with the input data. It has to include
#' levels with the same names as the variables used in `object`
#' @param metric_thresh a vector of length 2 giving a metric and its threshold,
#' which will be used to prune
#' which models in the ensemble will be used for the prediction. The metric's threshold needs to match 
#' the value used in [`calib_class_thresh`]. Examples are c("accuracy",0.8) or c("boyce_cont",0.7).
#' @param ... parameters to be passed to the standard `predict()` function
#' for the appropriate object type.
#' @returns a [`terra::SpatRaster`] with the predictions
#' @export
#'
predict_raster <- function(object, raster, ...) {
  UseMethod("predict_raster", object)
}

#' @rdname predict_raster
#' @export
predict_raster.default <- function(object, raster, metric_thresh = NULL, ...) {
  # create a dataframe
  raster_df <- terra::as.data.frame(raster, cell = TRUE, na.rm = TRUE)
  # create a vector of predictions by dispatching to the predict generics
  pred_df <- stats::predict(object, raster_df, metric_thresh = metric_thresh, ...)

  # create an empty raster where to put the predictions (using the original raster as a template)
  pred_raster <- terra::rast(raster[[1]])

  # and now fill in the values, adding a layer for each aggregating function we used
  pred_raster[raster_df$cell] <- pred_df %>% dplyr::pull(1)
  if (is.factor(pred_df %>% dplyr::pull(1))) {
    levels(pred_raster) <- data.frame(id = 1:2, class = levels(pred_df %>% dplyr::pull(1)))
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
  pred_raster
}
