#' Make predictions for a whole raster
#'
#' This function allows to use a raster as data to make predictions from a
#' variety of `tidymodels` objects, such as [`simple_ensemble`] or
#' [`stacks::stacks`]
#' @param object the `tidymodels` object of interest
#' @param raster the [`terra::SpatRaster`] or `stars` with the input data. It
#'   has to include levels with the same names as the variables used in `object`
#' @param filename the name of the output file raster file; this is only needed
#'   to save large rasters, and can be left blank for rasters that can be kept
#'   in memory.
#' @param n positive integer indicating how many copies the data may be in
#'   memory at any point in time (it defaults to 4). This is used to determine
#'   whether rasters can be processed in one go, or in chunks. If you get an out
#'   of memory error, increase `n`. See [terra::writeStart()] for more details.
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
predict_raster.default <- function(object, raster, filename = "", n = 4,
                                   ...) {
  if (inherits(raster, "stars")) {
    is_stars <- TRUE
    raster <- as(raster, "SpatRaster")
  } else {
    is_stars <- FALSE
  }

  # we need to figure out how many layers we will need in the output raster
  # we predict with 5 random points
  rast_sub_values <- terra::spatSample(raster, 
    size = 5,
    na.rm = TRUE,
    replace = FALSE,
    method = "random"
  )

  if (nrow(rast_sub_values) == 0) {
    stop("We could not find any non-NA value in the raster")
  }
  
  # make predictions
  pred <- stats::predict(object, rast_sub_values, ...)
  n_layers_out <- ncol(pred)
  layer_names <- names(pred)
  rm(pred)


  # start reading the raster
  terra::readStart(raster)
  on.exit(terra::readStop(raster))

  # now create an output raster with the correct number of layers
  pred_raster <- terra::rast(raster, nlyr = n_layers_out)
  pred_raster_out <- terra::writeStart(pred_raster, filename = filename, n = n)


  for (i in 1:pred_raster_out$n) {
    # read the values
    rast_sub_values <- terra::readValues(raster,
      pred_raster_out$row[i],
      pred_raster_out$nrows[i],
      dataframe = TRUE
    )
    tot_df_rows <- nrow(rast_sub_values)
    # add row numbers to the data frame
    rast_sub_values <- rast_sub_values %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      dplyr::filter(stats::complete.cases(rast_sub_values))
    # remove lines with any NA
    # this is important, as the predict function will not work with NA values
    # and we need to remove them before passing the data to the predict function

    # create a data.frame with predictions that has the same number of rows
    # as the original data frame
    pred_all <- data.frame(matrix(NA_real_,
      nrow = tot_df_rows,
      ncol = n_layers_out
    ))
    names(pred_all) <- layer_names

    # make predictions (only if we have some values to predict)
    if (nrow(rast_sub_values) > 0) {
      pred <- stats::predict(object, rast_sub_values, ...)
      pred_all[rast_sub_values$row, ] <- pred
    }


    # write the values
    terra::writeValues(
      pred_raster, as.matrix(pred_all),
      pred_raster_out$row[i],
      pred_raster_out$nrows[i]
    )
  }

  for (i in seq_len(ncol(pred))) {
    # if a given prediction is a factor, we need to convert teh relevant layer
    if (is.factor(pred %>% dplyr::pull(i))) {
      levels_in_factor <- levels(pred %>% dplyr::pull(i))

      levels(pred_raster[[i]]) <-
        data.frame(
          id = seq_along(levels_in_factor),
          class = levels_in_factor
        )
    }
  }

  # update names of the prediction raster
  names(pred_raster) <- layer_names
  # update if it is a factor
  if (is.factor(pred %>% dplyr::pull(1))) {
    names(pred_raster) <- paste0("binary_", names(pred_raster))
  }

  # set the time to match the raster of origin
  terra::time(pred_raster, tstep = terra::timeInfo(raster)$step) <-
    rep(terra::time(raster)[1], terra::nlyr(pred_raster))

  terra::writeStop(pred_raster)

  if (is_stars) {
    pred_raster <- stars::st_as_stars(pred_raster, as_attributes = TRUE)
  }
  pred_raster
}
