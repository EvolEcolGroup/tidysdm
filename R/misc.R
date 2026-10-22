# copy of internal abort_if_class_pred from yardstick
abort_if_class_pred <- function(x, call = rlang::caller_env()) {
  if (inherits(x, "class_pred")) {
    rlang::abort(
      "`truth` should not a `class_pred` object.",
      call = call
    )
  }
  return(invisible(x))
}

# copy of internal from spatialsample
is_longlat <- function(x) {
  !(sf::st_crs(x) == sf::NA_crs_) && sf::st_is_longlat(x)
}

# copy of internal from spatialsample
expand_grid <- function(grid_box, expansion = 0.00001) {
  grid_box[1] <- grid_box[1] - abs(grid_box[1] * expansion)
  grid_box[2] <- grid_box[2] - abs(grid_box[2] * expansion)
  grid_box[3] <- grid_box[3] + abs(grid_box[3] * expansion)
  grid_box[4] <- grid_box[4] + abs(grid_box[4] * expansion)
  grid_box
}

# convert a stars object with a time dimension into a SpatRaster, preserving
# the time information. Times stored as POSIXct or Date are kept as they are;
# for other times (e.g. years), the refsys of the time dimension of the stars
# object should give the time step used by terra (e.g. "years")
stars_to_spatraster_time <- function(x) {
  x_dims <- stars::st_dimensions(x)
  raster <- as(x, "SpatRaster")
  terra_tsteps <- c("seconds", "days", "months", "years", "yearmonths")
  if ("time" %in% names(x_dims)) {
    time_steps <- stars::st_get_dimension_values(x, "time")
    time_refsys <- x_dims$time$refsys
    if (inherits(time_steps, c("POSIXct", "Date"))) {
      terra::time(raster) <- time_steps
    } else if (isTRUE(time_refsys %in% terra_tsteps)) {
      terra::time(raster, tstep = time_refsys) <- time_steps
    }
  }
  raster
}
