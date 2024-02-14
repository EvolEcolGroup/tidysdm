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
