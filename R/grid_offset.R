#' Get default grid cellsize for a given dataset
#'
#' This function facilitates using [spatialsample::spatial_block_cv] multiple
#' times in an analysis. [spatialsample::spatial_block_cv] creates a grid
#' based on the object in `data`. However, if spatial blocks are generated
#' multiple times in an analysis (e.g. for a [spatial_initial_split()], and then
#' subsequently for cross-validation on the training dataset), it might be desirable to keep the
#' same grid). By applying this function to the largest dataset, usually the
#' full dataset before [spatial_initial_split()]. The resulting cellsize can
#' be used as an option in [spatialsample::spatial_block_cv].
#' @param data a [sf::sf] dataset used to size the grid
#' @returns the grid offset
#' @export

grid_offset <- function(
    data) {
  # TODO check that this is an sf object
  grid_box <- sf::st_bbox(data)
  if (is_longlat(data)) {
    grid_box <- expand_grid(grid_box)
  }
  sf::st_bbox(grid_box)[c("xmin", "ymin")]
}
