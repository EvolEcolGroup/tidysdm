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
#' @param n the number of cells in the grid, defaults to c(10,10), which is also
#' the default for [sf::st_make_grid()]
#' @export

grid_cellsize<-function(
  data,
  n=c(10,10)
){
  # TODO check that this is an sf object
  grid_box <- sf::st_bbox(data)
  if (utils::getFromNamespace("is_longlat", "spatialsample")(data)) {
    grid_box <- utils::getFromNamespace("expand_grid", "spatialsample")(grid_box)
  }
  c(diff(sf::st_bbox(grid_box)[c(1, 3)]), diff(sf::st_bbox(grid_box)[c(2, 4)]))/n
}

