#' Thin point dataset to have 1 observation per raster cell
#'
#' This function thins a dataset so that only one observation per cell
#' is retained.
#'
#' Further thinning can be achieved by aggregating cells in the raster
#' before thinning, as achieved by setting `agg_fact` > 1 (aggregation works in a
#' manner equivalent to [terra::aggregate()]).
#'
#' @param data An [`sf::sf`] data frame, or a data frame with coordinate variables.
#' These can be defined in `coords`, unless they have standard names
#' (see details below).
#' @param raster A [`terra::SpatRaster`] or `stars` object that defined the grid
#' @param coords a vector of length two giving the names of the "x" and "y"
#' coordinates, as found in `data`. If left to NULL, the function will
#' try to guess the columns based on standard names `c("x", "y")`, `c("X","Y")`,
#'  `c("longitude", "latitude")`, or `c("lon", "lat")`
#' @param drop_na boolean on whether locations that are NA in the raster should be dropped.
#' @param agg_fact positive integer. Aggregation factor expressed as number of cells
#' in each direction (horizontally and vertically). Or two integers (horizontal
#' and vertical aggregation factor) or three integers (when also aggregating over layers).
#' Defaults to NULL, which implies no aggregation (i.e. thinning is done on the
#' grid of `raster`)
#' @returns An object of class [`sf::sf`] or [`data.frame`], the same as "data".
#' @export

thin_by_cell <- function(data, raster, coords = NULL, drop_na = TRUE, agg_fact = NULL) {
  if (inherits(raster, "SpatRasterDataset")) {
    stop(
      "This function works on a SpatRaster for a given time point.\n",
      "For time series, which are represented by a SpatRasterDataset,\n",
      "use `thin_by_cell_time()`"
    )
  }
  if (inherits(raster, "stars")) raster <- as(raster, "SpatRaster")
  # add type checks for these parameters
  return_sf <- FALSE # flag whether we need to return an sf object
  if (inherits(data, "sf")) {
    if (all(c("X", "Y") %in% names(data))) {
      if (!all(data[, c("X", "Y")] %>% sf::st_drop_geometry() %>% as.matrix() == sf::st_coordinates(data)) |
        any(is.na(data[, c("X", "Y")]))) {
        data <- data %>%
          dplyr::rename("X_original" = "X", "Y_original" = "Y") %>%
          dplyr::bind_cols(sf::st_coordinates(data))
        warning("sf object contained 'X' and 'Y' coordinates that did not match the sf point geometry.
                These have been moved to columns 'X_original' and 'Y_original' and new X and Y columns
                have been added that match the sf point geometry.")
      }
    } else {
      data <- data %>% dplyr::bind_cols(sf::st_coordinates(data))
    }
    return_sf <- TRUE
  }
  coords <- check_coords_names(data, coords)
  # randomise the row order, so that when we get the first instance in a cell,
  # there should be no pattern
  data <- data[sample(1:nrow(data)), ]
  cell_ids <- terra::cellFromXY(
    raster,
    as.matrix(as.data.frame(data)[, coords])
  )
  cells_to_keep <- !duplicated(cell_ids)
  if (drop_na) {
    cells_to_keep <- ((cells_to_keep) & (cell_ids %in% terra::cells(raster)))
  }
  data_thin <- data[cells_to_keep, ]
  if (!is.null(agg_fact)) {
    raster_agg <- terra::aggregate(terra::rast(raster[[1]]), fact = agg_fact)
    data_thin <- thin_by_cell(data_thin, raster_agg, coords = coords, drop_na = FALSE)
  }
  # remove X and Y that were added to the sf object
  if (return_sf) {
    data_thin <- data_thin %>% dplyr::select(-dplyr::any_of(coords))
  }
  return(data_thin)
}
