#' Make a mask from presence data
#'
#' This functions uses the presence column to create a mask to apply to the
#' raster to define the area of interest. Two methods are available: one that
#' uses a buffer around each presence, and one that create a convex hull around
#' all presences (with the possibility of further adding a buffer around the
#' hull).
#'
#' To use [terra::mask()] on a raster, use `return_sf = FALSE` to get a
#' `terra::SpatVector` object that can be used for masking.
#'
#' @param data An [`sf::sf`] data frame of presences..
#' @param method the method to use to create the mask. Either 'buffer' or
#'   'convex_hull'
#' @param buffer the buffer to add around each presence (in the units of the crs
#'   of the data; for lat/lon, the buffer will be in meters), or around the
#'   convex hull (if method is 'convex_hull')
#' @param return_sf whether to return the mask as an `sf` object (if TRUE) or as
#'   a `terra::SpatVector` object (if FALSE, default)
#' @returns a `terra::SpatVector` or an `sf` object (depending on the value of
#'   `return_sf`) with the mask
#' @export
#' @examples
#' lacerta_sf <- lacerta %>%
#'   sf::st_as_sf(coords = c("longitude", "latitude")) %>%
#'   sf::st_set_crs(4326)
#' land_mask <- terra::readRDS(system.file("extdata/lacerta_land_mask.rds",
#'   package = "tidysdm"
#' ))
#' mask_buffer <- make_mask_from_presence(lacerta_sf, method = "buffer", 
#'                                                    buffer = 60000)
#' terra::plot(terra::mask(land_mask, mask_buffer))
#' mask_ch <- make_mask_from_presence(lacerta_sf, method = "convex_hull")
#' terra::plot(terra::mask(land_mask, mask_ch))
make_mask_from_presence <- function(data,
                                    method = "buffer",
                                    buffer = 0,
                                    return_sf = FALSE) {
  # Check method
  if (!method %in% c("buffer", "convex_hull")) {
    stop('method must be either "buffer" or "convex_hull"')
  }

  # Check buffer
  if (!is.numeric(buffer) || length(buffer) != 1) {
    stop("buffer must be a single numeric value")
  }

  # Check data
  if (!inherits(data, "sf")) {
    stop("data must be an sf object")
  }

  # Create mask
  if (method == "buffer") {
    if (buffer <= 0) {
      stop("buffer must be a positive value")
    }
    mask <- sf::st_buffer(sf::st_union(data), buffer)
  } else {
    mask <- sf::st_convex_hull(sf::st_union(data))
    if (buffer > 0) {
      mask <- sf::st_buffer(mask, buffer)
    }
  }
  if (return_sf) {
    return(mask)
  } else {
    return(terra::vect(mask))
  }
}
