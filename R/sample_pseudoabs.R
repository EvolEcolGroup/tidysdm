#' Sample pseudo-absence points for SDM analysis
#'
#' This function samples pseudo-absence points from a raster given a set of
#' presences. The locations returned as the center points of the sampled cells,
#' which can not overlap with the presences (in contrast to background points,
#' see [sample_background]). The following methods are implemented:
#' * 'random': pseudo-absences randomly sampled from the region covered by the
#' raster (i.e. not NAs).
#' * 'dist_min': pseudo-absences randomly sampled from the region excluding
#'  a buffer
#' of 'dist_min' from presences (distances in 'm' for lonlat rasters, and in map
#' units for projected rasters).
#' * 'dist_max': pseudo-absences randomly sampled from the unioned buffers
#' of 'dist_max' from presences (distances in 'm' for lonlat rasters, and in map
#' units for projected rasters). Using the union of buffers means that areas
#' that are in multiple buffers are not oversampled. This is also referred to as
#' "thickening".
#' * 'dist_disc': pseudo-absences randomly sampled from the unioned discs
#'  around presences
#' with the two values of 'dist_disc' defining the minimum and maximum distance
#' from presences.
#' @param data An [`sf::sf`] data frame, or a data frame with coordinate
#'   variables. These can be defined in `coords`, unless they have standard
#'   names (see details below).
#' @param raster the [terra::SpatRaster] or `stars` from which cells will be
#'   sampled
#' @param n number of pseudoabsence points to sample
#' @param coords a vector of length two giving the names of the "x" and "y"
#'   coordinates, as found in `data`. If left to NULL, the function will try to
#'   guess the columns based on standard names `c("x", "y")`, `c("X","Y")`,
#'   `c("longitude", "latitude")`, or `c("lon", "lat")`
#' @param method sampling method. One of 'random', 'dist_min', 'dist_max', or
#'   'dist_disc'. Threshold distances are set as additional elements of a
#'   vector, e.g c('dist_min',70000) or c('dist_disc',50000,200000).
#' @param class_label the label given to the sampled points. Defaults to
#'   `pseudoabs`
#' @param return_pres return presences together with pseudoabsences in a single
#'   tibble
#' @returns An object of class [tibble::tibble]. If presences are returned, the
#'   presence level is set as the reference (to match the expectations in the
#'   `yardstick` package that considers the first level to be the event)
#' @export


sample_pseudoabs <- function(data, raster, n, coords = NULL,
                             method = "random", class_label = "pseudoabs",
                             return_pres = TRUE) {
  if (inherits(raster, "stars")) raster <- as(raster, "SpatRaster")
  return_sf <- FALSE # flag whether we need to return an sf object
  if (inherits(data, "sf")) {
    bind_col <- TRUE
    if (all(c("X", "Y") %in% names(data))) {
      if (any(is.na(data[, c("X", "Y")]))) {
        stop("sf object contains NA values in the X and Y coordinates")
      } else if (all(sf::st_drop_geometry(data[, c("X", "Y")]) ==
        sf::st_coordinates(data))) {
        bind_col <- FALSE
      } else {
        stop(
          "sf object contains X and Y coordinates that do not match the ",
          "sf point geometry"
        )
      }
    }
    if (bind_col) {
      data <- data %>% dplyr::bind_cols(sf::st_coordinates(data))
    }
    crs_from_sf <- sf::st_crs(data)
    return_sf <- TRUE
  }
  coords <- check_coords_names(data, coords)
  dist_min <- dist_max <- NULL
  if (method[1] == "dist_disc") {
    if (length(method) != 3) {
      stop(
        "method 'dist_disc' should have two thresholds, ",
        "e.g. c('dist_disc',10,20)"
      )
    }
    dist_min <- as.numeric(method[2])
    dist_max <- as.numeric(method[3])
  } else if (method[1] == "dist_min") {
    if (length(method) != 2) {
      stop(
        "method 'dist_min' should have one threshold, ",
        "e.g. c('dist_min',10)"
      )
    }
    dist_min <- as.numeric(method[2])
  } else if (method[1] == "dist_max") {
    if (length(method) != 2) {
      stop(
        "method 'dist_max' should have one threshold, ",
        "e.g. c('dist_max',50)"
      )
    }
    dist_max <- as.numeric(method[2])
  } else if (!method[1] %in% "random") {
    stop(
      "method has to be one of 'random', 'dist_min', ",
      "'dist_max', or 'dist_disc'"
    )
  }
  xy_pres <- as.matrix(as.data.frame(data)[, coords])
  # get a one layer raster
  sampling_raster <- raster[[1]]
  names(sampling_raster) <- "class"
  # turn presences into additional NAs
  sampling_raster[stats::na.omit(
    terra::cellFromXY(sampling_raster, xy_pres)
  )] <- NA

  # remove buffer < dist_min (or first parameter for disc)
  if (!is.null(dist_min)) {
    min_buffer <- terra::buffer(
      terra::vect(xy_pres,
        crs = terra::crs(sampling_raster)
      ),
      dist_min
    )
    sampling_raster <- terra::mask(sampling_raster,
      min_buffer,
      inverse = TRUE,
      touches = FALSE
    )
  }
  # remove buffer >dist_max (or second parameter for disc)
  if (!is.null(dist_max)) {
    max_buffer <- terra::buffer(
      terra::vect(xy_pres,
        crs = terra::crs(sampling_raster)
      ),
      dist_max
    )
    sampling_raster <- terra::mask(sampling_raster, max_buffer, touches = FALSE)
  }
  # now sample points
  # cell ids excluding NAs
  cell_id <- terra::cells(sampling_raster)
  if (length(cell_id) > n) {
    cell_id <- sample(x = cell_id, size = n)
  } else {
    warning("There are fewer available cells for raster '", terra::time(sampling_raster), "' (", nrow(xy_pres), " presences) than the requested ", n, " pseudoabsences. Only ", length(cell_id), " will be returned.\n")
  }
  pseudoabsences <- as.data.frame(terra::xyFromCell(sampling_raster, cell_id))
  # fix the coordinate names to be the same we started with
  names(pseudoabsences) <- coords
  pseudoabsences <- pseudoabsences %>% dplyr::mutate(class = class_label)
  if (return_pres) {
    presences <- dplyr::as_tibble(xy_pres) %>%
      dplyr::mutate(class = "presence")
    pseudoabsences <- presences %>%
      dplyr::bind_rows(pseudoabsences) %>%
      dplyr::mutate(class = stats::relevel(factor(class), ref = "presence"))
  }
  # remove X and Y that were added to the sf object
  if (return_sf) {
    pseudoabsences <- sf::st_as_sf(pseudoabsences, coords = coords) %>%
      sf::st_set_crs(crs_from_sf)
  }
  return(pseudoabsences)
}
