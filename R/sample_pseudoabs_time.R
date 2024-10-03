#' Sample pseudo-absence points for SDM analysis for points with a time point.
#'
#' This function samples pseudo-absence points from a raster given a set of presences.
#' The locations returned as the center points of the sampled cells, which can
#' not overlap with the presences (in contrast to background points, see 
#' [sample_background_time]). The following methods are implemented:
#' * 'random': pseudo-absences randomly sampled from the region covered by the
#' raster (i.e. not NAs).
#' * 'dist_min': pseudo-absences randomly sampled from the region excluding a buffer
#' of 'dist_min' from presences (distances in 'm' for lonlat rasters, and in map
#' units for projected rasters).
#' * 'dist_max': pseudo-absences randomly sampled from the unioned buffers
#' of 'dist_max' from presences (distances in 'm' for lonlat rasters, and in map
#' units for projected rasters). Using the union of buffers means that areas that
#' are in multiple buffers are not oversampled. This is also referred to as "thickening".
#' * 'dist_disc': pseudo-absences randomly sampled from the unioned discs around presences
#' with the two values of 'dist_disc' defining the minimum and maximum distance from
#' presences.
#' @param data An [`sf::sf`] data frame, or a data frame with coordinate variables.
#' These can be defined in `coords`, unless they have standard names
#' (see details below).
#' @param raster the [terra::SpatRaster], `stars` or [terra::SpatRasterDataset] from which cells will be sampled.
#' If a [terra::SpatRasterDataset], the first dataset will be used to define which cells are valid,
#' and which are NAs.
#' @param n_per_presence number of pseudoabsence points to sample for
#' each presence
#' @param coords a vector of length two giving the names of the "x" and "y"
#' coordinates, as found in `data`. If left to NULL, the function will
#' try to guess the columns based on standard names `c("x", "y")`, `c("X","Y")`,
#'  `c("longitude", "latitude")`, or `c("lon", "lat")`
#' @param time_col The name of the column with time; if time is not a lubridate object,
#' use `lubridate_fun` to provide a function that can be used to convert appropriately
#' @param lubridate_fun function to convert the time column into a lubridate object
#' @param method sampling method. One of 'random', 'dist_min', 'dist_max', or
#' 'dist_disc'.
#' @param class_label the label given to the sampled points. Defaults to `pseudoabs`
#' @param return_pres return presences together with pseudoabsences
#'  in a single tibble
#' @param time_buffer the buffer on the time axis around presences that defines their effect when
#'  sampling pseudoabsences. If set to zero, presences have an effect only on the time step to which
#'  they are assigned in `raster`; if a positive value, it defines the number of days before
#'  and after the date provided in the `time` column for which the presence should be considered
#'  (e.g. 20 days means that a presence is considered in all time steps equivalent to plus and minus
#'  twenty days from its date).
#' @returns An object of class [tibble::tibble]. If presences are returned, the
#' presence level is set as the reference (to match the expectations in the
#' `yardstick` package that considers the first level to be the event)
#' @export


sample_pseudoabs_time <- function(data, raster, n_per_presence, coords = NULL, time_col = "time",
                                  lubridate_fun = c,
                                  method = "random", class_label = "pseudoabs",
                                  return_pres = TRUE, time_buffer = 0) {
  
  if(inherits(raster, "stars")) raster <- as(raster, "SpatRaster")
  
  # create a vector of times formatted as proper dates
  time_lub <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::pull(time_col)
  time_lub <- lubridate_fun(time_lub)
  if (!inherits(time_lub, "POSIXct")) {
    stop("time is not a date (or cannot be coerced to one)")
  }
  # create max and min date of influence for a presence with time_buffer
  time_lub_min <- time_lub-lubridate::days(time_buffer)
  time_lub_max <- time_lub+lubridate::days(time_buffer)

  # if it is a SpatRasterDataset, use the first dataset
  if (inherits(raster, "SpatRasterDataset")) {
    raster <- raster[[1]]
  } 
  
  # get the time steps
  time_steps <- terra::time(raster)
  if (terra::timeInfo(raster)[1, 2] == "years") {
    time_steps <- lubridate::date_decimal(time_steps)
  }
  out_of_range_warning(time_lub, time_steps)

  # convert time_lub dates into indices for the SpatRasterDatset
  time_indices <-
    sapply(time_lub, function(a, b) {
      which.min(abs(a - b))
    }, time_steps)
  time_indices_min <-
    sapply(time_lub_min, function(a, b) {
      which.min(abs(a - b))
    }, time_steps)
  time_indices_max <-
    sapply(time_lub_max, function(a, b) {
      which.min(abs(a - b))
    }, time_steps)
  
  
  pseudoabsences <- NULL

  for (i_index in unique(time_indices)) {
    # count the presences in this time step
    n_pres_this_time <- data %>% dplyr::filter(time_indices == i_index) %>% nrow()
    # create a dataset with presences for this time step plus presences within the time buffer
    data_sub <- data %>% dplyr::filter(
      i_index >= time_indices_min & i_index <=time_indices_max)

    # slice the region series based on the index;
    raster_sub <- terra::subset(raster, i_index)

    data_sub <- sample_pseudoabs(
      data = data_sub,
      raster = raster_sub,
      n = n_per_presence * n_pres_this_time,
      coords = coords,
      method = method,
      class_label = class_label,
      return_pres = FALSE
    )
    # we need to reattach time
    data_sub <- data_sub %>% dplyr::mutate(time_step = time_steps[i_index])
    pseudoabsences <- pseudoabsences %>% dplyr::bind_rows(data_sub)
  }
  if (return_pres){
    if (inherits(data,"sf")){
      presences <- sf::st_set_geometry(data.frame(geometry = data$geometry), 
                                       data$geometry)
    } else {
      presences <- data[,names(pseudoabsences[1:2])]
    }
    presences <- presences %>% dplyr::mutate(class="presence",time_step=time_lub) 
    pseudoabsences <- pseudoabsences %>% dplyr::bind_rows(presences) %>%
      dplyr::mutate(class = stats::relevel(factor(class), ref = "presence"))
  }
  return(pseudoabsences)
}
