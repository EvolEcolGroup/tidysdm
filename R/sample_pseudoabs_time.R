#' Sample pseudo-absence (or background) points for SDM analysis
#'
#' This function samples pseudo-absence (or background, the naming is a matter
#' of semantics) points from a raster given a set of presences.
#' The locations returned as the center points of the sampled cells, which can
#' not overlap with the presences. The following methods are implemented:
#' {/itemize:
#' /item: 'random': pseudo-absences/background randomly sampled from the region covered by the
#' raster (i.e. not NAs).
#' /item: 'dist_min': pseudo-absences/background randomly sampled from the region excluding a buffer
#' of 'dist_min' from presences (distances in 'm' for lonlat rasters, and in map
#' units for projected rasters).
#' /item: 'dist_max': pseudo-absences/background randomly sampled from the unioned buffers
#' of 'dist_max' from presences (distances in 'm' for lonlat rasters, and in map
#' units for projected rasters). Using the union of buffers means that areas that
#' are in multiple buffers are not oversampled. This is also referred to as "thickening".
#' /item: 'dist_disc': pseudo-absences/background randomly sampled from the unioned discs around presences
#' with the two values of 'dist_disc' defining the minimum and maximum distance from
#' presences.
#' }
#' @param data An [`sf::sf`] data frame, or a data frame with coordinate variables.
#' These can be defined in `coords`, unless they have standard names
#' (see details below).
#' @param raster the [terra::SpatRaster] from which cells will be sampled
#' @param n_per_presence number of pseudoabsence/background points to sample for
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
#' @param return_pres return presences together with pseudoabsences/background
#'  in a single tibble
#' @returns An object of class [tibble::tibble]. If presences are returned, the
#' presence level is set as the reference (to match the expectations in the
#' `yardstick` package that considers the first level to be the event)
#' @export


sample_pseudoabs_time <- function (data, raster, n_per_presence, coords = NULL,time_col="time",
                              lubridate_fun=c,
                              method="random", class_label = "pseudoabs",
                              return_pres=TRUE)
{
  # create a vector of times formatted as proper dates
  time_lub <- data[,time_col] %>% as.data.frame() %>% dplyr::select(dplyr::all_of(time_col))
  time_lub <- lubridate_fun(time_lub[,time_col])
  if (!inherits(time_lub,"POSIXct")){
    stop("time is not a date (or cannot be coerced to one)")
  }
  # get the time steps from the SpatRasterDataset
  time_steps <- time_steps_orig <- terra::time(raster)[[1]]
  if ( terra::timeInfo(raster)[1,2]=="years"){
    time_steps <- lubridate::date_decimal(time_steps)
  }
  # convert time_lub dates into indices for the SpatRasterDatset
  time_indices <-
    sapply(time_lub, function(a, b) {
      which.min(abs(a - b))
    }, time_steps)
  pseudoabsences<-NULL
  for (i_index in unique(time_indices)){
    #browser()
    # get data for this time_index, we remove coordinates as we don't need them
    data_sub <- data %>% dplyr::filter(time_indices==i_index)
    # slice the region series based on the index;
    raster_sub <- pastclim::slice_region_series(raster, time_bp=pastclim::time_bp(raster)[i_index])
    data_sub <- sample_pseudoabs (data= data_sub,
                                              raster = raster_sub,
                                              n = n_per_presence*nrow(data_sub),
                                              coords = coords,
                                              method=method,
                                              class_label = class_label,
                                              return_pres=return_pres)
    # we need to reattach time
    data_sub <- data_sub %>% dplyr::mutate(time_step = time_steps[i_index])
    pseudoabsences <- pseudoabsences %>% dplyr::bind_rows(data_sub)
  }
  return(pseudoabsences)
}


