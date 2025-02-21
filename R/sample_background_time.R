#' Sample background points for SDM analysis for points with a time point.
#'
#' This function samples background points from a raster given a set of
#' presences. The locations returned as the center points of the sampled cells,
#' which can overlap with the presences (in contrast to pseudo-absences, see
#' [sample_pseudoabs_time]). The following methods are implemented:
#' * 'random': background points randomly sampled from the region covered by the
#' raster (i.e. not NAs).
#' * 'dist_max': background points randomly sampled from the unioned buffers
#' of 'dist_max' from presences (distances in 'm' for lonlat rasters, and in map
#' units for projected rasters). Using the union of buffers means that areas
#' that are in multiple buffers are not oversampled. This is also referred to as
#' "thickening".
#' * 'bias': background points are sampled according to a surface representing
#' the biased sampling effort. Note that the surface for each time step is
#' normalised to sum to 1;use `n_per_time_step` to affect sampling effort within
#' each time step.
#'
#' @details Note that the time axis of the raster should be in `POSIXct` or
#'   `Date` format, or use `tstep="years"'. See [terra::time()] for details on
#'   how to set the time axis.
#' @param data An [`sf::sf`] data frame, or a data frame with coordinate
#'   variables. These can be defined in `coords`, unless they have standard
#'   names (see details below).
#' @param raster the [terra::SpatRaster], `stars` or [terra::SpatRasterDataset]
#'   from which cells will be sampled. If a [terra::SpatRasterDataset], the
#'   first dataset will be used to define which cells are valid, and which are
#'   NAs.
#' @param n_per_time_step number of background points to sample for each time
#'   step (i.e. a vector of length equal to the number of time steps in raster)
#' @param coords a vector of length two giving the names of the "x" and "y"
#'   coordinates, as found in `data`. If left to NULL, the function will try to
#'   guess the columns based on standard names `c("x", "y")`, `c("X","Y")`,
#'   `c("longitude", "latitude")`, or `c("lon", "lat")`
#' @param time_col The name of the column with time; if time is not a lubridate
#'   object, use `lubridate_fun` to provide a function that can be used to
#'   convert appropriately
#' @param lubridate_fun function to convert the time column into a lubridate
#'   object
#' @param method sampling method. One of 'random', 'dist_max', or 'bias'.
#' @param class_label the label given to the sampled points. Defaults to
#'   `background`
#' @param return_pres return presences together with background in a single
#'   tibble
#' @param time_buffer the buffer on the time axis around presences that defines
#'   their effect when sampling background with method 'max_dist'. If set to
#'   zero, presences have an effect only on the time step to which they are
#'   assigned in `raster`; if a positive value, it defines the number of days
#'   before and after the date provided in the `time` column for which the
#'   presence should be considered (e.g. 20 days means that a presence is
#'   considered in all time steps equivalent to plus and minus twenty days from
#'   its date).
#' @returns An object of class [tibble::tibble]. If presences are returned, the
#'   presence level is set as the reference (to match the expectations in the
#'   `yardstick` package that considers the first level to be the event)
#' @export


sample_background_time <- function(data, raster,
                                   n_per_time_step,
                                   coords = NULL,
                                   time_col = "time",
                                   lubridate_fun = c,
                                   method = "random",
                                   class_label = "background",
                                   return_pres = TRUE, time_buffer = 0) {
  if (time_buffer != 0 && method[1] != "dist_max") {
    stop("'time_buffer' should only be set with method 'dist_max'")
  }

  # create a vector of times formatted as proper dates
  time_lub <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::pull(time_col)
  time_lub <- lubridate_fun(time_lub)
  if (!inherits(time_lub, "POSIXct")) {
    stop("time is not a date (or cannot be coerced to one)")
  }

  # create max and min date of influence for a presence with time_buffer
  time_lub_min <- time_lub - lubridate::days(time_buffer)
  time_lub_max <- time_lub + lubridate::days(time_buffer)

  # if it is a SpatRasterDataset, use the first dataset
  if (inherits(raster, "SpatRasterDataset")) {
    raster <- raster[[1]]
  }

  if (inherits(raster, "stars")) raster <- as(raster, "SpatRaster")

  if (length(n_per_time_step) != terra::nlyr(raster)) {
    stop(
      "length of 'n_per_time_step' should be the same as the number",
      "of layers in 'raster'"
    )
  }

  # get the time steps
  time_steps <- terra::time(raster)
  if (terra::timeInfo(raster)[1, 2] == "years") {
    time_steps <- lubridate::date_decimal(time_steps)
  }
  if (inherits(time_steps, "Date")) {
    time_steps <- as.POSIXct(time_steps)
  }
  # check that time_steps is POSIXct
  if (!inherits(time_steps, "POSIXct")) {
    stop(
      "the units of the time axis of the raster are not defined;\n",
      "when using terra::time() use either POSIXct or Dates, ",
      "or set tstep to 'years'"
    )
  }
  out_of_range_warning(time_lub, time_steps)

  # convert time_lub dates into indices for the SpatRaster
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


  background <- NULL

  # we cycle over very time step in the raster
  for (i_index in seq_len(terra::nlyr(raster))) {
    # if we don't need to sample this layer, skip it
    if (n_per_time_step[i_index] == 0) {
      next
    }
    # create a dataset with presences for this time step plus presences within
    # the time buffer this is really only needed for dist_max; for other
    # methods, min and max time indeces are the same thing
    data_sub <- data %>% dplyr::filter(
      i_index >= time_indices_min & i_index <= time_indices_max
    )

    # if we need to sample it but we have nopresences
    if (nrow(data_sub) == 0) {
      if (method[1] == "dist_max") {
        stop(
          "for time ", time_steps[i_index], " there no presences when ",
          n_per_time_step[i_index],
          " background points were requested; it is not possible to sample ",
          "from a 'dist_max' buffer without presences!"
        )
      } else {
        # create a fake set of presences to feed the sample_background function
        data_sub <- data[1:2, ]
      }
    }

    # slice the region series based on the index;
    raster_sub <- terra::subset(raster, i_index)

    data_sub <- sample_background(
      data = data_sub,
      raster = raster_sub,
      n = n_per_time_step[i_index],
      coords = coords,
      method = method,
      class_label = class_label,
      return_pres = FALSE
    )
    # we need to reattach time
    data_sub <- data_sub %>% dplyr::mutate(time_step = time_steps[i_index])
    background <- background %>% dplyr::bind_rows(data_sub)
  }
  if (return_pres) {
    if (inherits(data, "sf")) {
      presences <- sf::st_set_geometry(
        data.frame(geometry = data$geometry),
        data$geometry
      )
    } else {
      presences <- data[, names(background[1:2])]
    }
    presences <- presences %>% dplyr::mutate(
      class = "presence",
      time_step = time_lub
    )
    background <- background %>%
      dplyr::bind_rows(presences) %>%
      dplyr::mutate(class = stats::relevel(factor(class), ref = "presence"))
  }
  return(background)
}
