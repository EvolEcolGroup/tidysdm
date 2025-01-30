#' Thin point dataset to have 1 observation per raster cell per time slice
#'
#' This function thins a dataset so that only one observation per cell per time
#' slice is retained. We use a raster with layers as time slices to define the
#' data cube on which thinning is enforced (see details below on how time should be
#' formatted).
#'
#' Further spatial thinning can be achieved by aggregating cells in the raster
#' before thinning, as achieved by setting `agg_fact` > 1 (aggregation works in a
#' manner equivalent to [terra::aggregate()]).
#' Note that if `data` is an `sf` object, the function will transform the coordinates
#' to the same projection as the `raster` (recommended); if `data` is a data.frame, it is up
#' to the user to ensure that the coordinates are in the correct units.
#'
#' @param data An [`sf::sf`] data frame, or a data frame with coordinate variables.
#' These can be defined in `coords`, unless they have standard names
#' (see details below).
#' @param raster A [`terra::SpatRaster`] or `stars` object that defined the grid with layers
#' corresponding to the time slices (times should be set as either POSIXlt or
#'  "years", see [terra::time()] for details), or a [`terra::SpatRasterDataset`]
#'   where the first dataset will be
#'  used (again, times for that dataset should be set as either POSIXlt or
#'  "years")
#' `terra::time()`
#' @param coords a vector of length two giving the names of the "x" and "y"
#' coordinates, as found in `data`. If left to NULL, the function will
#' try to guess the columns based on standard names `c("x", "y")`, `c("X","Y")`,
#'  `c("longitude", "latitude")`, or `c("lon", "lat")`
#' @param time_col The name of the column with time; if time is not a lubridate object,
#' use `lubridate_fun` to provide a function that can be used to convert appropriately
#' @param lubridate_fun function to convert the time column into a lubridate object
#' @param drop_na boolean on whether locations that are NA in the raster should be dropped.
#' @param agg_fact positive integer. Aggregation factor expressed as number of cells
#' in each direction (horizontally and vertically). Or two integers (horizontal
#' and vertical aggregation factor) or three integers (when also aggregating over layers).
#' Defaults to NULL, which implies no aggregation (i.e. thinning is done on the
#' grid of `raster`)
#' @returns An object of class [`sf::sf`] or [`data.frame`], the same as "data".
#' @export

thin_by_cell_time <- function(data, raster, coords = NULL, time_col = "time",
                              lubridate_fun = c, drop_na = TRUE, agg_fact = NULL) {
  # randomise the row order, so that when we get the first instance in a cell,
  # there should be no pattern
  data <- data[sample(1:nrow(data)), ]
  # create a vector of times formatted as proper dates
  time_lub <- lubridate_fun(data %>% dplyr::pull(dplyr::all_of(time_col)))
  if (!inherits(time_lub, "POSIXct")) {
    stop("time is not a date (or cannot be coerced to one)")
  }
  # if we have a SpatRasterDataset, ge the first dataset
  if (inherits(raster, "SpatRasterDataset")) {
    raster <- raster[[1]]
  }

  if(inherits(raster, "stars")) {
    d <- stars::st_dimensions(raster)
    time <- stars::st_get_dimension_values(raster, "time")
    raster <- as(raster, "SpatRaster")
    terra::time(raster, tstep = d$time$refsys) <- time
  }

  time_steps <- terra::time(raster)

  if (any(is.na(time_steps))) {
    stop("`raster` does not have a time dimension; use `terra::time()` to set it")
  }
  if (terra::timeInfo(raster)[1, 2] == "years") {
    time_steps <- lubridate::date_decimal(time_steps)
  }
  out_of_range_warning(time_lub, time_steps)
  # convert time_lub dates into indices for the SpatRasterDataset
  time_indices <-
    sapply(time_lub, function(a, b) {
      which.min(abs(a - b))
    }, time_steps)
  data_thin <- NULL
  for (i_index in unique(time_indices)) {
    # get data for this time_index, we remove coordinates as we don't need them
    data_sub <- data %>% dplyr::filter(time_indices == i_index)
    raster_sub <- raster[[i_index]]
    data_sub <- thin_by_cell(data_sub, raster_sub,
      drop_na = drop_na, agg_fact = agg_fact
    )
    data_thin <- data_thin %>% dplyr::bind_rows(data_sub)
  }
  return(data_thin)
}
