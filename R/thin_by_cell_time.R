#' Thin point dataset to have 1 observation per raster cell per time slice
#'
#' This function thins a dataset so that only one observation per cell per time
#' slice is retained.
#'
#' Further thinning can be achieved by aggregating cells in the raster
#' before thinning, as achieved by setting `agg_fact` > 1 (aggregation works in a
#' manner equivalent to [terra::aggregate()]).
#'
#' @param data An [`sf::sf`] data frame, or a data frame with coordinate variables.
#' These can be defined in `coords`, unless they have standard names
#' (see details below).
#' @param raster A [`terra::SpatRaster`] object that defined the grid
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
#' @return An object of class [`sf::sf`] or [`data.frame`], the same as "data".
#' @export

thin_by_cell_time <- function(data, raster, coords=NULL, time_col="time",
                         lubridate_fun=c, drop_na = TRUE, agg_fact=NULL){
  # randomise the row order, so that when we get the first instance in a cell,
  # there should be no pattern
  data <- data[sample(1:nrow(data)),]
  # create a vector of times formatted as proper dates
  time_lub <- data[,time_col] %>% as.data.frame() %>% dplyr::select(dplyr::all_of(time_col))
  time_lub <- lubridate_fun(time_lub[,time_col])
  if (!inherits(time_lub,"POSIXct")){
    stop("time is not a date (or cannot be coerced to one)")
  }
  # get the time steps from the SpatRasterDataset
  time_steps <- terra::time(raster)[[1]]
  if ( terra::timeInfo(raster)[1,2]=="years"){
    time_steps <- lubridate::date_decimal(time_steps)
  }
  # convert time_lub dates into indeces for the SpatRasterDatset
  time_indeces <-
    sapply(time_lub, function(a, b) {
      which.min(abs(a - b))
    }, time_steps)
  data_thin<-NULL
  for (i_index in unique(time_indeces)){
    # get data for this time_index, we remove coordinates as we don't need them
    data_sub <- data %>% dplyr::filter(time_indeces==i_index)
    raster_sub <- raster[[1]][[i_index]]
    data_sub <- thin_by_cell(data_sub, raster_sub,
                             drop_na = drop_na, agg_fact=agg_fact)
      data_thin <- data_thin %>% dplyr::bind_rows(data_sub)
  }
  return(data_thin)
}
