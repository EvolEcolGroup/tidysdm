#' Warn if some times are outside the range of time steps from a raster
#'
#' This function helps making sure that, when we assign times to time_step
#' layers of a raster, we do not have values which are badly out of range
#' @param times the times of the locations
#' @param time_steps the time steps from the raster
#' @returns NULL
#' @keywords internal

out_of_range_warning <- function(times, time_steps){
  time_steps_ordered<-sort(time_steps)
  range_minmax <- c(utils::head(time_steps_ordered,n=1)[1]-
                      (abs(utils::head(time_steps_ordered,n=2)[1]-utils::head(time_steps_ordered,n=2)[2])/2),
                    utils::tail(time_steps_ordered,n=1)[1] + 
                      (abs(utils::tail(time_steps_ordered,n=2)[1]-utils::tail(time_steps_ordered,n=2)[2])/2))
  if(any(times<range_minmax[1]) | any(times>range_minmax[2])) {
    warning("Some dates are out of the range of the available time series.\n",
            "They will be assigned to the most extreme time point available, but this\n",
            "might not make sense. The potentially problematic dates are:\n",
            times[which((times<range_minmax[1]) | (times>range_minmax[2]))])
  } 
}
