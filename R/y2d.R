#' Convert a time interval from years to days
#'
#' This function takes takes a time interval in years and converts into days,
#' the unit commonly used in time operations in `R`. The simple conversion
#' x * 365 does not work for large number of years, due to the presence of
#' leap years.
#' @param x the number of years of the interval
#' @returns a `difftime` object (in days)
#' @examples
#' y2d(1)
#' y2d(1000)
#'
#' @export

y2d <- function(x) {
  lubridate::date_decimal(x + 1950) - lubridate::date_decimal(1950)
}
