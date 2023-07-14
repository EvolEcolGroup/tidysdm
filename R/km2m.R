#' Convert a geographic distance from km to m
#'
#' This function takes distance in km and converts it into meters, the
#' units generally used by geographic operations in `R`. This is a trivial
#' conversion, but this functions ensures that no zeroes are lost along the way!
#' @param x the number of km
#' @returns the number of meters
#' @examples
#' km2m(10000)
#' km2m(1)
#'
#' @export

km2m <- function(x){
  x * 1000
}
