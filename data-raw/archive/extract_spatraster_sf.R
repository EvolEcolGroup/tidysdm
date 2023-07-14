#' Extract values of sf points from a SpatRaster
#'
#' Additional method for [terra::extract()] to use an sf point object instead of
#' a SpatVector when extracting. It simply casts the sf into a SpatVector in
#' the background.
#' @param x the SpatRaster
#' @param y the sf point object
#' @returns an sf object with the values from the SpatRaster
#' @export
#' @importMethodsFrom terra extract

setMethod("extract", signature(x="SpatRaster", y="sf"),
      function(x,y,...){
        # terra::extract(x,terra::vect(y),...)
        rlang::inform("you are here")
      })
