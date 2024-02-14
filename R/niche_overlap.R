#' Compute overlap metrics of the niche
#'
#' This function computes overlap metrics between two rasters. It currently
#' implements Schoener's D and the inverse I of Hellinger's distance. 
#' 
#' Note that Hellinger's distance is
#'  normalised by dividing by square root of 2 (which is the correct
#' asymptote for Hellinger's D), rather than the incorrect 2 used originally in 
#' Warren et al (2008).
#' 
#' @param x a [terra::SpatRaster]
#' @param y a [terra::SpatRaster]
#' @param method a string (or vector of strings) taking values "Schoener" and
#' "Hellinger"
#' @returns a list of overlap metrics, with slots *D* and *I* (depending on `method`)
#' @export
#' 
niche_overlap <- function (x, y, method = c("Schoener", "Hellinger")) {
  # standardise the two distributions
  # divide by sum so that integral of pdf is one
  x <- x / sum(x, na.rm = TRUE)
  y <- y / sum(y, na.rm = TRUE)
  
  res_list <- list()
  # Schoeners D
  if ("Schoener" %in% method) {
    res_list$D <- 1 - 0.5 * sum(abs(x - y), na.rm = TRUE)
  }
  
  # Hellinger's Distance
  if ("Hellinger" %in% method) {
    H <- sqrt(sum((sqrt(x) - sqrt(y)) ^ 2, na.rm = TRUE))
    # scaling in https://onlinelibrary.wiley.com/doi/10.1111/j.1558-5646.2008.00482.x
    # is incorrect, as it uses 2 rather than sqrt(2) as the max value of H
    # scaling to 1 following https://www.sciencedirect.com/science/article/pii/S2287884X18300153
    res_list$I <- 1 - H / sqrt(2)
  }
  return(res_list)
}
