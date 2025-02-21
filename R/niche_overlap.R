#' Compute overlap metrics of the two niches
#'
#' This function computes overlap metrics between two rasters. It currently
#' implements Schoener's D and the inverse I of Hellinger's distance.
#'
#' Note that Hellinger's distance is normalised by dividing by square root of 2
#' (which is the correct asymptote for Hellinger's D), rather than the incorrect
#' 2 used originally in Warren et al (2008), based on the Erratum for that
#' paper.
#'
#' @param x a [terra::SpatRaster] with a single layer
#' @param y a [terra::SpatRaster] with a single layer
#' @param method a string (or vector of strings) taking values "Schoener" and
#'   "Hellinger"
#' @returns a list of overlap metrics, with slots *D* and *I* (depending on
#'   `method`)
#' @references Warren, D.L., Glor, R.E. & Turelli M. (2008) Environmental niche
#'   equivalency versus conservativism: quantitative approaches to niche
#'   evolution. Evolution 62: 2868-2883
#' @export
#'
niche_overlap <- function(x, y, method = c("Schoener", "Hellinger")) {
  if (inherits(x, "stars")) x <- as(x, "SpatRaster")
  if (inherits(y, "stars")) y <- as(y, "SpatRaster")

  if (any(terra::nlyr(x) != 1, terra::nlyr(y) != 1)) {
    stop("x and y are expected to each contain one layer")
  }
  # standardise the two distributions
  # divide by sum so that integral of pdf is one
  x <- x / unlist(terra::global(x, sum, na.rm = TRUE))
  y <- y / unlist(terra::global(y, sum, na.rm = TRUE))

  res_list <- list()
  # Schoeners D
  if ("Schoener" %in% method) {
    res_list$D <- unname(
      1 - 0.5 * unlist(terra::global(abs(x - y),
        sum,
        na.rm = TRUE
      ))
    )
  }

  # Hellinger's Distance
  if ("Hellinger" %in% method) {
    hell <- sqrt(unlist(
      terra::global((sqrt(x) - sqrt(y))^2, sum,
        na.rm = TRUE
      )
    ))
    # scaling in
    # https://onlinelibrary.wiley.com/doi/10.1111/j.1558-5646.2008.00482.x is
    # incorrect, as it uses 2 rather than sqrt(2) as the max value of H scaling
    # to 1 following
    # https://www.sciencedirect.com/science/article/pii/S2287884X18300153
    res_list$I <- unname(1 - hell / sqrt(2))
  }
  return(res_list)
}
