#' Thin points dataset based on geographic distance
#'
#' This function thins a dataset so that only observations that have a distance
#' from each other greater than "dist_min" are retained.
#'
#' Distances are measured in the appropriate units for the projection used. In case
#' of raw latitude and longitude (e.g. as provided in a data.frame), the crs is set
#' to WGS84, and units are set to meters.
#'
#' This function is a modified version of the algorithm in `spThin`, adapted
#' to work on `sf` objects.
#'
#' @param data An [`sf::sf`] data frame, or a data frame with coordinate variables.
#' These can be defined in `coords`, unless they have standard names
#' (see details below).
#' @param coords A vector of length two giving the names of the "x" and "y"
#' coordinates, as found in `data`. If left to NULL, the function will
#' try to guess the columns based on standard names `c("x", "y")`, `c("X","Y")`,
#'  `c("longitude", "latitude")`, or `c("lon", "lat")`
#' @param dist_min Minimum distance between points (in units appropriate for
#' the projection, or meters for lonlat data).
#' @returns An object of class [`sf::sf`] or [`data.frame`], the same as "data".
#' @export
#' @importFrom rlang :=

# This code is an adaptation of spThin to work on sf objects

thin_by_dist <- function(data, dist_min, coords = NULL) {
  return_dataframe <-
    FALSE # flag whether we need to return a data.frame
  if (!inherits(data, "sf")) {
    coords <- check_coords_names(data = data, coords = coords)
    data <-
      sf::st_as_sf(data, coords = coords) %>% sf::st_set_crs(4326)
    return_dataframe <- TRUE
  }

  # compute distances with sf, using the appropriate units for the projection
  dist_mat <- sf::st_distance(data)
  units(dist_min) <- units(dist_mat)
  dist_mat <- dist_mat < dist_min

  ## Set the diagonal of the dist matrix to FALSE
  diag(dist_mat) <- FALSE

  ## Set any NA values in the dist matrix to FALSE
  dist_mat[is.na(dist_mat)] <- FALSE

  ## sum up the number of neighbours that are TRUE (i.e > dist_min)
  n_neighbours <- rowSums(dist_mat)

  # points to keep (we will drop them progressively)
  points_to_keep <- rep(TRUE, length(n_neighbours))

  ## Perform while loop based on two criteria
  ## 1. The minimum distance between two occurences is less than the
  ##    thinning parameter
  ## 2. The number of rows in the resulting data set is greater than 1
  while (any(dist_mat) && sum(points_to_keep) > 1) {
    ## Identify the row(s) (occurence) that is within the thin.par distance
    ## to the greatest number of other occurrences.
    ## If there is more than one row, choose one at random to remove
    ## TODO should we give the option of sampling from this, to increate
    ## the stochasticity of the choices? We could sample with weights
    ## proportional to the number neighbours
    points_to_remove <- which(n_neighbours == max(n_neighbours))
    if (length(points_to_remove) > 1) {
      points_to_remove <- sample(points_to_remove, 1)
    }

    ## Assuming the row chosen above is removed, decrease the
    ## SumVec object by how many other rows are influenced by its removal
    n_neighbours <- n_neighbours - dist_mat[, points_to_remove]

    ## Set the SumVec value for the row to be removed equal to 0
    n_neighbours[points_to_remove] <- 0

    ## Set the occ to be ignored in the next iteration of the while loop
    dist_mat[points_to_remove,] <- FALSE
    dist_mat[, points_to_remove] <- FALSE

    ## Note the occurrence for removal from the thinned data set
    points_to_keep[points_to_remove] <- FALSE
  }

  ## Subset the original dataset
  thinned_points <- data[points_to_keep,]
  if (return_dataframe) {
    thinned_points <- thinned_points %>%
      dplyr::bind_cols(sf::st_coordinates(thinned_points)) %>% # re-add coordinates
      as.data.frame() %>% #turn it into a data.frame
      dplyr::select(-"geometry") %>% # remove the geometry column
      dplyr::rename("{coords[1]}" := "X", "{coords[2]}" := "Y")
  }
  return(thinned_points)
}
