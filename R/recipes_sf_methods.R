#' @export
prep.spatial_recipe <- function(x, training = NULL, fresh = FALSE, verbose = FALSE,
                                retain = TRUE, log_changes = FALSE, strings_as_factors = TRUE,
                                ...) {
  if (!is.null(training)) {
    # if we have a geometry
    if ("geometry" %in% names(training)) {
      ## convert_geometry_column
      training <- training %>%
        dplyr::bind_cols(sf::st_coordinates(training$geometry)) %>%
        sf::st_drop_geometry()
    }
    # Add dummy X and Y if they are not already present
    if (!all(c("X", "Y") %in% names(training))) {
      training <- training %>% dplyr::mutate(X = NA, Y = NA)
    }
  }
  NextMethod(generic="prep",
    x = x, training = sf::st_drop_geometry(training),
    fresh = fresh, verbose = FALSE,
    retain = retain, log_changes = log_changes,
    strings_as_factors = strings_as_factors, ...)
}

#' @export
bake.spatial_recipe <- function(object, new_data, ..., composition = "tibble") {
  ## convert_geometry_column
  if (!is.null(new_data)) {
    if ("geometry" %in% names(new_data)) {
      ## convert_geometry_column
      new_data <- new_data %>%
        dplyr::bind_cols(sf::st_coordinates(new_data$geometry)) %>%
        sf::st_drop_geometry()
    }
    # Add dummy X and Y if they are not already present
    if (!all(c("X", "Y") %in% names(new_data))) {
      new_data <- new_data %>% dplyr::mutate(X = NA, Y = NA)
    }
  }
  NextMethod(generic="bake", object = object, 
                  new_data = sf::st_drop_geometry(new_data), ...,
                  composition = composition)
}
