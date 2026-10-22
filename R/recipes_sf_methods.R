#' @export
prep.spatial_recipe <- function(x, training = NULL,
                                fresh = FALSE,
                                verbose = FALSE,
                                retain = TRUE,
                                log_changes = FALSE,
                                strings_as_factors = TRUE,
                                ...) {
  if (!is.null(training)) {
    # sometimes we have a tibble with a geometry column but not cast into an sf
    # object (this is the case when running a workflow)
    if (inherits(training, "sf")) {
      geom_col <- attributes(training)$sf_column
    } else {
      geom_col <- names(training)[unlist(lapply(training, inherits, "sfc"))]
    }
    if (length(geom_col) > 0) {
      geom_col <- geom_col[1]
      training <- training %>%
        dplyr::bind_cols(sf::st_coordinates(training[[geom_col]])) %>%
        sf::st_drop_geometry()
    }
    # Add dummy X and Y if they are not already present
    if (!all(c("X", "Y") %in% names(training))) {
      training <- training %>% dplyr::mutate(X = NA_real_, Y = NA_real_)
    }
    training <- sf::st_drop_geometry(training)
  }
  # NextMethod() passes on the arguments as they were given by the user, with
  # the updated value of training. Passing arguments explicitly would add them
  # to the call rather than replacing those given by position (which would
  # then end up in the wrong argument), and would always pass
  # strings_as_factors, which is deprecated in prep.recipe()
  NextMethod()
}

#' @export
bake.spatial_recipe <- function(object, new_data, ..., composition = "tibble") {
  ## convert_geometry_column
  if (!is.null(new_data)) {
    # sometimes we have a tibble with a geometry column but not cast into an sf
    # object (this is the case when running a workflow)
    if (inherits(new_data, "sf")) {
      geom_col <- attributes(new_data)$sf_column
    } else {
      geom_col <- names(new_data)[unlist(lapply(new_data, inherits, "sfc"))]
    }
    if (length(geom_col) > 0) {
      # if we have multiple geometry column, we use the first one
      geom_col <- geom_col[1]
      new_data <- new_data %>%
        dplyr::bind_cols(sf::st_coordinates(new_data[[geom_col]])) %>%
        sf::st_drop_geometry()
    }
    # Add dummy X and Y if they are not already present
    if (!all(c("X", "Y") %in% names(new_data))) {
      new_data <- new_data %>% dplyr::mutate(X = NA_real_, Y = NA_real_)
    }
    new_data <- sf::st_drop_geometry(new_data)
  }
  # NextMethod() passes on the arguments as they were given by the user, with
  # the updated value of new_data (see prep.spatial_recipe() for why we do not
  # pass arguments explicitly)
  NextMethod()
}
