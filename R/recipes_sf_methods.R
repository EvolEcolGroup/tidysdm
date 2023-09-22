# foo<-workflow(
#   preprocessor = lacerta_rec_uncor,
#   spec = sdm_spec_glm()
# )
# 
# foo<-lacerta_rec_uncor %>% prep(st_drop_geometry(lacerta_thin))
# foo <- prep(foo, lacerta_thin)
# bake (foo, lacerta_thin)
#' @export
prep.spatial_recipe<-function (x, training = NULL, fresh = FALSE, verbose = FALSE, 
                       retain = TRUE, log_changes = FALSE, strings_as_factors = TRUE, 
                       ...) {
 # browser()
  if (!is.null(training)){
    # if we have a geometry
    if ("geometry" %in% names(training)){
      ## convert_geometry_column
      training <- training %>%
        dplyr::bind_cols(sf::st_coordinates(training$geometry)) %>%
        sf::st_drop_geometry()
    }
    # Add dummy X and Y if they are not already present
    if (!all(c("X","Y") %in% names(training))){
      training <- training %>% dplyr::mutate(X=NA, Y=NA)
    }
  }
  #recipes:::prep.recipe
  utils::getFromNamespace("prep.recipe", "recipes") (
    x=x,training=sf::st_drop_geometry(training),
    fresh=fresh, verbose=FALSE,
    retain=retain, log_changes=log_changes,
    strings_as_factors =strings_as_factors, ...)
}

#' @export
bake.spatial_recipe <- function (object, new_data, ..., composition = "tibble") {
  ## convert_geometry_column
  if ("geometry" %in% names(new_data)){
    ## convert_geometry_column
    training <- training %>%
      dplyr::bind_cols(sf::st_coordinates(new_data$geometry)) %>%
      sf::st_drop_geometry()
  }
  # Add dummy X and Y if they are not already present
  if (!all(c("X","Y") %in% names(new_data))){
    new_data <- new_data %>% dplyr::mutate(X=NA, Y=NA)
  }
  #recipes:::bake.recipe
  utils::getFromNamespace("bake.recipe", "recipes") (object=object, ..., 
                        new_data = sf::st_drop_geometry(new_data),
                        composition=composition)
}


