#' Recipe for `sf` objects
#'
#' This method for [recipes::recipe()] handles the case when `x` is an [sf::sf]
#' object, as commonly used in Species Distribution Model, and generates a
#' `spatial_recipe`.
#'
#' [recipes::recipe()] are not natively compatible with [sf::sf] objects. The
#' problem is that the `geometry` column of [sf::sf] objects is a list, which is
#' incompatible with the translation of formulae in [recipes::recipe()]. This
#' method strips the `geometry` column from the [data.frame] and replaces it
#' with a simple `X` and `Y` columns before any further operations, thus
#' allowing the usual processing by [recipes::recipe()] to succeed (`X` and `Y`
#' are give the role of coords in a spatial recipe). When prepping and baking a
#' `spatial_recipe`, if a data.frame or tibble without coordinates is used as
#' `training` or `new_data`, dummy `X` and `Y` columns are generated and filled
#' with NAs. NOTE that order matters! You need to use the syntax
#' `recipe(x=sf_obj, formula=class~.)` for the method to successfully detect the
#' [sf::sf] object. Starting with `formula` will fail.
#'
#' @param x An [sf::sf] data frame.
#' @param ... parameters to be passed to [recipes::recipe()]
#' @returns An object of class `spatial_recipe`, which is a derived version of
#'   [recipes::recipe()] , see the manpage for [recipes::recipe()] for details.
#' @export
#' @import recipes

recipe.sf <- function(x, ...) {
  # we should check that all coordinates are points
  if (all(c("X", "Y") %in% names(x))) {
    if (all(sf::st_drop_geometry(x[, c("X", "Y")]) == sf::st_coordinates(x))) {
      x <- x %>% sf::st_drop_geometry()
    } else {
      stop(
        "sf object `x` contains `X` and `Y` coordinates that do not ",
        "match the sf point geometry"
      )
    }
  } else {
    x <- x %>%
      dplyr::bind_cols(sf::st_coordinates(x)) %>%
      sf::st_drop_geometry()
  }

  rec <- recipe(x, ...) %>%
    recipes::update_role(dplyr::any_of(c("X", "Y")), new_role = "coords")
  class(rec) <- c("spatial_recipe", class(rec))
  rec
}

#' @export
#' @rdname recipe.sf
spatial_recipe <- function(x, ...) {
  if (!inherits(x, "sf")) {
    stop("x should be an `sf` object")
  }
  recipes::recipe(x, ...)
}
