#' Recipe for `sf` objects
#'
#' This method for [recipes::recipe()] handles the
#' case when `x` is an [sf::sf] object, as commonly
#' used in Species Distribution Model.
#'
#' [recipes] are not natively compatible with [sf::sf] objects. The problem is that
#' the `geometry` column of [sf::sf] objects is a list, which is incompatible with
#' the translation of formulae in [recipe]. This method strips the `geometry`
#' column from the [data.frame] before any further operations, thus allowing
#' the usual processing by [recipe()] to succeed.
#' NOTE that order matters! You need to use the syntax
#' `recipe(x=sf_obj, formula=class~.)` for the method to successfully detect
#' the [sf::sf] object. Starting with `formula` will fail.
#'
#' @param x An [sf::sf] data frame.
#' @param ... parameters to be passed to [recipes::recipe()]
#' @returns An object of class [recipes::recipe] , see
#' the manpage for [recipes::recipe()] for details.
#' @export
#' @import recipes
#' @importFrom rlang .data

recipe.sf <- function (x, ...) {
  # we should check that all coordinates are points
  x<-x %>% dplyr::bind_cols(sf::st_coordinates(x)) %>% sf::st_drop_geometry()
  recipe(x, ...) %>% add_role(dplyr::any_of(c("X","Y")),new_role="coords") %>%
    update_role_requirements("coords", bake = FALSE) 
}

## This breaks as X and Y might or might not be there
## The best option for an sf object is possibly to create a step_add_dummy_coords()
## which, if needed, will add the X and Y coordinates to the dataset, so that we can then set their
## role without an issue.
