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
  x<-x %>% sf::st_drop_geometry()
  recipes::recipe(x, ...)
}
