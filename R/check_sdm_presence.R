#' Check that the column with presences is correctly formatted
#'
#' In `tidysdm`, the string defining presences should be the first level of
#' the response factor. This function checks that the column is correctly formatted.
#'
#' @param .data a `data.frame` or `tibble`, or a derived object such as an `sf` data.frame,
#' or a factor (e.g. the column with the response variable)
#' @param .col the column containing the presences
#' @param presence_level the string used to define the presence level of `.col`
#' @returns TRUE if correctly formatted
#' @export

check_sdm_presence <- function(
    .data,
    .col,
    presence_level = "presence") {
  .col <- rlang::enquo(.col) %>%
    rlang::quo_get_expr() %>%
    rlang::as_string()
  presence_level <- rlang::enquo(presence_level) %>%
    rlang::quo_get_expr() %>%
    rlang::as_string()
  browser
  # extract the column if .data is a dataframe
  if (inherits(.data, "data.frame")){
    x <- .data %>% dplyr::pull(.col)
  } else {
    x <- .data
  }
  if (!is.factor(x)) {
    stop(.col, " should be a factor")
  }
  if (!presence_level %in% levels(x)) {
    stop("level ", presence_level, " is not used in ", .col)
  }
  if (presence_level != levels(x)[1]) {
    stop("level ", presence_level, " is not the first level in ", .col)
  }
  return(TRUE)
}
