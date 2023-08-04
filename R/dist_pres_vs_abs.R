#' Distance between the distribution of climate values for presences vs background
#'
#' For each environmental variable, this function computes the density functions
#' of presences and absences and returns (1-overlap), which is a measure of the
#' distance between the two distributions. Variables with a high distance are good
#' candidates for SDMs, as species occurrences are confined to a subset
#' of the available background.
#'
#' @param .data a `data.frame` (or derived object, such as `tibble`, or
#' `sf`) with values for the bioclimate variables for presences and background
#' @param .col the column containing the presences; it assumes presences to be
#' the first level of this factor
#' @returns a name vector of distances
#' @examples
#' # This should be updatd to use a dataset from tidysdm
#' data("bradypus", package="maxnet")
#' bradypus_tb <- tibble::as_tibble(bradypus) %>% dplyr::mutate(presence = relevel(factor(
#'   dplyr::case_match (presence, 1~"presence",0 ~"absence")),
#'   ref="presence")) %>% select(-ecoreg)
#'
#' bradypus_tb %>% dist_pres_vs_bg(presence)
#'
#' @export

dist_pres_vs_bg <- function(
    .data,
    .col){
  .col = rlang::enquo(.col) %>% rlang::quo_get_expr() %>% rlang::as_string()
  if (inherits(.data, "sf")){
    .data <- .data %>% sf::st_drop_geometry()
  }
  # subset to only columns which are numeric
  num_vars <- names(.data)[!names(.data)%in%.col]
  dist_vec<- numeric()
  for (i_var in num_vars){
    vals_list <- list(.data[[i_var]][.data[[.col]]==levels(.data[[.col]])[1]],
                       .data[[i_var]][.data[[.col]]==levels(.data[[.col]])[2]])
    dist_vec <- c(dist_vec, 1-overlapping::overlap(vals_list)$OV)
  }
  names(dist_vec)<- num_vars
  dist_vec[order(dist_vec, decreasing = TRUE)]
}
