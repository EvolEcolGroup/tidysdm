#' Plot presences vs background
#'
#' Create a composite plots contrasting the distribution of multiple variables
#' for presences vs the background.
#'
#' @param .data either a `data.frame` (or derived object, such as `tibble`, or
#' `sf`) with values for the bioclimate variables for presences and background;
#' or an `sf` object with coordinates for presences and absences, which will be
#' extracted from
#' @param .col the column containing the presences; it assumes presences to be
#' the first level of this factor
#' @export

plot_pres_vs_bg <- function(
    .data,
    .col,
    raster=NULL){
  .col = rlang::enquo(.col) %>% rlang::quo_get_expr() %>% rlang::as_string()
  # subset to only columns which are numeric
  num_vars <- names(.data)[!names(.data)%in%.col]
  .data$x<-rep("",nrow(.data))
  plot_list <- list()
  for (i_var in num_vars){
    # remove legend and add it to the last panel
    p <- ggplot(data=.data, aes(x=x, y=.data[[i_var]],fill=.data[[.col]]))+
      geom_split_violin(nudge=0.01)
    browser()
  }



}
