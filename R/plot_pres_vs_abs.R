#' Plot presences vs background
#'
#' Create a composite plots contrasting the distribution of multiple variables
#' for presences vs the background.
#'
#' @param .data a [`data.frame`] (or derived object, such as [`tibble::tibble`], or
#' [`sf::st_sf`]) with values for the bioclimate variables for presences and background
#' @param .col the column containing the presences; it assumes presences to be
#' the first level of this factor
#' @returns a `patchwork` composite plot
#' @examples
#' data("bradypus", package = "maxnet")
#' bradypus_tb <- tibble::as_tibble(bradypus) %>%
#'   dplyr::mutate(presence = relevel(
#'     factor(
#'       dplyr::case_match(presence, 1 ~ "presence", 0 ~ "absence")
#'     ),
#'     ref = "presence"
#'   )) %>%
#'   select(-ecoreg)
#'
#' bradypus_tb %>% plot_pres_vs_bg(presence)
#'
#' @export

plot_pres_vs_bg <- function(
    .data,
    .col) {
  .col <- rlang::enquo(.col) %>%
    rlang::quo_get_expr() %>%
    rlang::as_string()
  if (inherits(.data, "sf")) {
    .data <- .data %>% sf::st_drop_geometry()
  }
  # subset to only columns which are numeric
  num_vars <- names(.data)[!names(.data) %in% .col]
  plot_list <- list()
  for (i_var in num_vars) {
    p <- ggplot2::ggplot(data = .data, ggplot2::aes(x = "", y = .data[[i_var]], fill = .data[[.col]])) +
      geom_split_violin(nudge = 0.01) +
      ggplot2::labs(x = "")
    # vals_list <- list(.data[[i_var]][.data[[.col]]==levels(.data[[.col]])[1]],
    #                   .data[[i_var]][.data[[.col]]==levels(.data[[.col]])[2]])
    #
    # p <- p+annotate("text",0.5,min(.data[[i_var]])*1.1,label=round(1-overlap(vals_list)$OV,2))
    plot_list[[i_var]] <- p
  }
  plot_patch <- patchwork::wrap_plots(plot_list) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
  return(plot_patch)
}
