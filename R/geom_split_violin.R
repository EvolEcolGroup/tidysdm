#' Split violin geometry for ggplots
#'
#' This geometry displays the density distribution of two groups side by side,
#' as two halves of a violin. Note that an empty`x` aesthetic has to be provided even
#' if you want to plot a single variable (see example below).
#'
#' The implementation is based on https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot2.
#' Credit goes to @jan-jlx for providing a complete implementation on StackOverflow, and to
#' Trang Q. Nguyen for adding the nudge parameter.
#'
#' @inheritParams ggplot2::geom_violin
#' @param stat Use to override the default connection between [geom_violin()]
#' and [stat_ydensity()].
#' @param nudge Add space between the half-violin and the middle of the space
#' allotted to a given factor on the x-axis.
#' @returns a [`ggplot2::layer`] object
#' @examples
#' data("bradypus", package = "maxnet")
#' bradypus_tb <- tibble::as_tibble(bradypus) %>% dplyr::mutate(presence = relevel(
#'   factor(
#'     dplyr::case_match(presence, 1 ~ "presence", 0 ~ "absence")
#'   ),
#'   ref = "presence"
#' ))
#'
#' ggplot(bradypus_tb, aes(
#'   x = "",
#'   y = cld6190_ann,
#'   fill = presence
#' )) +
#'   geom_split_violin(nudge = 0.01)
#'
#' @export

geom_split_violin <- function(mapping = NULL,
                              data = NULL,
                              stat = "ydensity",
                              position = "identity",
                              # nudge param here
                              nudge = 0,
                              ...,
                              draw_quantiles = NULL,
                              trim = TRUE,
                              scale = "area",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSplitViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      # don't forget the nudge
      nudge = nudge,
      draw_quantiles = draw_quantiles,
      na.rm = na.rm,
      ...
    )
  )
}

#' @keywords internal
GeomSplitViolin <- ggplot2::ggproto(
  "GeomSplitViolin",
  ggplot2::GeomViolin,
  draw_group = function(self,
                        data,
                        ...,
                        # add the nudge here
                        nudge = 0,
                        draw_quantiles = NULL) {
    data <- transform(data,
      xminv = x - violinwidth * (x - xmin),
      xmaxv = x + violinwidth * (xmax - x)
    )
    grp <- data[1, "group"]
    newdata <- dplyr::arrange(
      transform(data,
        x = if (grp %% 2 == 1) xminv else xmaxv
      ),
      if (grp %% 2 == 1) y else -y
    )
    newdata <- rbind(
      newdata[1, ],
      newdata,
      newdata[nrow(newdata), ],
      newdata[1, ]
    )
    newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])

    # now nudge them apart
    newdata$x <- ifelse(newdata$group %% 2 == 1,
      newdata$x - nudge,
      newdata$x + nudge
    )

    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))

      quantiles <- ggplot2:::create_quantile_segment_frame(
        data,
        draw_quantiles
      )
      aesthetics <- data[rep(1, nrow(quantiles)),
        setdiff(names(data), c("x", "y")),
        drop = FALSE
      ]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- ggplot2::GeomPath$draw_panel(both, ...)
      ggplot2:::ggname(
        "geom_split_violin",
        grid::grobTree(
          ggplot2::GeomPolygon$draw_panel(newdata, ...),
          quantile_grob
        )
      )
    } else {
      ggplot2:::ggname(
        "geom_split_violin",
        ggplot2::GeomPolygon$draw_panel(newdata, ...)
      )
    }
  }
)
