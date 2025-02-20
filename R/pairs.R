#' Pairwise matrix of scatterplot for stars objects
#'
#' Pairs plot of attributes for `stars` objects. This is equivalent to
#' [terra::pairs()] but works with `stars` objects.
#' @inheritParams terra::pairs
#' @rdname pairs-stars
#' @export
#' @examples
#' r <- terra::rast(system.file("ex/elev.tif", package = "terra"))
#' s <- c(r, 1 / r, sqrt(r))
#' names(s) <- c("elevation", "inverse", "sqrt")
#' terra::pairs(s)
#' s_stars <- stars::st_as_stars(s, as_attributes = TRUE)
#' pairs(s_stars)
setMethod(
  "pairs", signature(x = "stars"),
  function(x, hist = TRUE, cor = TRUE, use = "pairwise.complete.obs", 
           maxcells = 100000, ...) {
    if (length(x) < 2) {
      stop("x must have at least two layers")
    }

    panelhist <- function(x, ...) {
      usr <- graphics::par("usr")
      on.exit(graphics::par(usr = usr))
      graphics::par(usr = c(usr[1:2], 0, 1.5))
      h <- hist(x, plot = FALSE)
      breaks <- h$breaks
      n_breaks <- length(breaks)
      y <- h$counts
      y <- y / max(y)
      graphics::rect(breaks[-n_breaks], 0, breaks[-1], y, col = "green")
    }

    panelcor <- function(x, y, ...) {
      usr <- graphics::par("usr")
      on.exit(graphics::par(usr = usr))
      graphics::par(usr = c(0, 1, 0, 1))
      r <- abs(stats::cor(x, y, use = use))
      txt <- format(c(r, 0.123456789), digits = 2)[1]
      text(0.5, 0.5, txt, cex = max(0.5, r * 2))
    }

    if (hist) {
      dp <- panelhist
    } else {
      dp <- NULL
    }
    if (cor) {
      up <- panelcor
    } else {
      up <- NULL
    }

    N <- prod(dim(x))
    maxcells <- pmin(N, maxcells)
    ix <- sample(N, maxcells, replace = FALSE)
    d <- sapply(names(x),
      function(name, x = NULL, index = NULL) {
        x[[name]][index]
      },
      x = x, index = ix, simplify = FALSE
    ) |>
      as.data.frame()

    dots <- list(...)
    cex <- dots$cex
    main <- dots$main
    if (is.null(cex)) cex <- 0.5
    if (is.null(main)) main <- ""

    graphics::pairs(d, main = main, cex = cex,
                    upper.panel = up, diag.panel = dp)
  }
)
