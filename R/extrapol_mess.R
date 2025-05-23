#' Multivariate environmental similarity surfaces (MESS)
#'
#' Compute multivariate environmental similarity surfaces (MESS), as described
#' by Elith et al., 2010.
#'
#' This function is a modified version of `mess` in package `predicts`, with a
#' method added to work on [`terra::SpatRasterDataset`]. Note that the method
#' for [`terra::SpatRasterDataset`] assumes that each variables is stored as a
#' [`terra::SpatRaster`] with time information within `x`. Time is also assumed
#' to be in `years`. If these conditions are not met, it is possible to manually
#' extract a [`terra::SpatRaster`] for each time step, and use `extrapol_mess`
#' on those [`terra::SpatRaster`]s
#'
#'
#' @param x [`terra::SpatRaster`], `stars`, [`terra::SpatRasterDataset`] or
#'   [`data.frame`]
#' @param training matrix or data.frame or sf object containing the reference
#'   values; each column should correspond to one layer of the
#'   [`terra::SpatRaster`] object, with the exception of the presences column
#'   defined in `.col` (optional).
#' @param ... additional arguments as for [terra::writeRaster()]
#' @return a [`terra::SpatRaster`] (data.frame) with the MESS values.
#' @author Jean-Pierre Rossi, Robert Hijmans, Paulo van Breugel, Andrea Manica
#' @references Elith J., M. Kearney M., and S. Phillips, 2010. The art of
#'   modelling range-shifting species. Methods in Ecology and Evolution
#'   1:330-342.
#' @export
#' @keywords extrapolation

# author: Jean-Pierre Rossi <jean-pierre.rossi@supagro.inra.fr>
# modifications by Robert Hijmans and Paulo van Breugel
# rewritten for predicts by RH
# adapted for tidysdm by AM

extrapol_mess <- function(x, training, .col, ...) {
  UseMethod("extrapol_mess", object = x)
}

#' @rdname extrapol_mess
#' @export
extrapol_mess.default <- function(x, training, ...) {
  stop("no method available for this object type")
}


#' @rdname extrapol_mess
#' @export
extrapol_mess.stars <- function(x, ...) {
  extrapol_mess(as(x, "SpatRaster"), ...) %>%
    stars::st_as_stars(as_attributes = TRUE)
}

#' @rdname extrapol_mess
#' @param .col the column containing the presences (optional). If specified,
#' it is excluded when computing the MESS scores.
#' @param filename character. Output filename (optional)
#' @export
extrapol_mess.SpatRaster <- function(x, training, .col, filename = "", ...) {
  # remove the class column if it is present
  .col <- rlang::enquo(.col) %>%
    rlang::quo_get_expr() %>%
    rlang::as_string()
  if (.col != "") {
    training <- training %>% dplyr::select(-dplyr::one_of(.col))
  }
  # remove locations in training if they are present
  training <- training %>% sf::st_drop_geometry()
  # check that all variables are present in the raster
  if (!setequal(names(training), names(x))) {
    stop("`x` and `training` should contain the same variables")
  }

  training <- stats::na.omit(training)
  if (nrow(training) < 2) {
    stop("insufficient number of reference points")
  }

  out <- terra::rast(x)
  nl <- terra::nlyr(x)
  terra::readStart(x)
  on.exit(terra::readStop(x))
  if (nl == 1) {
    names(out) <- "mess"
    b <- terra::writeStart(out, filename, ...)
    for (i in 1:b$n) {
      vv <- terra::readValues(x, b$row[i], b$nrows[i])
      p <- .messi(vv, training)
      terra::writeValues(out, p, b$row[i], b$nrows[i])
    }
  } else {
    terra::nlyr(out) <- 1
    names(out) <- "mess"
    b <- terra::writeStart(out, filename, ...)
    for (i in 1:b$n) {
      vv <- terra::readValues(x, b$row[i], b$nrows[i], mat = TRUE)
      vv <- sapply(
        seq_len(ncol(training)),
        function(i) .messi(vv[, i], training[, i])
      )
      suppressWarnings(m <- apply(vv, 1, min, na.rm = TRUE))
      m[!is.finite(m)] <- NA
      terra::writeValues(out, m, b$row[i], b$nrows[i])
    }
  }
  terra::writeStop(out)
  out
}


#' @rdname extrapol_mess
#' @param .col the column containing the presences (optional). If specified,
#' it is excluded when computing the MESS scores.
#' @export
extrapol_mess.data.frame <- function(x, training, .col, ...) {
  rlang::check_dots_empty()
  # remove the class column if it is present
  .col <- rlang::enquo(.col) %>%
    rlang::quo_get_expr() %>%
    rlang::as_string()
  if (.col != "") {
    training <- training %>% dplyr::select(-dplyr::one_of(.col))
  }
  # remove locations in training if they are present
  training <- training %>% sf::st_drop_geometry()
  x <- x %>% sf::st_drop_geometry()
  # check that all variables are present in the raster
  if (!setequal(names(training), names(x))) {
    stop("`x` and `training` should contain the same variables")
  }

  training <- stats::na.omit(training)
  if (nrow(training) < 2) {
    stop("insufficient number of reference points")
  }

  if (ncol(x) == 1) {
    data.frame(mess = .messi(x[, 1], training))
  } else {
    x <- sapply(seq_len(ncol(x)), function(i) .messi(x[, i], training[, i]))
    rmess <- apply(x, 1, min, na.rm = TRUE)
    data.frame(mess = rmess)
  }
}


#' @rdname extrapol_mess
#' @param .col the column containing the presences (optional). If specified,
#' it is excluded when computing the MESS scores.
#' @export
extrapol_mess.SpatRasterDataset <- function(x, training, .col, ...) {
  rlang::check_dots_empty()
  # remove the class column if it is present
  .col <- rlang::enquo(.col) %>%
    rlang::quo_get_expr() %>%
    rlang::as_string()
  if (.col != "") {
    training <- training %>% dplyr::select(-dplyr::one_of(.col))
  }

  # check this is a valid region_series
  # TODO

  # get times from the first layer
  if (!terra::timeInfo(x[[1]])$time) {
    stop(
      "The rasters in `SpatRasterDataset` 'x' have no time info; define ",
      "times with terra::time"
    )
  }
  if (terra::timeInfo(x[[1]])$step != "years") {
    stop(
      "The time step of `x` is not `years`; you will need to extract one ",
      "time step at a time manually and compute the MESS on each SpatRaster"
    )
  }
  raster_times <- terra::time(x[[1]])

  # null mess raster to fill in
  mess_rast <- NULL
  # cycle over the time steps
  for (i_time in seq_len(length(raster_times))) {
    # get a slice
    this_slice <- pastclim::slice_region_series(x,
      time_ce = raster_times[i_time]
    )
    # apply mess to the slice
    this_mess <- extrapol_mess(this_slice, training = training)
    if (is.null(mess_rast)) {
      mess_rast <- this_mess
    } else {
      terra::add(mess_rast) <- this_mess
    }
  }
  # now sort out the names
  names(mess_rast) <- paste0("mess_", raster_times)
  terra::time(mess_rast) <- terra::time(x[[1]])
  return(mess_rast)
}



.messi <- function(p, v) {
  if (inherits(v, "data.frame")) {
    v <- v %>% dplyr::pull()
  }
  v <- sort(v)
  f <- 100 * findInterval(p, v) / length(v)
  minv <- v[1]
  maxv <- v[length(v)]
  res <- 2 * f
  f[is.na(f)] <- -99
  i <- f > 50 & f < 100
  res[i] <- 200 - res[i]

  i <- f == 0
  res[i] <- 100 * (p[i] - minv) / (maxv - minv)
  i <- f == 100
  res[i] <- 100 * (maxv - p[i]) / (maxv - minv)
  res
}
