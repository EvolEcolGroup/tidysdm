#' Coordinates of presences for Iberian emerald lizard
#'
#' Coordinates for presences of *Lacerta schreiberi*. The variables are as
#' follows:
#'
#' @format An `tibble` with 1,297 rows and 3 variables:
#' \describe{
#'   \item{ID}{ids from GBIF}
#'   \item{latitude}{latitudes in degrees}
#'   \item{longitude}{longitudes in degrees}
#' }
"lacerta"

#' Coordinates of radiocarbon dates for horses
#'
#' Coordinates for presences of horses from 22k to 8k YBP.
#'
#' @format An `tibble` with 1,297 rows and 3 variables:
#' \describe{
#'   \item{latitude}{latitudes in degrees}
#'   \item{longitude}{longitudes in degrees}
#'   \item{time_bp}{time in years before present}
#' }
"horses"

#' A simple ensemble for the lacerta data
#'
#' Ensemble SDM for *Lacerta schreiberi*, as generated in the vignette.
#'
#' @format A [`simple_ensemble`] object
"lacerta_ensemble"

#' A repeat ensemble for the lacerta data
#'
#' Ensemble SDM for *Lacerta schreiberi*, as generated in the vignette.
#'
#' @format A [`repeat_ensemble`] object
"lacerta_rep_ens"
