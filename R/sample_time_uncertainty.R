#' Sample a time when have a range or a distribution
#'
#' The time (date) of a sample can come with uncertainty (e.g. typical of
#' radiocarbon dates). This function samples a time from a distribution of
#' possible times, which can be defined by a truncated normal distribution (with
#' mean and sd) or a uniform distribution (with min and max). Fixed times (i.e.
#' times without uncertainty) can also be included for certain samples. The
#' function returns a vector of sampled times.
#'
#' @details Each sample (i.e. row) should only include one type of information,
#'   i.e. either the mean and sd, or the min and max, or a fixed time.
#'
#' @param data An [`sf::sf`] data frame, or a data frame with time information.
#'   If an `sf` object, the geometry is ignored.
#' @param mean_sd_truncnorm_col a vector of length 2 with the names of the
#'   columns with the mean and standard deviation of the truncated normal
#'   distribution to sample from. It defaults to "mean_time" and "sd_time", but
#'   if the column names are with different names, they can be specified here.
#'   The "mean_time" column should be either a lubridate object, or an object
#'   that can be converted to a lubridate object with `lubridate_fun`.
#' @param sd_time_units the units of the standard deviation of the truncated
#'   normal'   distribution (e.g. "years" or "days"). This has to be defined
#'   if the sd_time column exists (i.e. if some dates are being resampled from
#'   a truncated normal).
#' @param old_young_unif_col a vector of length 2 with the names of the columns
#'   with the minimum and maximum values of the uniform distribution to sample
#'   from. It defaults to "oldest_time" and "youngest_time", but if the column names are
#'   different, they can be specified here. The "oldest_time" and "youngest_time"
#'   columns should be either lubridate objects, or objects that can be
#'   converted to lubridate objects with `lubridate_fun`.
#' @param fixed_time_col The name of the column with fixed times that do not
#'   need resampling (e.g. contemporary samples). It defaults to "fixed_time",
#'   but if the column name is different, it can be specified here. The
#'   "fixed_time" column should be either a lubridate object, or an object that
#'   can be converted to a lubridate object with `lubridate_fun`.
#' @param lubridate_fun function to convert the time columns (mean, oldest, youngest and
#'   fixed) into a lubridate object
#' @returns a vector of lubridate objects with the sampled times
#' @export


sample_time_uncertainty <- function(data,
                                    mean_sd_truncnorm_col = c("mean_time", "sd_time"),
                                    sd_time_units = NULL,
                                    old_young_unif_col = c("oldest_time", "youngest_time"),
                                    fixed_time_col = "fixed_time",
                                    lubridate_fun = c) {
  # check that data is a dataframe or an sf object
  if (!inherits(data, c("data.frame", "sf"))) {
    stop("`data` should be a data frame or an sf object")
  }

  # drop the geometry if it exists, and convert to a tibble
  data <- data %>%
    sf::st_drop_geometry()

  # check that the mean and sd columns exist
  if (all(mean_sd_truncnorm_col %in% names(data))) {
    mean_sd_exist <- TRUE
    # convert to lubridate objects
    data <- data %>%
      dplyr::mutate(
        mean_time = lubridate_fun(.data[[mean_sd_truncnorm_col[1]]]),
        # convert sd_time into the right units
        sd_time = units::as_units(.data[[mean_sd_truncnorm_col[2]]], sd_time_units)
      )
    # test that they are valid dates
    if (!inherits(data$mean_time, "POSIXct")) {
      stop("the first (mean) column specified in `mean_sd_truncnorm_col` ",
      "cannot be converted to dates with `lubridate_fun`")
    }
    # check that for every row with mean_time, we also have a value for sd time,
    # and vice versa
    if (any(is.na(data$mean_time) & !is.na(data$sd_time)) ||
      any(!is.na(data$mean_time) & is.na(data$sd_time))) {
      stop("for every row with a value for mean_time, there should also be a ",
           "value for sd_time, and vice versa")

  } else { # if they don't exist
    mean_sd_exist <- FALSE
    # throw an error if we were custom values for the mean and sd columns
    if (!all(mean_sd_truncnorm_col == c("mean_time", "sd_time"))) {
      stop("the columns specified in `mean_sd_truncnorm_col` do not exist in `data`")
    }
  }

  # now do the same for the min and max columns
  if (all(old_young_unif_col %in% names(data))) {
    old_young_exist <- TRUE
    # convert to lubridate objects
    data <- data %>%
      dplyr::mutate(
        oldest_time = lubridate_fun(.data[[old_young_unif_col[1]]]),
        youngest_time = lubridate_fun(.data[[old_young_unif_col[2]]])
      )
    # test that they are valid dates
    if (!inherits(data$oldest_time, "POSIXct") || !inherits(data$youngest_time, "POSIXct")) {
      stop("the columns specified in `old_young_unif_col` ",
      "cannot be converted to dates with `lubridate_fun`")
    }
    # check that for every row with oldest_time, we also have a value for youngest_time,
    # and vice versa
    if (any(is.na(data$oldest_time) & !is.na(data$youngest_time)) ||
      any(!is.na(data$oldest_time) & is.na(data$youngest_time))) {
      stop("for every row with a value for oldest_time, there should also be a ",
           "value for youngest_time, and vice versa")
    }
  } else {
    old_young_exist <- FALSE
    if (!all(old_young_unif_col == c("oldest_time", "youngest_time")))
      stop("the columns specified in `old_young_unif_col` do not exist in `data`")
  }

  # and for the fixed time column
  if (fixed_time_col %in% names(data)) {
    fixed_time_exist <- TRUE
    # convert to lubridate objects
    data <- data %>%
      dplyr::mutate(
        fixed_time = lubridate_fun(.data[[fixed_time_col]])
      )
    # test that they are valid dates
    if (!inherits(data$fixed_time, "POSIXct")) {
      stop("the column specified in `fixed_time_col` ",
      "cannot be converted to dates with `lubridate_fun`")
    }
  } else {
    fixed_time_exist <- FALSE
    if (fixed_time_col != "fixed_time") {
      stop("the column specified in `fixed_time_col` does not exist in `data`")
    }
  }

  # now check that at least one of either mean_sd or min_max exist
  if (!mean_sd_exist && !old_young_exist) {
    stop(
      "none of the columns specified in `mean_sd_truncnorm_col` and ",
      "`old_young_unif_col` exist in `data`"
    )
  }

  #subset to only the time columns
  data <- data %>%
    dplyr::select(
      dplyr::any_of(c("mean_time",
      "sd_time",
      "oldest_time",
      "youngest_time",
      "fixed_time"
    )))

  # TODO We stopped here!!!!!!

  # check that for every row we have at least one type of time information (i.e.
  # either mean and sd, or oldest and youngest, or fixed time)
  if (any(
    is.na(data$mean_time) & is.na(data$oldest_time) & is.na(data$fixed_time)
  )) {
    stop(
      "for every row, there should be at least one type of time information: ",
      "either mean and sd, or oldest and youngest, or fixed time"
    )
  }

  browser()
  # sample times from the distributions
  sampled_times <- vector(mode = "list", length = nrow(data))
  for (i in seq_len(nrow(data))) {
    if (!is.na(data$mean_time[i]) && !is.na(data$sd_time[i])) {
      sampled_times[[i]] <- rnorm(
        n = 1,
        a = as.numeric(data$oldest_time[i]),
        b = as.numeric(data$youngest_time[i]),
        mean = as.numeric(data$mean_time[i]),
        sd = as.numeric(data$sd_time[i])
      ) %>%
        lubridate::as_datetime()
    } else if (!is.na(data$oldest_time[i]) && !is.na(data$youngest_time[i])) {
      sampled_times[[i]] <- runif(
        n = 1,
        min = as.numeric(data$oldest_time[i]),
        max = as.numeric(data$youngest_time[i])
      ) %>%
        lubridate::as_datetime()
    } else if (!is.na(data$fixed_time[i])) {
      sampled_times[[i]] <- data$fixed_time[i]
    } else {
      sampled_times[[i]] <- NA
    }
  }
}

