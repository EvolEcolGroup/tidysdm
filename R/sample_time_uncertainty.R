#' Sample uncertain specimen times subject to relative ordering constraints
#'
#' @description
#' `sample_time_uncertainty()` samples one time for each specimen from one of
#' three supported per-specimen time models: fixed time, uniform interval, or
#' truncated normal uncertainty around a mean time. It can additionally enforce
#' relative temporal-ordering constraints encoded as specimen IDs that are known
#' to be older or younger than each focal specimen.
#'
#' The ordering information is treated as a directed acyclic graph within each
#' group. This allows strict stratigraphic sequences, partial sequences,
#' intrusive/out-of-sequence specimens, and specimens constrained by multiple
#' older and/or younger neighbours.
#'
#' @details
#' ## Date direction
#'
#' This function assumes that times are represented as calendar-like
#' [`POSIXct`][base::DateTimeClasses] values, where older specimens have earlier
#' times. Therefore, if specimen `A` is younger than specimen `B`, the sampled
#' times must satisfy:
#'
#' ```
#' sampled_time[B] < sampled_time[A]
#' ```
#'
#' This is appropriate for calendar dates. If your data are expressed as ages
#' before present, where larger numbers are older, convert them to calendar dates
#' before using this function, or adapt the constraint comparison accordingly.
#'
#' ## Time information per row
#'
#' Each row must contain exactly one of the following time specifications:
#'
#' * a truncated normal distribution, given by `trnorm_cols`;
#' * a uniform interval, given by `unif_cols`;
#' * a fixed time, given by `fixed_col`.
#'
#' Missing columns are allowed if that type of time information is not used.
#'
#' ## Relative ordering constraints
#'
#' Constraints are supplied in two optional columns:
#'
#' * `older_col`: IDs of specimens known to be older than the focal row;
#' * `younger_col`: IDs of specimens known to be younger than the focal row.
#'
#' These columns may be list-columns of character vectors, or character columns
#' containing multiple IDs separated by `id_sep`.
#'
#' IDs are resolved within `group_col`, so the same `sample_id` may be reused in
#' different groups/sites.
#'
#' Internally, both columns are converted to a constraint table with columns:
#'
#' `group_id`, `younger_id`, and `older_id`.
#'
#' ## Sampling method
#'
#' The constrained sampler uses rejection sampling:
#'
#' 1. sample all specimen times independently from their per-row uncertainty
#'    models;
#' 2. check all ordering constraints;
#' 3. repeat until all constraints are satisfied or `max_iter` is reached.
#'
#' This is simple and robust for small to moderate numbers of constraints. Very
#' tight or contradictory constraints may lead to rejection failure.
#'
#' @param data A data frame or an `sf` object containing specimen-level time and
#'   constraint information. If `data` is an `sf` object, geometry is dropped.
#' @param trnorm_cols Character vector of length 2 giving the columns containing
#'   the mean time and standard deviation for truncated-normal sampling. Defaults
#'   to `c("mean_time", "sd_time")`.
#' @param trnorm_n_sd Numeric scalar. Number of standard deviations on each side
#'   of the mean used as the truncation interval. Defaults to `2`.
#' @param sd_time_units Character scalar giving units for `sd_time` when
#'   truncated-normal dates are used. Supported values are seconds, minutes,
#'   hours, days, weeks, months, and years, including common singular/plural
#'   abbreviations. Required if any truncated-normal rows are present and
#'   `sd_time` is not already a `units` object.
#' @param unif_cols Character vector of length 2 giving the columns containing
#'   oldest and youngest bounds for uniform sampling. Defaults to
#'   `c("oldest_time", "youngest_time")`.
#' @param fixed_col Character scalar giving the fixed-time column. Defaults to
#'   `"fixed_time"`.
#' @param lubridate_fun Function used to convert input time columns to `POSIXct`.
#'   Defaults to [lubridate::as_datetime].
#' @param group_col Character scalar giving the grouping/site column. Defaults to
#'   `"group_id"`. Required if ordering constraints are supplied.
#' @param sample_col Character scalar giving the specimen ID column. Defaults to
#'   `"sample_id"`. Required if ordering constraints are supplied.
#' @param older_col Character scalar giving the column containing IDs of
#'   specimens older than the focal row. Defaults to `"older_ids"`.
#' @param younger_col Character scalar giving the column containing IDs of
#'   specimens younger than the focal row. Defaults to `"younger_ids"`.
#' @param id_sep Character scalar used to split character constraint columns into
#'   multiple IDs. Defaults to `","`. Ignored for list-columns.
#' @param max_iter Integer scalar. Maximum number of rejection-sampling attempts.
#'   Defaults to `10000`.
#' @param return_constraints Logical. If `FALSE`, return only the sampled times.
#'   If `TRUE`, return a list containing sampled times, the constraint table, and
#'   the number of iterations used.
#'
#' @return
#' If `return_constraints = FALSE`, a named `POSIXct` vector of sampled times,
#' one per row of `data`, in the original row order. Names are of the form
#' `group_id::sample_id` when grouping and sample columns are available;
#' otherwise row numbers are used.
#'
#' If `return_constraints = TRUE`, a list with elements:
#'
#' * `sampled_times`: the named `POSIXct` vector;
#' * `constraints`: the derived constraint table;
#' * `iterations`: the number of sampling attempts used.
#'
#' @seealso [build_time_constraint_table()], [check_time_constraints()]
#'
#' @examples
#' if (requireNamespace("lubridate", quietly = TRUE)) {
#'   x <- data.frame(
#'     group_id = "site-a",
#'     sample_id = c("A", "B", "X"),
#'     fixed_time = lubridate::as_datetime(c(
#'       "2000-01-01", "1900-01-01", "1950-01-01"
#'     )),
#'     I(list(c("B"), character(0), character(0))),
#'     I(list(character(0), character(0), c("B")))
#'   )
#'   names(x)[4:5] <- c("older_ids", "younger_ids")
#'   sample_time_uncertainty(x)
#' }
#'
#' @export
sample_time_uncertainty <- function(data,
                                    trnorm_cols = c("mean_time", "sd_time"),
                                    trnorm_n_sd = 2,
                                    sd_time_units = NULL,
                                    unif_cols = c("oldest_time", "youngest_time"),
                                    fixed_col = "fixed_time",
                                    lubridate_fun = lubridate::as_datetime,
                                    group_col = "group_id",
                                    sample_col = "sample_id",
                                    older_col = "older_ids",
                                    younger_col = "younger_ids",
                                    id_sep = ",",
                                    max_iter = 10000,
                                    return_constraints = FALSE) {
  if (!inherits(data, c("data.frame", "sf"))) {
    stop("`data` should be a data frame or an sf object.", call. = FALSE)
  }

  if (inherits(data, "sf")) {
    data <- sf::st_drop_geometry(data)
  }
  data <- as.data.frame(data)

  if (!is.numeric(trnorm_n_sd) || length(trnorm_n_sd) != 1L ||
      is.na(trnorm_n_sd) || trnorm_n_sd <= 0) {
    stop("`trnorm_n_sd` must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1L ||
      is.na(max_iter) || max_iter < 1) {
    stop("`max_iter` must be a positive integer-like scalar.", call. = FALSE)
  }
  max_iter <- as.integer(max_iter)

  data <- prepare_time_data(
    data = data,
    trnorm_cols = trnorm_cols,
    unif_cols = unif_cols,
    fixed_col = fixed_col,
    lubridate_fun = lubridate_fun,
    sd_time_units = sd_time_units
  )

  constraints <- build_time_constraint_table(
    data = data,
    group_col = group_col,
    sample_col = sample_col,
    older_col = older_col,
    younger_col = younger_col,
    id_sep = id_sep
  )

  validate_time_constraints(
    data = data,
    constraints = constraints,
    group_col = group_col,
    sample_col = sample_col
  )

  check_time_constraint_cycles(constraints)

  out_names <- make_sample_keys(data, group_col = group_col, sample_col = sample_col)

  for (iter in seq_len(max_iter)) {
    sampled_numeric <- vapply(
      seq_len(nrow(data)),
      function(i) sample_one_time_numeric(data[i, , drop = FALSE], trnorm_n_sd),
      numeric(1)
    )

    sampled_times <- lubridate::as_datetime(sampled_numeric, tz = "UTC")
    names(sampled_times) <- out_names

    if (check_time_constraints(sampled_times, constraints)) {
      if (return_constraints) {
        return(list(
          sampled_times = sampled_times,
          constraints = constraints,
          iterations = iter
        ))
      }
      return(sampled_times)
    }
  }

  stop(
    "Failed to sample times satisfying all ordering constraints after ",
    max_iter,
    " iterations. Check whether constraints are too tight or impossible.",
    call. = FALSE
  )
}

#' Prepare and validate time columns
#'
#' @description
#' Converts user-specified time columns to internal standard columns and checks
#' that each row contains exactly one type of time information.
#'
#' @inheritParams sample_time_uncertainty
#'
#' @return A data frame containing original columns plus standardised internal
#'   columns: `mean_time`, `sd_time_seconds`, `oldest_time`, `youngest_time`, and
#'   `fixed_time`.
#'
#' @keywords internal
#' @export
prepare_time_data <- function(data,
                              trnorm_cols = c("mean_time", "sd_time"),
                              unif_cols = c("oldest_time", "youngest_time"),
                              fixed_col = "fixed_time",
                              lubridate_fun = lubridate::as_datetime,
                              sd_time_units = NULL) {
  if (length(trnorm_cols) != 2L) {
    stop("`trnorm_cols` must have length 2.", call. = FALSE)
  }
  if (length(unif_cols) != 2L) {
    stop("`unif_cols` must have length 2.", call. = FALSE)
  }

  # Check column existence before creating/overwriting the internal standard
  # columns. This matters when the user uses the default names, e.g. `mean_time`
  # or `fixed_time`, because those columns are both input names and internal
  # standard names.
  mean_sd_exist <- all(trnorm_cols %in% names(data))
  old_young_exist <- all(unif_cols %in% names(data))
  fixed_exist <- fixed_col %in% names(data)

  mean_input <- if (mean_sd_exist) data[[trnorm_cols[1]]] else NULL
  sd_input <- if (mean_sd_exist) data[[trnorm_cols[2]]] else NULL
  oldest_input <- if (old_young_exist) data[[unif_cols[1]]] else NULL
  youngest_input <- if (old_young_exist) data[[unif_cols[2]]] else NULL
  fixed_input <- if (fixed_exist) data[[fixed_col]] else NULL

  n <- nrow(data)
  data$mean_time <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
  data$sd_time_seconds <- rep(NA_real_, n)
  data$oldest_time <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
  data$youngest_time <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
  data$fixed_time <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")

  if (mean_sd_exist) {
    data$mean_time <- convert_to_posixct(mean_input, lubridate_fun)
    data$sd_time_seconds <- sd_to_seconds(sd_input, sd_time_units)

    if (any(is.na(data$mean_time) != is.na(data$sd_time_seconds))) {
      stop(
        "For every row with a truncated-normal mean time there must also be ",
        "an sd time, and vice versa.",
        call. = FALSE
      )
    }
  } else if (!identical(trnorm_cols, c("mean_time", "sd_time"))) {
    stop("The columns specified in `trnorm_cols` do not exist in `data`.", call. = FALSE)
  }

  if (old_young_exist) {
    data$oldest_time <- convert_to_posixct(oldest_input, lubridate_fun)
    data$youngest_time <- convert_to_posixct(youngest_input, lubridate_fun)

    if (any(is.na(data$oldest_time) != is.na(data$youngest_time))) {
      stop(
        "For every row with an oldest time there must also be a youngest time, ",
        "and vice versa.",
        call. = FALSE
      )
    }

    bad_bounds <- !is.na(data$oldest_time) &
      !is.na(data$youngest_time) &
      as.numeric(data$oldest_time) > as.numeric(data$youngest_time)
    if (any(bad_bounds)) {
      stop(
        "Uniform rows must satisfy `oldest_time <= youngest_time` for ",
        "calendar/POSIXct times.",
        call. = FALSE
      )
    }
  } else if (!identical(unif_cols, c("oldest_time", "youngest_time"))) {
    stop("The columns specified in `unif_cols` do not exist in `data`.", call. = FALSE)
  }

  if (fixed_exist) {
    data$fixed_time <- convert_to_posixct(fixed_input, lubridate_fun)
  } else if (!identical(fixed_col, "fixed_time")) {
    stop("The column specified in `fixed_col` does not exist in `data`.", call. = FALSE)
  }

  has_trnorm <- !is.na(data$mean_time) & !is.na(data$sd_time_seconds)
  has_unif <- !is.na(data$oldest_time) & !is.na(data$youngest_time)
  has_fixed <- !is.na(data$fixed_time)
  n_types <- has_trnorm + has_unif + has_fixed

  if (any(n_types == 0)) {
    stop(
      "Every row must contain one type of time information: truncated normal, ",
      "uniform interval, or fixed time.",
      call. = FALSE
    )
  }
  if (any(n_types > 1)) {
    stop(
      "Each row must contain exactly one type of time information. Do not mix ",
      "truncated normal, uniform interval, and fixed time in the same row.",
      call. = FALSE
    )
  }

  data
}

#' Build pairwise time-ordering constraints
#'
#' @description
#' Converts per-row `older_ids` and `younger_ids` vectors into a pairwise
#' constraint table with one row per relationship.
#'
#' @inheritParams sample_time_uncertainty
#'
#' @return A data frame with columns `group_id`, `younger_id`, and `older_id`.
#'   Each row means: `younger_id` is younger than `older_id` within `group_id`.
#'
#' @export
build_time_constraint_table <- function(data,
                                        group_col = "group_id",
                                        sample_col = "sample_id",
                                        older_col = "older_ids",
                                        younger_col = "younger_ids",
                                        id_sep = ",") {
  has_older <- older_col %in% names(data)
  has_younger <- younger_col %in% names(data)

  if (!has_older && !has_younger) {
    return(data.frame(
      group_id = character(0),
      younger_id = character(0),
      older_id = character(0),
      stringsAsFactors = FALSE
    ))
  }

  if (!all(c(group_col, sample_col) %in% names(data))) {
    stop(
      "`group_col` and `sample_col` must exist in `data` when ordering ",
      "constraints are supplied.",
      call. = FALSE
    )
  }

  pieces <- list()

  if (has_older) {
    pieces[[length(pieces) + 1L]] <- ids_to_constraints(
      data = data,
      ids_col = older_col,
      group_col = group_col,
      sample_col = sample_col,
      id_sep = id_sep,
      direction = "focal_younger"
    )
  }

  if (has_younger) {
    pieces[[length(pieces) + 1L]] <- ids_to_constraints(
      data = data,
      ids_col = younger_col,
      group_col = group_col,
      sample_col = sample_col,
      id_sep = id_sep,
      direction = "focal_older"
    )
  }

  constraints <- do.call(rbind, pieces)
  if (is.null(constraints) || nrow(constraints) == 0L) {
    return(data.frame(
      group_id = character(0),
      younger_id = character(0),
      older_id = character(0),
      stringsAsFactors = FALSE
    ))
  }

  constraints <- unique(constraints)
  rownames(constraints) <- NULL
  constraints
}

#' Validate time-ordering constraints
#'
#' @description
#' Checks that specimen IDs are unique within groups, that all IDs referenced by
#' constraints exist in the relevant group, that there are no self-constraints,
#' and that the constraint table has the expected columns.
#'
#' @inheritParams sample_time_uncertainty
#' @param constraints A data frame produced by [build_time_constraint_table()].
#'
#' @return Invisibly returns `TRUE` if validation succeeds; otherwise throws an
#'   error.
#'
#' @export
validate_time_constraints <- function(data,
                                      constraints,
                                      group_col = "group_id",
                                      sample_col = "sample_id") {
  required <- c("group_id", "younger_id", "older_id")
  if (!all(required %in% names(constraints))) {
    stop("`constraints` must contain group_id, younger_id, and older_id.", call. = FALSE)
  }

  # Validate specimen keys even when there are no constraint rows. This catches
  # repeated IDs within a group before they can cause ambiguous name lookups.
  if (!all(c(group_col, sample_col) %in% names(data))) {
    if (nrow(constraints) == 0L) {
      return(invisible(TRUE))
    }
    stop("`group_col` and `sample_col` must exist in `data`.", call. = FALSE)
  }

  samples <- data.frame(
    group_id = as.character(data[[group_col]]),
    sample_id = as.character(data[[sample_col]]),
    stringsAsFactors = FALSE
  )

  if (any(is.na(samples$group_id) | samples$group_id == "")) {
    stop("Group IDs must not be missing or empty.", call. = FALSE)
  }
  if (any(is.na(samples$sample_id) | samples$sample_id == "")) {
    stop("Sample IDs must not be missing or empty.", call. = FALSE)
  }

  dup <- duplicated(samples) | duplicated(samples, fromLast = TRUE)
  if (any(dup)) {
    bad <- unique(paste(samples$group_id[dup], samples$sample_id[dup], sep = "::"))
    stop(
      "Sample IDs must be unique within each group. Duplicated keys: ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(constraints) == 0L) {
    return(invisible(TRUE))
  }

  if (any(constraints$younger_id == constraints$older_id)) {
    stop("Constraints cannot relate a specimen to itself.", call. = FALSE)
  }

  younger_keys <- paste(constraints$group_id, constraints$younger_id, sep = "::")
  older_keys <- paste(constraints$group_id, constraints$older_id, sep = "::")
  sample_keys <- paste(samples$group_id, samples$sample_id, sep = "::")

  missing_younger <- setdiff(younger_keys, sample_keys)
  if (length(missing_younger) > 0L) {
    stop(
      "Some younger_id values do not exist within their group: ",
      paste(missing_younger, collapse = ", "),
      call. = FALSE
    )
  }

  missing_older <- setdiff(older_keys, sample_keys)
  if (length(missing_older) > 0L) {
    stop(
      "Some older_id values do not exist within their group: ",
      paste(missing_older, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Check constraints for directed cycles
#'
#' @description
#' Detects cycles in the younger-to-older ordering graph. A cycle means that a
#' specimen would have to be both older and younger than itself, so the
#' constraints are impossible.
#'
#' @param constraints A data frame with columns `group_id`, `younger_id`, and
#'   `older_id`.
#'
#' @return Invisibly returns `TRUE` if no cycles are detected; otherwise throws
#'   an error.
#'
#' @export
check_time_constraint_cycles <- function(constraints) {
  if (nrow(constraints) == 0L) {
    return(invisible(TRUE))
  }

  groups <- unique(constraints$group_id)
  for (g in groups) {
    cg <- constraints[constraints$group_id == g, , drop = FALSE]
    vertices <- unique(c(cg$younger_id, cg$older_id))
    state <- stats::setNames(rep(0L, length(vertices)), vertices)
    adjacency <- split(cg$older_id, cg$younger_id)

    visit <- function(v) {
      if (state[[v]] == 1L) return(TRUE)
      if (state[[v]] == 2L) return(FALSE)

      state[[v]] <<- 1L
      neighbours <- adjacency[[v]]
      if (!is.null(neighbours)) {
        for (u in neighbours) {
          if (visit(u)) return(TRUE)
        }
      }
      state[[v]] <<- 2L
      FALSE
    }

    for (v in vertices) {
      if (state[[v]] == 0L && visit(v)) {
        stop(
          "Ordering constraints contain a cycle in group `", g,
          "` and cannot be satisfied.",
          call. = FALSE
        )
      }
    }
  }

  invisible(TRUE)
}

#' Check sampled times against ordering constraints
#'
#' @description
#' Tests whether a named vector of sampled times satisfies all pairwise ordering
#' constraints.
#'
#' @param sampled_times A named `POSIXct` vector. Names must be keys of the form
#'   `group_id::sample_id`.
#' @param constraints A data frame with columns `group_id`, `younger_id`, and
#'   `older_id`.
#'
#' @return Logical scalar. `TRUE` if all constraints are satisfied.
#'
#' @export
check_time_constraints <- function(sampled_times, constraints) {
  if (nrow(constraints) == 0L) {
    return(TRUE)
  }

  younger_keys <- paste(constraints$group_id, constraints$younger_id, sep = "::")
  older_keys <- paste(constraints$group_id, constraints$older_id, sep = "::")

  younger_times <- sampled_times[younger_keys]
  older_times <- sampled_times[older_keys]

  if (any(is.na(younger_times)) || any(is.na(older_times))) {
    return(FALSE)
  }

  # Calendar/POSIXct convention: older specimens have earlier dates.
  all(as.numeric(older_times) < as.numeric(younger_times))
}

# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

convert_to_posixct <- function(x, lubridate_fun) {
  out <- lubridate_fun(x)
  if (!inherits(out, "POSIXct")) {
    out <- as.POSIXct(out, tz = "UTC")
  }
  if (!inherits(out, "POSIXct")) {
    stop("A time column could not be converted to POSIXct.", call. = FALSE)
  }
  out
}

sd_to_seconds <- function(x, sd_time_units = NULL) {
  if (inherits(x, "units")) {
    return(as.numeric(units::set_units(x, "seconds", mode = "standard")))
  }

  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }

  if (is.null(sd_time_units)) {
    stop(
      "`sd_time_units` must be supplied when truncated-normal dates are used ",
      "and sd_time is not a units object.",
      call. = FALSE
    )
  }

  multiplier <- sd_unit_multiplier(sd_time_units)
  as.numeric(x) * multiplier
}

sd_unit_multiplier <- function(unit) {
  unit <- tolower(as.character(unit))
  unit <- gsub("\\s+", "", unit)

  switch(
    unit,
    "s" =, "sec" =, "secs" =, "second" =, "seconds" = 1,
    "m" =, "min" =, "mins" =, "minute" =, "minutes" = 60,
    "h" =, "hr" =, "hrs" =, "hour" =, "hours" = 3600,
    "d" =, "day" =, "days" = 86400,
    "w" =, "week" =, "weeks" = 7 * 86400,
    "month" =, "months" = 365.25 * 86400 / 12,
    "y" =, "yr" =, "yrs" =, "year" =, "years" = 365.25 * 86400,
    stop("Unsupported `sd_time_units`: ", unit, call. = FALSE)
  )
}

normalise_id_vector <- function(x, id_sep = ",") {
  if (is.null(x)) {
    return(character(0))
  }

  if (length(x) == 1L && is.na(x)) {
    return(character(0))
  }

  if (is.list(x) && length(x) == 1L) {
    x <- x[[1L]]
  }

  if (length(x) == 0L) {
    return(character(0))
  }

  if (is.character(x) && length(x) == 1L && grepl(id_sep, x, fixed = TRUE)) {
    x <- strsplit(x, id_sep, fixed = TRUE)[[1L]]
  }

  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  unique(x)
}

ids_to_constraints <- function(data,
                               ids_col,
                               group_col,
                               sample_col,
                               id_sep,
                               direction = c("focal_younger", "focal_older")) {
  direction <- match.arg(direction)
  out <- vector("list", nrow(data))

  for (i in seq_len(nrow(data))) {
    ids <- normalise_id_vector(data[[ids_col]][i], id_sep = id_sep)

    # If this is a genuine list-column, data[[ids_col]][i] is itself a list of
    # length one, and normalise_id_vector() unwraps it. For AsIs list columns in
    # data.frames, this also works.
    if (is.list(data[[ids_col]])) {
      ids <- normalise_id_vector(data[[ids_col]][[i]], id_sep = id_sep)
    }

    if (length(ids) == 0L) {
      out[[i]] <- NULL
      next
    }

    if (direction == "focal_younger") {
      out[[i]] <- data.frame(
        group_id = as.character(data[[group_col]][i]),
        younger_id = as.character(data[[sample_col]][i]),
        older_id = ids,
        stringsAsFactors = FALSE
      )
    } else {
      out[[i]] <- data.frame(
        group_id = as.character(data[[group_col]][i]),
        younger_id = ids,
        older_id = as.character(data[[sample_col]][i]),
        stringsAsFactors = FALSE
      )
    }
  }

  res <- do.call(rbind, out)
  if (is.null(res)) {
    return(data.frame(
      group_id = character(0),
      younger_id = character(0),
      older_id = character(0),
      stringsAsFactors = FALSE
    ))
  }
  rownames(res) <- NULL
  res
}

sample_one_time_numeric <- function(row, trnorm_n_sd = 2) {
  if (!is.na(row$fixed_time[1])) {
    return(as.numeric(row$fixed_time[1]))
  }

  if (!is.na(row$mean_time[1]) && !is.na(row$sd_time_seconds[1])) {
    mean_num <- as.numeric(row$mean_time[1])
    sd_num <- as.numeric(row$sd_time_seconds[1])
    lower <- mean_num - trnorm_n_sd * sd_num
    upper <- mean_num + trnorm_n_sd * sd_num

    return(truncnorm::rtruncnorm(
      n = 1,
      a = lower,
      b = upper,
      mean = mean_num,
      sd = sd_num
    ))
  }

  if (!is.na(row$oldest_time[1]) && !is.na(row$youngest_time[1])) {
    return(stats::runif(
      n = 1,
      min = as.numeric(row$oldest_time[1]),
      max = as.numeric(row$youngest_time[1])
    ))
  }

  stop("Internal error: row has no recognised time specification.", call. = FALSE)
}

make_sample_keys <- function(data, group_col = "group_id", sample_col = "sample_id") {
  if (all(c(group_col, sample_col) %in% names(data))) {
    return(paste(as.character(data[[group_col]]), as.character(data[[sample_col]]), sep = "::"))
  }
  as.character(seq_len(nrow(data)))
}
