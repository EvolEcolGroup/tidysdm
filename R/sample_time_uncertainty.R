#' Sample uncertain specimen times with graph constraints
#'
#' @description
#' `sample_time_uncertainty()` samples one date-time per specimen from fixed,
#' uniform, or truncated-normal time information. It can enforce relative
#' older/younger constraints within groups using a graph-constrained Gibbs
#' sampler. The Gibbs sampler is designed for constrained data because it
#' updates each specimen from its own distribution truncated to the interval
#' currently allowed by neighbouring specimens in the ordering graph.
#'
#' @details
#' ## Calendar-time convention
#'
#' The function assumes that dates are calendar-like [`base::POSIXct`] values. Under
#' this convention, older specimens have earlier numeric times. Therefore, if
#' specimen `A` is younger than specimen `B`, the sampled times must satisfy:
#'
#' ```
#' time[B] < time[A]
#' ```
#'
#' ## Per-row time model
#'
#' Each row must contain exactly one time model:
#'
#' * a fixed time in `fixed_col`;
#' * a uniform interval in `unif_cols`, interpreted as oldest to youngest;
#' * a truncated normal defined by `trnorm_cols`.
#'
#' ## Relative ordering constraints
#'
#' Constraints are supplied as row-wise vectors of specimen IDs:
#'
#' * `older_col`: IDs of specimens older than the focal row;
#' * `younger_col`: IDs of specimens younger than the focal row.
#'
#' These columns may either be list-columns of character vectors or character
#' columns containing multiple IDs separated by `id_sep`. IDs are resolved within
#' `group_col`, so the same `sample_id` can be reused in different groups.
#'
#' Internally, the row-wise vectors are converted to a pairwise edge table with
#' columns `group_id`, `younger_id`, and `older_id`.
#'
#' ## Exclusion windows
#'
#' Optional `exclude_cols` define a forbidden interval for each row. If supplied,
#' the sampler removes the interval `exclude_start <= time <= exclude_end` from
#' that specimen's support. This applies to fixed, uniform, and truncated-normal
#' rows. A fixed date inside its own exclusion window is invalid.
#'
#' ## Sampling methods
#'
#' `method = "auto"` uses independent sampling when no constraints are present and
#' Gibbs sampling when constraints are present. No global-discard sampler is
#' exposed because that approach scales poorly for constrained datasets.
#'
#' @param data A data frame or an `sf` object. If `sf`, geometry is dropped.
#' @param trnorm_cols Character vector of length 2 giving mean-time and sd-time
#'   columns for truncated-normal rows.
#' @param trnorm_n_sd Number of standard deviations defining the original support
#'   of truncated-normal rows.
#' @param sd_time_units Units for numeric `sd_time`, e.g. `"days"` or `"years"`.
#' @param unif_cols Character vector of length 2 giving oldest and youngest
#'   bounds for uniform rows.
#' @param fixed_col Column containing fixed times.
#' @param lubridate_fun Function used to convert input date columns to `POSIXct`.
#' @param group_col Group/site column used to resolve specimen IDs.
#' @param sample_col Specimen ID column.
#' @param older_col Column containing IDs older than the focal specimen.
#' @param younger_col Column containing IDs younger than the focal specimen.
#' @param exclude_cols Optional character vector of length 2 giving the start and
#'   end columns of a forbidden time window.
#' @param id_sep Separator used when constraint IDs are stored as character
#'   strings rather than list-columns.
#' @param method One of `"auto"`, `"gibbs"`, or `"independent"`.
#' @param n_iter Number of Gibbs iterations.
#' @param burnin Number of initial Gibbs iterations to discard when retaining
#'   draws.
#' @param thin Retain every `thin`-th post-burn-in Gibbs draw if
#'   `return_draws = TRUE`.
#' @param eps Strict-ordering gap in seconds used by the Gibbs conditional bounds.
#' @param return_constraints If `TRUE`, return a list including the constraint
#'   table.
#' @param return_draws If `TRUE`, return retained Gibbs draws in long format.
#'
#' @return A named `POSIXct` vector unless `return_constraints` or `return_draws`
#'   is `TRUE`, in which case a list is returned.
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
                                    exclude_cols = c("exclude_start", "exclude_end"),
                                    id_sep = ",",
                                    method = c("auto", "gibbs", "independent"),
                                    n_iter = 2000,
                                    burnin = 500,
                                    thin = 1,
                                    eps = 1e-6,
                                    return_constraints = FALSE,
                                    return_draws = FALSE) {
  # Input class validation is done before any data transformation so that errors
  # refer to the user's object rather than to an internal intermediate object.
  if (!inherits(data, c("data.frame", "sf"))) {
    stop("`data` should be a data frame or an sf object.", call. = FALSE)
  }
  method <- match.arg(method)

  # Geometry is irrelevant for temporal sampling, so remove it immediately while
  # preserving all attribute columns.
  if (inherits(data, "sf")) data <- sf::st_drop_geometry(data)
  data <- as.data.frame(data)

  # Scalar argument checks are intentionally strict: these arguments control loop
  # lengths or support widths, so silent recycling would be dangerous.
  check_positive_scalar(trnorm_n_sd, "trnorm_n_sd")
  check_positive_scalar(n_iter, "n_iter")
  check_nonnegative_scalar(burnin, "burnin")
  check_positive_scalar(thin, "thin")
  check_nonnegative_scalar(eps, "eps")

  # Standardise all possible date columns to internal columns with known names.
  # This lets the sampling code below avoid repeated non-standard evaluation.
  data <- prepare_time_data(data, trnorm_cols, unif_cols, fixed_col,
                            lubridate_fun, sd_time_units)
  data <- prepare_exclusion_data(data, exclude_cols, lubridate_fun)

  # Convert row-wise vector constraints to a simple directed edge table, validate
  # all IDs, and reject impossible cycles before attempting to sample.
  constraints <- build_time_constraint_table(data, group_col, sample_col,
                                             older_col, younger_col, id_sep)
  validate_time_constraints(data, constraints, group_col, sample_col)
  check_time_constraint_cycles(constraints)

  sample_names <- make_sample_keys(data, group_col, sample_col)

  # `auto` is the recommended user-facing mode: independent when possible, Gibbs
  # when relative ordering constraints make independent sampling invalid.
  if (method == "auto") {
    method <- if (nrow(constraints) > 0) "gibbs" else "independent"
  }

  if (method == "independent") {
    if (nrow(constraints) > 0) {
      stop("`method = 'independent'` cannot be used when constraints are present.", call. = FALSE)
    }
    sampled <- vapply(seq_len(nrow(data)), function(i) {
      sample_one_time_numeric_excluding(data[i, , drop = FALSE], trnorm_n_sd)
    }, numeric(1))
    sampled <- lubridate::as_datetime(sampled, tz = "UTC")
    names(sampled) <- sample_names
    if (return_constraints) return(list(sampled_times = sampled, constraints = constraints, iterations = 1L))
    return(sampled)
  }

  # Graph-constrained Gibbs sampler. Each update samples one specimen from its
  # own distribution truncated to the currently valid interval implied by its
  # older and younger neighbours. This avoids throwing away whole-vector draws.
  gibbs <- sample_times_gibbs(data, constraints, sample_names, trnorm_n_sd,
                              as.integer(n_iter), as.integer(burnin),
                              as.integer(thin), eps, return_draws)
  if (return_constraints || return_draws) {
    out <- list(sampled_times = gibbs$sampled_times,
                constraints = constraints,
                iterations = gibbs$iterations)
    if (return_draws) out$draws <- gibbs$draws
    return(out)
  }
  gibbs$sampled_times
}

#' Prepare and validate time columns
#'
#' @description Standardises user-facing time columns to internal columns used by
#' the sampler. Exactly one time model must be present per row.
#' @inheritParams sample_time_uncertainty
#' @return `data` with standardised internal time columns.
#' @export
prepare_time_data <- function(data,
                              trnorm_cols = c("mean_time", "sd_time"),
                              unif_cols = c("oldest_time", "youngest_time"),
                              fixed_col = "fixed_time",
                              lubridate_fun = lubridate::as_datetime,
                              sd_time_units = NULL) {
  if (length(trnorm_cols) != 2L) stop("`trnorm_cols` must have length 2.", call. = FALSE)
  if (length(unif_cols) != 2L) stop("`unif_cols` must have length 2.", call. = FALSE)

  # Column existence is checked before creating internal columns, because default
  # input names can be the same as internal names, e.g. `fixed_time`.
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
      stop("For every row with a truncated-normal mean time there must also be an sd time, and vice versa.", call. = FALSE)
    }
  } else if (!identical(trnorm_cols, c("mean_time", "sd_time"))) {
    stop("The columns specified in `trnorm_cols` do not exist in `data`.", call. = FALSE)
  }

  if (old_young_exist) {
    data$oldest_time <- convert_to_posixct(oldest_input, lubridate_fun)
    data$youngest_time <- convert_to_posixct(youngest_input, lubridate_fun)
    if (any(is.na(data$oldest_time) != is.na(data$youngest_time))) {
      stop("For every row with an oldest time there must also be a youngest time, and vice versa.", call. = FALSE)
    }
    if (any(!is.na(data$oldest_time) & !is.na(data$youngest_time) &
            as.numeric(data$oldest_time) > as.numeric(data$youngest_time))) {
      stop("Uniform rows must satisfy `oldest_time <= youngest_time` for calendar/POSIXct times.", call. = FALSE)
    }
  } else if (!identical(unif_cols, c("oldest_time", "youngest_time"))) {
    stop("The columns specified in `unif_cols` do not exist in `data`.", call. = FALSE)
  }

  if (fixed_exist) {
    data$fixed_time <- convert_to_posixct(fixed_input, lubridate_fun)
  } else if (!identical(fixed_col, "fixed_time")) {
    stop("The column specified in `fixed_col` does not exist in `data`.", call. = FALSE)
  }

  n_types <- (!is.na(data$mean_time) & !is.na(data$sd_time_seconds)) +
    (!is.na(data$oldest_time) & !is.na(data$youngest_time)) +
    (!is.na(data$fixed_time))
  if (any(n_types == 0)) stop("Every row must contain one type of time information: truncated normal, uniform interval, or fixed time.", call. = FALSE)
  if (any(n_types > 1)) stop("Each row must contain exactly one type of time information. Do not mix truncated normal, uniform interval, and fixed time in the same row.", call. = FALSE)
  data
}

#' Prepare optional exclusion-window columns
#'
#' @description Converts optional exclusion-window columns to internal POSIXct
#' columns. If one exclusion column exists, both must exist.
#' @inheritParams sample_time_uncertainty
#' @return `data` with `exclude_start_time` and `exclude_end_time`.
#' @export
prepare_exclusion_data <- function(data,
                                   exclude_cols = c("exclude_start", "exclude_end"),
                                   lubridate_fun = lubridate::as_datetime) {
  if (length(exclude_cols) != 2L) stop("`exclude_cols` must have length 2.", call. = FALSE)
  n <- nrow(data)
  has_excl <- all(exclude_cols %in% names(data))
  one_excl <- any(exclude_cols %in% names(data))
  start_input <- if (has_excl) data[[exclude_cols[1]]] else NULL
  end_input <- if (has_excl) data[[exclude_cols[2]]] else NULL
  data$exclude_start_time <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
  data$exclude_end_time <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
  if (has_excl) {
    data$exclude_start_time <- convert_to_posixct(start_input, lubridate_fun)
    data$exclude_end_time <- convert_to_posixct(end_input, lubridate_fun)
    if (any(is.na(data$exclude_start_time) != is.na(data$exclude_end_time))) {
      stop("For every row with an exclusion-window start there must also be an exclusion-window end, and vice versa.", call. = FALSE)
    }
    bad <- !is.na(data$exclude_start_time) & !is.na(data$exclude_end_time) &
      as.numeric(data$exclude_start_time) > as.numeric(data$exclude_end_time)
    if (any(bad)) stop("Exclusion windows must satisfy `exclude_start <= exclude_end`.", call. = FALSE)
  } else if (one_excl || !identical(exclude_cols, c("exclude_start", "exclude_end"))) {
    stop("Both columns specified in `exclude_cols` must exist in `data`, or neither should be supplied.", call. = FALSE)
  }
  data
}

#' Build pairwise time-ordering constraints
#'
#' @description Converts row-wise `older_ids` and `younger_ids` into a directed
#' edge table. Each row of the returned table means `younger_id` is younger than
#' `older_id` within `group_id`.
#' @inheritParams sample_time_uncertainty
#' @return A data frame with columns `group_id`, `younger_id`, and `older_id`.
#' @export
build_time_constraint_table <- function(data,
                                        group_col = "group_id",
                                        sample_col = "sample_id",
                                        older_col = "older_ids",
                                        younger_col = "younger_ids",
                                        id_sep = ",") {
  has_older <- older_col %in% names(data)
  has_younger <- younger_col %in% names(data)
  empty <- data.frame(group_id = character(0), younger_id = character(0), older_id = character(0), stringsAsFactors = FALSE)
  if (!has_older && !has_younger) return(empty)
  if (!all(c(group_col, sample_col) %in% names(data))) {
    stop("`group_col` and `sample_col` must exist in `data` when ordering constraints are supplied.", call. = FALSE)
  }
  pieces <- list()
  if (has_older) pieces[[length(pieces) + 1L]] <- ids_to_constraints(data, older_col, group_col, sample_col, id_sep, "focal_younger")
  if (has_younger) pieces[[length(pieces) + 1L]] <- ids_to_constraints(data, younger_col, group_col, sample_col, id_sep, "focal_older")
  constraints <- do.call(rbind, pieces)
  if (is.null(constraints) || nrow(constraints) == 0L) return(empty)
  rownames(constraints) <- NULL
  unique(constraints)
}

#' Validate time-ordering constraints
#'
#' @description Checks unique group-local sample IDs, missing references, and
#' self-constraints.
#' @inheritParams sample_time_uncertainty
#' @param constraints A pairwise constraint table.
#' @return Invisibly returns `TRUE` or raises an error.
#' @export
validate_time_constraints <- function(data,
                                      constraints,
                                      group_col = "group_id",
                                      sample_col = "sample_id") {
  req <- c("group_id", "younger_id", "older_id")
  if (!all(req %in% names(constraints))) stop("`constraints` must contain group_id, younger_id, and older_id.", call. = FALSE)
  if (!all(c(group_col, sample_col) %in% names(data))) {
    if (nrow(constraints) == 0L) return(invisible(TRUE))
    stop("`group_col` and `sample_col` must exist in `data`.", call. = FALSE)
  }
  samples <- data.frame(group_id = as.character(data[[group_col]]), sample_id = as.character(data[[sample_col]]), stringsAsFactors = FALSE)
  if (any(is.na(samples$group_id) | samples$group_id == "")) stop("Group IDs must not be missing or empty.", call. = FALSE)
  if (any(is.na(samples$sample_id) | samples$sample_id == "")) stop("Sample IDs must not be missing or empty.", call. = FALSE)
  dup <- duplicated(samples) | duplicated(samples, fromLast = TRUE)
  if (any(dup)) stop("Sample IDs must be unique within each group. Duplicated keys: ", paste(unique(paste(samples$group_id[dup], samples$sample_id[dup], sep = "::")), collapse = ", "), call. = FALSE)
  if (nrow(constraints) == 0L) return(invisible(TRUE))
  if (any(constraints$younger_id == constraints$older_id)) stop("Constraints cannot relate a specimen to itself.", call. = FALSE)
  sample_keys <- paste(samples$group_id, samples$sample_id, sep = "::")
  yk <- paste(constraints$group_id, constraints$younger_id, sep = "::")
  ok <- paste(constraints$group_id, constraints$older_id, sep = "::")
  my <- setdiff(yk, sample_keys)
  mo <- setdiff(ok, sample_keys)
  if (length(my) > 0L) stop("Some younger_id values do not exist within their group: ", paste(my, collapse = ", "), call. = FALSE)
  if (length(mo) > 0L) stop("Some older_id values do not exist within their group: ", paste(mo, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

#' Check constraints for directed cycles
#'
#' @description Detects impossible cycles in each group-specific DAG.
#' @param constraints Pairwise constraint table.
#' @return Invisibly returns `TRUE` or raises an error.
#' @export
check_time_constraint_cycles <- function(constraints) {
  if (nrow(constraints) == 0L) return(invisible(TRUE))
  for (g in unique(constraints$group_id)) {
    cg <- constraints[constraints$group_id == g, , drop = FALSE]
    vertices <- unique(c(cg$younger_id, cg$older_id))
    state <- stats::setNames(rep(0L, length(vertices)), vertices)
    adjacency <- split(cg$older_id, cg$younger_id)
    visit <- function(v) {
      if (state[[v]] == 1L) return(TRUE)
      if (state[[v]] == 2L) return(FALSE)
      state[[v]] <<- 1L
      for (u in adjacency[[v]]) if (visit(u)) return(TRUE)
      state[[v]] <<- 2L
      FALSE
    }
    for (v in vertices) if (state[[v]] == 0L && visit(v)) stop("Ordering constraints contain a cycle in group `", g, "` and cannot be satisfied.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Check sampled times against ordering constraints
#'
#' @description Tests whether a named POSIXct vector satisfies all constraints.
#' @param sampled_times Named POSIXct vector with names `group_id::sample_id`.
#' @param constraints Pairwise constraint table.
#' @return Logical scalar.
#' @export
check_time_constraints <- function(sampled_times, constraints) {
  if (nrow(constraints) == 0L) return(TRUE)
  yk <- paste(constraints$group_id, constraints$younger_id, sep = "::")
  ok <- paste(constraints$group_id, constraints$older_id, sep = "::")
  yt <- sampled_times[yk]
  ot <- sampled_times[ok]
  if (any(is.na(yt)) || any(is.na(ot))) return(FALSE)
  all(as.numeric(ot) < as.numeric(yt))
}

# -----------------------------------------------------------------------------
# Gibbs sampler and support/exclusion helpers
# -----------------------------------------------------------------------------

sample_times_gibbs <- function(data, constraints, sample_names, trnorm_n_sd,
                               n_iter, burnin, thin, eps, return_draws) {
  # Prior bounds represent only each row's own dating information. Graph bounds
  # are layered on top through bound propagation and conditional Gibbs updates.
  bounds <- compute_prior_bounds(data, trnorm_n_sd)
  graph <- build_graph_indices(sample_names, constraints)
  prop <- propagate_bounds(bounds$lower, bounds$upper, graph, eps)
  tcur <- initialise_feasible_times(data, prop$lower, prop$upper, graph, eps)
  names(tcur) <- sample_names
  if (!check_time_constraints(lubridate::as_datetime(tcur, tz = "UTC"), constraints)) {
    stop("Could not initialise a feasible state for the Gibbs sampler.", call. = FALSE)
  }

  keep_iter <- which(seq_len(n_iter) > burnin & ((seq_len(n_iter) - burnin) %% thin == 0L))
  keep <- vector("list", length(keep_iter))
  keep_pos <- 0L
  update_order <- seq_along(tcur)

  for (iter in seq_len(n_iter)) {
    # Random-scan-without-replacement updates reduce deterministic sweep artefacts.
    update_order <- sample(update_order, length(update_order), replace = FALSE)
    for (i in update_order) {
      if (!is.na(data$fixed_time[i])) next
      lo <- bounds$lower[i]
      hi <- bounds$upper[i]
      older <- graph$older_neighbours[[i]]
      younger <- graph$younger_neighbours[[i]]
      if (length(older) > 0L) lo <- max(lo, tcur[older] + eps, na.rm = TRUE)
      if (length(younger) > 0L) hi <- min(hi, tcur[younger] - eps, na.rm = TRUE)
      if (!is.finite(lo) || !is.finite(hi) || lo >= hi) {
        stop("Gibbs sampler reached an infeasible conditional interval.", call. = FALSE)
      }
      tcur[i] <- sample_numeric_from_row_interval(data[i, , drop = FALSE], lo, hi)
    }
    if (iter %in% keep_iter) {
      keep_pos <- keep_pos + 1L
      keep[[keep_pos]] <- tcur
    }
  }

  sampled <- lubridate::as_datetime(tcur, tz = "UTC")
  names(sampled) <- sample_names
  out <- list(sampled_times = sampled, iterations = n_iter)
  if (return_draws) {
    if (length(keep) == 0L) {
      draws <- data.frame(iteration = integer(0), sample_key = character(0), sampled_time = as.POSIXct(character(0)), stringsAsFactors = FALSE)
    } else {
      draws <- do.call(rbind, lapply(seq_along(keep), function(j) {
        data.frame(iteration = keep_iter[j], sample_key = sample_names,
                   sampled_time = lubridate::as_datetime(keep[[j]], tz = "UTC"),
                   stringsAsFactors = FALSE)
      }))
    }
    out$draws <- draws
  }
  out
}

compute_prior_bounds <- function(data, trnorm_n_sd) {
  n <- nrow(data)
  lo <- hi <- rep(NA_real_, n)
  fixed <- !is.na(data$fixed_time)
  lo[fixed] <- hi[fixed] <- as.numeric(data$fixed_time[fixed])
  unif <- !is.na(data$oldest_time) & !is.na(data$youngest_time)
  lo[unif] <- as.numeric(data$oldest_time[unif])
  hi[unif] <- as.numeric(data$youngest_time[unif])
  norm <- !is.na(data$mean_time) & !is.na(data$sd_time_seconds)
  lo[norm] <- as.numeric(data$mean_time[norm]) - trnorm_n_sd * data$sd_time_seconds[norm]
  hi[norm] <- as.numeric(data$mean_time[norm]) + trnorm_n_sd * data$sd_time_seconds[norm]
  if (any(is.na(lo) | is.na(hi))) stop("Internal error: missing prior bounds.", call. = FALSE)
  list(lower = lo, upper = hi)
}

build_graph_indices <- function(sample_names, constraints) {
  # Four adjacency lists are maintained because different parts of the algorithm
  # need different graph traversals: topological sorting, bound propagation, and
  # conditional Gibbs bounds.
  n <- length(sample_names)
  old_to_young <- vector("list", n); young_to_old <- vector("list", n)
  older_neighbours <- vector("list", n); younger_neighbours <- vector("list", n)
  for (i in seq_len(n)) old_to_young[[i]] <- young_to_old[[i]] <- older_neighbours[[i]] <- younger_neighbours[[i]] <- integer(0)
  if (nrow(constraints) > 0L) {
    yk <- paste(constraints$group_id, constraints$younger_id, sep = "::")
    ok <- paste(constraints$group_id, constraints$older_id, sep = "::")
    yi <- match(yk, sample_names); oi <- match(ok, sample_names)
    for (e in seq_along(yi)) {
      old_to_young[[oi[e]]] <- unique(c(old_to_young[[oi[e]]], yi[e]))
      young_to_old[[yi[e]]] <- unique(c(young_to_old[[yi[e]]], oi[e]))
      older_neighbours[[yi[e]]] <- unique(c(older_neighbours[[yi[e]]], oi[e]))
      younger_neighbours[[oi[e]]] <- unique(c(younger_neighbours[[oi[e]]], yi[e]))
    }
  }
  order <- topo_order_old_to_young(n, old_to_young)
  list(old_to_young = old_to_young, young_to_old = young_to_old,
       older_neighbours = older_neighbours, younger_neighbours = younger_neighbours,
       topo_order = order)
}

topo_order_old_to_young <- function(n, old_to_young) {
  indeg <- integer(n)
  for (i in seq_len(n)) for (j in old_to_young[[i]]) indeg[j] <- indeg[j] + 1L
  queue <- which(indeg == 0L)
  out <- integer(0)
  while (length(queue) > 0L) {
    v <- queue[1L]; queue <- queue[-1L]
    out <- c(out, v)
    for (u in old_to_young[[v]]) {
      indeg[u] <- indeg[u] - 1L
      if (indeg[u] == 0L) queue <- c(queue, u)
    }
  }
  if (length(out) != n) stop("Ordering constraints contain a cycle and cannot be topologically sorted.", call. = FALSE)
  out
}

propagate_bounds <- function(lower, upper, graph, eps) {
  # Forward pass: if old -> young, the young lower bound must be later than the
  # old lower bound. Reverse pass: the old upper bound must be earlier than the
  # young upper bound. This catches many impossible configurations before MCMC.
  lo <- lower; hi <- upper
  for (v in graph$topo_order) {
    for (u in graph$old_to_young[[v]]) lo[u] <- max(lo[u], lo[v] + eps)
  }
  for (v in rev(graph$topo_order)) {
    for (p in graph$young_to_old[[v]]) hi[p] <- min(hi[p], hi[v] - eps)
  }
  if (any(lo > hi)) stop("The ordering constraints and time supports are mutually incompatible.", call. = FALSE)
  list(lower = lo, upper = hi)
}

initialise_feasible_times <- function(data, lower, upper, graph, eps) {
  # Initialise deterministically from oldest to youngest. Choosing the first
  # allowed value keeps the initial state simple and reproducible; subsequent
  # Gibbs iterations randomise the state.
  tcur <- rep(NA_real_, nrow(data))
  for (i in graph$topo_order) {
    if (!is.na(data$fixed_time[i])) {
      val <- as.numeric(data$fixed_time[i])
      if (val < lower[i] || val > upper[i]) stop("A fixed date is incompatible with the ordering constraints.", call. = FALSE)
      if (is_excluded_value(val, data$exclude_start_time[i], data$exclude_end_time[i])) stop("A fixed date falls inside its excluded time window.", call. = FALSE)
      tcur[i] <- val
    } else {
      lo <- lower[i]
      older <- graph$older_neighbours[[i]]
      if (length(older) > 0L && any(!is.na(tcur[older]))) lo <- max(lo, tcur[older] + eps, na.rm = TRUE)
      hi <- upper[i]
      val <- first_allowed_value(lo, hi, data$exclude_start_time[i], data$exclude_end_time[i], eps)
      if (is.na(val)) stop("Could not initialise a non-excluded feasible time.", call. = FALSE)
      tcur[i] <- val
    }
  }
  tcur
}

sample_one_time_numeric_excluding <- function(row, trnorm_n_sd) {
  b <- compute_prior_bounds(row, trnorm_n_sd)
  sample_numeric_from_row_interval(row, b$lower[1], b$upper[1])
}

sample_numeric_from_row_interval <- function(row, lower, upper) {
  if (!is.na(row$fixed_time[1])) {
    val <- as.numeric(row$fixed_time[1])
    if (val < lower || val > upper) stop("A fixed date is outside its allowed interval.", call. = FALSE)
    if (is_excluded_value(val, row$exclude_start_time[1], row$exclude_end_time[1])) stop("A fixed date falls inside its excluded time window.", call. = FALSE)
    return(val)
  }
  intervals <- allowed_intervals(lower, upper, row$exclude_start_time[1], row$exclude_end_time[1])
  if (is.null(intervals)) stop("No allowed time remains after applying constraints and exclusion window.", call. = FALSE)
  if (!is.na(row$mean_time[1]) && !is.na(row$sd_time_seconds[1])) {
    return(sample_truncnorm_from_intervals(as.numeric(row$mean_time[1]), row$sd_time_seconds[1], intervals))
  }
  sample_uniform_from_intervals(intervals)
}

allowed_intervals <- function(lower, upper, exclude_start, exclude_end) {
  # A single exclusion window can split the support into zero, one, or two
  # allowed intervals. The downstream samplers work with the resulting matrix.
  lower <- as.numeric(lower); upper <- as.numeric(upper)
  if (!is.finite(lower) || !is.finite(upper) || lower > upper) return(NULL)
  if (is.na(exclude_start) || is.na(exclude_end)) return(matrix(c(lower, upper), ncol = 2, byrow = TRUE))
  es <- as.numeric(exclude_start); ee <- as.numeric(exclude_end)
  if (ee < lower || es > upper) return(matrix(c(lower, upper), ncol = 2, byrow = TRUE))
  pieces <- list()
  if (es > lower) pieces[[length(pieces) + 1L]] <- c(lower, min(es, upper))
  if (ee < upper) pieces[[length(pieces) + 1L]] <- c(max(ee, lower), upper)
  if (length(pieces) == 0L) return(NULL)
  out <- do.call(rbind, pieces)
  out <- out[out[, 1] < out[, 2], , drop = FALSE]
  if (nrow(out) == 0L) return(NULL)
  out
}

sample_uniform_from_intervals <- function(intervals) {
  widths <- intervals[, 2] - intervals[, 1]
  idx <- sample(seq_len(nrow(intervals)), 1L, prob = widths)
  stats::runif(1L, intervals[idx, 1], intervals[idx, 2])
}

sample_truncnorm_from_intervals <- function(mean, sd, intervals) {
  # Inverse-CDF sampling avoids an external truncnorm dependency and naturally
  # handles exclusion windows by weighting each interval by its normal mass.
  if (!is.finite(sd) || sd <= 0) stop("Normal sd must be positive.", call. = FALSE)
  masses <- stats::pnorm(intervals[, 2], mean, sd) - stats::pnorm(intervals[, 1], mean, sd)
  if (all(masses <= 0) || anyNA(masses)) stop("No normal probability mass remains in the allowed interval.", call. = FALSE)
  idx <- sample(seq_len(nrow(intervals)), 1L, prob = masses)
  u <- stats::runif(1L, stats::pnorm(intervals[idx, 1], mean, sd), stats::pnorm(intervals[idx, 2], mean, sd))
  stats::qnorm(u, mean, sd)
}

first_allowed_value <- function(lower, upper, exclude_start, exclude_end, eps) {
  intervals <- allowed_intervals(lower, upper, exclude_start, exclude_end)
  if (is.null(intervals)) return(NA_real_)
  width <- intervals[1, 2] - intervals[1, 1]
  intervals[1, 1] + min(max(eps, width * 0.01), width / 2)
}

is_excluded_value <- function(value, exclude_start, exclude_end) {
  if (is.na(exclude_start) || is.na(exclude_end)) return(FALSE)
  value >= as.numeric(exclude_start) && value <= as.numeric(exclude_end)
}

# -----------------------------------------------------------------------------
# General helpers
# -----------------------------------------------------------------------------

convert_to_posixct <- function(x, lubridate_fun) {
  out <- lubridate_fun(x)
  if (!inherits(out, "POSIXct")) out <- as.POSIXct(out, tz = "UTC")
  if (!inherits(out, "POSIXct")) stop("A time column could not be converted to POSIXct.", call. = FALSE)
  out
}

sd_to_seconds <- function(x, sd_time_units = NULL) {
  if (inherits(x, "units")) return(as.numeric(units::set_units(x, "seconds", mode = "standard")))
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  if (is.null(sd_time_units)) stop("`sd_time_units` must be supplied when truncated-normal dates are used and sd_time is not a units object.", call. = FALSE)
  as.numeric(x) * sd_unit_multiplier(sd_time_units)
}

sd_unit_multiplier <- function(unit) {
  unit <- tolower(gsub("\\s+", "", as.character(unit)))
  switch(unit,
         "s" =, "sec" =, "secs" =, "second" =, "seconds" = 1,
         "m" =, "min" =, "mins" =, "minute" =, "minutes" = 60,
         "h" =, "hr" =, "hrs" =, "hour" =, "hours" = 3600,
         "d" =, "day" =, "days" = 86400,
         "w" =, "week" =, "weeks" = 7 * 86400,
         "month" =, "months" = 365.25 * 86400 / 12,
         "y" =, "yr" =, "yrs" =, "year" =, "years" = 365.25 * 86400,
         stop("Unsupported `sd_time_units`: ", unit, call. = FALSE))
}

normalise_id_vector <- function(x, id_sep = ",") {
  if (is.null(x)) return(character(0))
  if (length(x) == 1L && is.na(x)) return(character(0))
  if (is.list(x) && length(x) == 1L) x <- x[[1L]]
  if (length(x) == 0L) return(character(0))
  if (is.character(x) && length(x) == 1L && grepl(id_sep, x, fixed = TRUE)) x <- strsplit(x, id_sep, fixed = TRUE)[[1L]]
  x <- trimws(as.character(x))
  unique(x[!is.na(x) & nzchar(x)])
}

ids_to_constraints <- function(data, ids_col, group_col, sample_col, id_sep,
                               direction = c("focal_younger", "focal_older")) {
  direction <- match.arg(direction)
  out <- vector("list", nrow(data))
  for (i in seq_len(nrow(data))) {
    ids <- if (is.list(data[[ids_col]])) normalise_id_vector(data[[ids_col]][[i]], id_sep) else normalise_id_vector(data[[ids_col]][i], id_sep)
    if (length(ids) == 0L) next
    if (direction == "focal_younger") {
      out[[i]] <- data.frame(group_id = as.character(data[[group_col]][i]), younger_id = as.character(data[[sample_col]][i]), older_id = ids, stringsAsFactors = FALSE)
    } else {
      out[[i]] <- data.frame(group_id = as.character(data[[group_col]][i]), younger_id = ids, older_id = as.character(data[[sample_col]][i]), stringsAsFactors = FALSE)
    }
  }
  res <- do.call(rbind, out)
  if (is.null(res)) return(data.frame(group_id = character(0), younger_id = character(0), older_id = character(0), stringsAsFactors = FALSE))
  rownames(res) <- NULL
  res
}

make_sample_keys <- function(data, group_col = "group_id", sample_col = "sample_id") {
  if (all(c(group_col, sample_col) %in% names(data))) paste(as.character(data[[group_col]]), as.character(data[[sample_col]]), sep = "::") else as.character(seq_len(nrow(data)))
}

check_positive_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0) stop("`", name, "` must be a positive numeric scalar.", call. = FALSE)
}
check_nonnegative_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0) stop("`", name, "` must be a non-negative numeric scalar.", call. = FALSE)
}
