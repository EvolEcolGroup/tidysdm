# Tests for sample_time_uncertainty.R
#
# This file deliberately tests the code in layers:
#   1. time-column preparation;
#   2. exclusion-window preparation;
#   3. constraint-table construction and validation;
#   4. low-level constraint checking;
#   5. top-level independent and Gibbs sampling behaviour.
#
# The Gibbs-specific expectations reflect the current default behaviour:
# method = "auto" uses Gibbs whenever constraints exist.

dt <- function(x) lubridate::as_datetime(x, tz = "UTC")

sort_constraints <- function(x) {
  x[order(x$group_id, x$younger_id, x$older_id), , drop = FALSE]
}

# -----------------------------------------------------------------------------
# Time preparation
# -----------------------------------------------------------------------------

test_that("prepare_time_data standardises fixed-time rows", {
  dat <- data.frame(
    group_id = "site1",
    sample_id = c("A", "B"),
    fixed_time = dt(c("2020-01-01", "2021-01-01")),
    stringsAsFactors = FALSE
  )

  out <- prepare_time_data(dat)

  expect_s3_class(out$fixed_time, "POSIXct")
  expect_equal(unname(as.Date(out$fixed_time)), as.Date(c("2020-01-01", "2021-01-01")))
  expect_true(all(is.na(out$mean_time)))
  expect_true(all(is.na(out$oldest_time)))
})

test_that("prepare_time_data standardises uniform interval rows", {
  dat <- data.frame(
    group_id = "site1",
    sample_id = c("A", "B"),
    oldest_time = dt(c("1900-01-01", "1950-01-01")),
    youngest_time = dt(c("1910-01-01", "1960-01-01")),
    stringsAsFactors = FALSE
  )

  out <- prepare_time_data(dat)

  expect_s3_class(out$oldest_time, "POSIXct")
  expect_s3_class(out$youngest_time, "POSIXct")
  expect_true(all(out$oldest_time <= out$youngest_time))
})

test_that("prepare_time_data standardises truncated-normal rows", {
  dat <- data.frame(
    group_id = "site1",
    sample_id = c("A", "B"),
    mean_time = dt(c("2000-01-01", "2001-01-01")),
    sd_time = c(10, 20),
    stringsAsFactors = FALSE
  )

  out <- prepare_time_data(dat, sd_time_units = "days")

  expect_s3_class(out$mean_time, "POSIXct")
  expect_equal(out$sd_time_seconds, c(10, 20) * 86400)
})

test_that("prepare_time_data accepts custom fixed-time column names", {
  dat <- data.frame(
    group = "site1",
    id = "A",
    date = dt("2020-01-01"),
    stringsAsFactors = FALSE
  )

  out <- prepare_time_data(dat, fixed_col = "date")

  expect_equal(as.Date(out$fixed_time), as.Date("2020-01-01"))
})

test_that("prepare_time_data rejects incomplete or ambiguous time models", {
  no_time <- data.frame(group_id = "site1", sample_id = "A", stringsAsFactors = FALSE)
  expect_error(prepare_time_data(no_time), "Every row must contain one type")

  mixed <- data.frame(
    group_id = "site1",
    sample_id = "A",
    fixed_time = dt("2020-01-01"),
    oldest_time = dt("2019-01-01"),
    youngest_time = dt("2021-01-01"),
    stringsAsFactors = FALSE
  )
  expect_error(prepare_time_data(mixed), "exactly one type")

  unpaired <- data.frame(
    group_id = "site1",
    sample_id = c("A", "B"),
    mean_time = c(dt("2000-01-01"), as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")),
    sd_time = c(NA, 10),
    stringsAsFactors = FALSE
  )
  expect_error(prepare_time_data(unpaired, sd_time_units = "days"), "mean time")
})

test_that("prepare_time_data rejects reversed uniform bounds and missing sd units", {
  bad_bounds <- data.frame(
    group_id = "site1",
    sample_id = "A",
    oldest_time = dt("2021-01-01"),
    youngest_time = dt("2020-01-01"),
    stringsAsFactors = FALSE
  )
  expect_error(prepare_time_data(bad_bounds), "oldest_time <= youngest_time")

  no_units <- data.frame(
    group_id = "site1",
    sample_id = "A",
    mean_time = dt("2000-01-01"),
    sd_time = 10,
    stringsAsFactors = FALSE
  )
  expect_error(prepare_time_data(no_units), "sd_time_units")
})

# -----------------------------------------------------------------------------
# Exclusion windows
# -----------------------------------------------------------------------------

test_that("prepare_exclusion_data standardises valid exclusion windows", {
  dat <- prepare_time_data(data.frame(
    fixed_time = dt("2020-01-01"),
    exclude_oldest = dt("2010-01-01"),
    exclude_youngest = dt("2011-01-01"),
    stringsAsFactors = FALSE
  ))

  out <- prepare_exclusion_data(dat)

  expect_s3_class(out$exclude_oldest_time, "POSIXct")
  expect_equal(as.Date(out$exclude_oldest_time), as.Date("2010-01-01"))
})

test_that("prepare_exclusion_data rejects malformed exclusion windows", {
  reversed <- prepare_time_data(data.frame(
    fixed_time = dt("2020-01-01"),
    exclude_oldest = dt("2021-01-01"),
    exclude_youngest = dt("2020-01-01"),
    stringsAsFactors = FALSE
  ))
  expect_error(prepare_exclusion_data(reversed), "exclude_oldest <= exclude_youngest")

  one_col <- prepare_time_data(data.frame(
    fixed_time = dt("2020-01-01"),
    exclude_oldest = dt("2021-01-01"),
    stringsAsFactors = FALSE
  ))
  expect_error(prepare_exclusion_data(one_col), "Both columns")
})

# -----------------------------------------------------------------------------
# Constraint construction and validation
# -----------------------------------------------------------------------------

test_that("build_time_constraint_table handles list-column older and younger IDs", {
  dat <- data.frame(
    group_id = rep("site1", 4),
    sample_id = c("A", "B", "C", "X"),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list(c("B"), c("C"), character(0), c("D", "E")))
  dat$younger_ids <- I(list(character(0), character(0), c("B"), c("B", "A")))

  constraints <- build_time_constraint_table(dat)

  expected <- data.frame(
    group_id = rep("site1", 6),
    younger_id = c("A", "B", "X", "X", "B", "A"),
    older_id = c("B", "C", "D", "E", "X", "X"),
    stringsAsFactors = FALSE
  )

  expect_equal(sort_constraints(constraints), sort_constraints(expected), ignore_attr = TRUE)
})

test_that("build_time_constraint_table handles semicolon-separated IDs and drops duplicates", {
  dat <- data.frame(
    group_id = "site1",
    sample_id = "X",
    older_ids = "D; E; D",
    younger_ids = "A;B;A",
    stringsAsFactors = FALSE
  )

  constraints <- build_time_constraint_table(dat)

  expected <- data.frame(
    group_id = rep("site1", 4),
    younger_id = c("X", "X", "A", "B"),
    older_id = c("D", "E", "X", "X"),
    stringsAsFactors = FALSE
  )

  expect_equal(sort_constraints(constraints), sort_constraints(expected), ignore_attr = TRUE)
})

test_that("build_time_constraint_table rejects non-semicolon-separated IDs", {
  dat <- data.frame(
    group_id = "site1",
    sample_id = "X",
    older_ids = "D, E",
    younger_ids = "A, B",
    stringsAsFactors = FALSE
  )
  
  constraints <- build_time_constraint_table(dat)
  
  expected <- data.frame(
    group_id = rep("site1", 4),
    younger_id = c("X, X, A, B"),
    older_id = c("D, E, X, X"),
    stringsAsFactors = FALSE
  )
  
  expect_false(identical(sort_constraints(constraints), sort_constraints(expected)))
})

test_that("validate_time_constraints allows repeated IDs across groups and rejects duplicates within groups", {
  dat_ok <- data.frame(
    group_id = c("site1", "site1", "site2", "site2"),
    sample_id = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
  dat_ok$older_ids <- I(list("B", character(0), "B", character(0)))
  expect_true(validate_time_constraints(dat_ok, build_time_constraint_table(dat_ok)))

  dat_bad <- data.frame(group_id = c("site1", "site1"), sample_id = c("A", "A"), stringsAsFactors = FALSE)
  dat_bad$older_ids <- I(list(character(0), character(0)))
  expect_error(validate_time_constraints(dat_bad, build_time_constraint_table(dat_bad)), "unique within each group")
})

test_that("validate_time_constraints rejects missing references and self-constraints", {
  missing_old <- data.frame(group_id = "site1", sample_id = "A", stringsAsFactors = FALSE)
  missing_old$older_ids <- I(list("B"))
  expect_error(validate_time_constraints(missing_old, build_time_constraint_table(missing_old)), "older_id")

  missing_young <- data.frame(group_id = "site1", sample_id = "A", stringsAsFactors = FALSE)
  missing_young$younger_ids <- I(list("B"))
  expect_error(validate_time_constraints(missing_young, build_time_constraint_table(missing_young)), "younger_id")

  self <- data.frame(group_id = "site1", sample_id = "A", stringsAsFactors = FALSE)
  self$older_ids <- I(list("A"))
  expect_error(validate_time_constraints(self, build_time_constraint_table(self)), "itself")
})

test_that("check_time_constraint_cycles detects cycles but treats groups separately", {
  cyclic <- data.frame(
    group_id = rep("site1", 3),
    younger_id = c("A", "B", "C"),
    older_id = c("B", "C", "A"),
    stringsAsFactors = FALSE
  )
  expect_error(check_time_constraint_cycles(cyclic), "cycle")

  acyclic_by_group <- data.frame(
    group_id = c("site1", "site2"),
    younger_id = c("A", "B"),
    older_id = c("B", "A"),
    stringsAsFactors = FALSE
  )
  expect_true(check_time_constraint_cycles(acyclic_by_group))
})

# -----------------------------------------------------------------------------
# Constraint checking
# -----------------------------------------------------------------------------

test_that("check_time_constraints uses calendar direction and strict inequality", {
  constraints <- data.frame(group_id = "site1", younger_id = "A", older_id = "B", stringsAsFactors = FALSE)

  good <- dt(c("2000-01-01", "1900-01-01"))
  names(good) <- c("site1::A", "site1::B")
  expect_true(check_time_constraints(good, constraints))

  bad <- dt(c("1800-01-01", "1900-01-01"))
  names(bad) <- c("site1::A", "site1::B")
  expect_false(check_time_constraints(bad, constraints))

  equal <- dt(c("1900-01-01", "1900-01-01"))
  names(equal) <- c("site1::A", "site1::B")
  expect_false(check_time_constraints(equal, constraints))
})

# -----------------------------------------------------------------------------
# Top-level samplers
# -----------------------------------------------------------------------------

test_that("sample_time_uncertainty works without constraint columns", {
  dat <- data.frame(
    group_id = rep("site1", 2),
    sample_id = c("A", "B"),
    fixed_time = dt(c("2020-01-01", "2021-01-01")),
    stringsAsFactors = FALSE
  )

  res <- sample_time_uncertainty(dat)

  expect_equal(names(res), c("site1::A", "site1::B"))
  expect_equal(unname(as.Date(res)), as.Date(c("2020-01-01", "2021-01-01")))
})

test_that("sample_time_uncertainty returns row-number names when no group/sample columns are present", {
  dat <- data.frame(fixed_time = dt(c("2020-01-01", "2021-01-01")), stringsAsFactors = FALSE)

  res <- sample_time_uncertainty(dat)

  expect_equal(names(res), c("1", "2"))
  expect_equal(unname(as.Date(res)), as.Date(c("2020-01-01", "2021-01-01")))
})

test_that("sample_time_uncertainty returns fixed constrained samples with Gibbs", {
  # Constrained data defaults to Gibbs. Iterations therefore equal n_iter rather
  # than 1, even though all rows are fixed and the state is unchanged.
  dat <- data.frame(
    group_id = rep("site1", 3),
    sample_id = c("A", "B", "X"),
    fixed_time = dt(c("2000-01-01", "1900-01-01", "1950-01-01")),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list(character(0), character(0), character(0)))
  dat$younger_ids <- I(list(character(0), c("A", "X"), character(0)))

  res <- sample_time_uncertainty(dat, method = "gibbs",
                                 n_iter = 10, burnin = 0,
                                 return_constraints = TRUE)

  expect_s3_class(res$sampled_times, "POSIXct")
  expect_true(check_time_constraints(res$sampled_times, res$constraints))
  expect_equal(names(res$sampled_times), c("site1::A", "site1::B", "site1::X"))
  expect_equal(res$iterations, 10L)
})

test_that("sample_time_uncertainty detects impossible fixed constraints with Gibbs", {
  # A is declared younger than B, but A's fixed date is earlier. Gibbs detects
  # this immediately during bound propagation.
  dat <- data.frame(
    group_id = rep("site1", 2),
    sample_id = c("A", "B"),
    fixed_time = dt(c("1800-01-01", "1900-01-01")),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list("B", character(0)))

  expect_error(sample_time_uncertainty(dat, method = "gibbs",
                                       n_iter = 10), "incompatible")
})

test_that("Gibbs sampler satisfies uniform constraints", {
  set.seed(123)
  dat <- data.frame(
    group_id = rep("site1", 2),
    sample_id = c("A", "B"),
    oldest_time = dt(c("1990-01-01", "1900-01-01")),
    youngest_time = dt(c("2000-01-01", "1910-01-01")),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list("B", character(0)))

  res <- sample_time_uncertainty(dat, method = "gibbs",
                                 n_iter = 50, burnin = 10)

  expect_true(res["site1::B"] < res["site1::A"])
})

test_that("Gibbs sampler handles multiple older and younger neighbours", {
  set.seed(456)
  dat <- data.frame(
    group_id = rep("site1", 4),
    sample_id = c("A", "B", "C", "X"),
    oldest_time = dt(rep("1800-01-01", 4)),
    youngest_time = dt(rep("2100-01-01", 4)),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list("X", character(0), character(0), c("B", "C")))

  res <- sample_time_uncertainty(dat, method = "gibbs",
                                 n_iter = 50, burnin = 10)

  expect_true(res["site1::B"] < res["site1::X"])
  expect_true(res["site1::C"] < res["site1::X"])
  expect_true(res["site1::X"] < res["site1::A"])
})

test_that("exclusion windows are respected by independent and Gibbs sampling", {
  set.seed(789)
  independent_dat <- data.frame(
    group_id = "site1",
    sample_id = "A",
    oldest_time = dt("2000-01-01"),
    youngest_time = dt("2010-01-01"),
    exclude_oldest = dt("2003-01-01"),
    exclude_youngest = dt("2007-01-01"),
    stringsAsFactors = FALSE
  )
  independent_draws <- replicate(20,
                                 sample_time_uncertainty(independent_dat,
                                                         method = "independent"))
  independent_draws <- as.POSIXct(independent_draws,
                                  origin = "1970-01-01", tz = "UTC")
  expect_true(all(independent_draws < dt("2003-01-01") | independent_draws > dt("2007-01-01")))

  gibbs_dat <- data.frame(
    group_id = rep("site1", 2),
    sample_id = c("A", "B"),
    oldest_time = dt(c("1900-01-01", "1850-01-01")),
    youngest_time = dt(c("2000-01-01", "1930-01-01")),
    exclude_oldest = c(dt("1940-01-01"), as.POSIXct(NA,
                                                   origin = "1970-01-01",
                                                   tz = "UTC")),
    exclude_youngest = c(dt("1960-01-01"), as.POSIXct(NA,
                                                 origin = "1970-01-01",
                                                 tz = "UTC")),
    stringsAsFactors = FALSE
  )
  gibbs_dat$older_ids <- I(list("B", character(0)))
  res <- sample_time_uncertainty(gibbs_dat, method = "gibbs", n_iter = 50, burnin = 10)
  expect_true(res["site1::B"] < res["site1::A"])
  expect_true((res["site1::A"] < dt("1940-01-01")) |
                (res["site1::A"] > dt("1960-01-01")))
})

test_that("fixed dates inside exclusion windows are rejected", {
  dat <- data.frame(
    group_id = "site1",
    sample_id = "A",
    fixed_time = dt("2005-01-01"),
    exclude_oldest = dt("2000-01-01"),
    exclude_youngest = dt("2010-01-01"),
    stringsAsFactors = FALSE
  )

  expect_error(sample_time_uncertainty(dat, method = "independent"),
               "excluded time window")
})

test_that("Gibbs sampler can return retained draws", {
  set.seed(321)
  dat <- data.frame(
    group_id = rep("site1", 2),
    sample_id = c("A", "B"),
    oldest_time = dt(c("1900-01-01", "1850-01-01")),
    youngest_time = dt(c("2000-01-01", "1950-01-01")),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list("B", character(0)))

  res <- sample_time_uncertainty(dat,
    method = "gibbs", n_iter = 10, burnin = 2, thin = 2,
    return_draws = TRUE, return_constraints = TRUE
  )

  expect_equal(nrow(res$draws), 8L)
  expect_equal(sort(unique(res$draws$iteration)), c(4L, 6L, 8L, 10L))
  expect_true(check_time_constraints(res$sampled_times, res$constraints))
})

test_that("truncated-normal sampling uses inverse-CDF bounds", {
  set.seed(999)
  dat <- data.frame(
    group_id = "site1",
    sample_id = "A",
    mean_time = dt("2000-01-01"),
    sd_time = 10,
    stringsAsFactors = FALSE
  )

  res <- sample_time_uncertainty(dat,
    method = "independent",
    sd_time_units = "days", trnorm_n_sd = 2
  )

  expect_s3_class(res, "POSIXct")
  expect_true(res >= dt("1999-12-12"))
  expect_true(res <= dt("2000-01-21"))
})

test_that("custom group, sample, time, and constraint columns work", {
  dat <- data.frame(
    site = rep("s1", 2),
    specimen = c("young", "old"),
    date = dt(c("2000-01-01", "1900-01-01")),
    older = c("old", NA_character_),
    stringsAsFactors = FALSE
  )

  res <- sample_time_uncertainty(dat,
    fixed_col = "date", group_col = "site",
    sample_col = "specimen", older_col = "older",
    method = "gibbs", n_iter = 10, burnin = 0
  )

  expect_equal(names(res), c("s1::young", "s1::old"))
  expect_true(res["s1::old"] < res["s1::young"])
})
