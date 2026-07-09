# Tests for sample_time_uncertainty_gibbs.R
#
# These tests focus on the new Gibbs sampler and exclusion-window behaviour, but
# also re-check the most important constraint-table and validation logic. They
# are annotated so future maintainers can see what each test is protecting.

dt <- function(x) lubridate::as_datetime(x, tz = "UTC")

sort_constraints <- function(x) {
  x[order(x$group_id, x$younger_id, x$older_id), , drop = FALSE]
}

# -----------------------------------------------------------------------------
# Constraint-table construction and validation
# -----------------------------------------------------------------------------

testthat::test_that("row-wise older_ids and younger_ids are converted to pairwise constraints", {
  # older_ids: focal specimen is younger than each listed ID.
  # younger_ids: each listed ID is younger than the focal specimen.
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

  testthat::expect_equal(sort_constraints(constraints), sort_constraints(expected), ignore_attr = TRUE)
})

testthat::test_that("sample IDs may repeat across groups but not within groups", {
  # Group-qualified lookup keys are group_id::sample_id, so A can appear in two
  # groups but not twice in the same group.
  dat_ok <- data.frame(
    group_id = c("s1", "s1", "s2", "s2"),
    sample_id = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
  dat_ok$older_ids <- I(list("B", character(0), "B", character(0)))
  con_ok <- build_time_constraint_table(dat_ok)
  testthat::expect_true(validate_time_constraints(dat_ok, con_ok))

  dat_bad <- data.frame(
    group_id = c("s1", "s1"),
    sample_id = c("A", "A"),
    stringsAsFactors = FALSE
  )
  dat_bad$older_ids <- I(list(character(0), character(0)))
  con_bad <- build_time_constraint_table(dat_bad)
  testthat::expect_error(validate_time_constraints(dat_bad, con_bad), "unique within each group")
})

testthat::test_that("cycles are rejected before sampling", {
  constraints <- data.frame(
    group_id = rep("s1", 3),
    younger_id = c("A", "B", "C"),
    older_id = c("B", "C", "A"),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(check_time_constraint_cycles(constraints), "cycle")
})

# -----------------------------------------------------------------------------
# Exclusion-window preparation and sampling support
# -----------------------------------------------------------------------------

testthat::test_that("exclusion windows are standardised and validated", {
  dat <- data.frame(
    fixed_time = dt("2020-01-01"),
    exclude_start = dt("2019-01-01"),
    exclude_end = dt("2019-12-31"),
    stringsAsFactors = FALSE
  )
  dat <- prepare_time_data(dat)
  out <- prepare_exclusion_data(dat)

  testthat::expect_s3_class(out$exclude_start_time, "POSIXct")
  testthat::expect_equal(as.Date(out$exclude_start_time), as.Date("2019-01-01"))

  dat_bad <- data.frame(
    fixed_time = dt("2020-01-01"),
    exclude_start = dt("2021-01-01"),
    exclude_end = dt("2020-01-01"),
    stringsAsFactors = FALSE
  )
  dat_bad <- prepare_time_data(dat_bad)
  testthat::expect_error(prepare_exclusion_data(dat_bad), "exclude_start <= exclude_end")
})

testthat::test_that("uniform independent sampling avoids the excluded window", {
  # The whole original support is 2000--2010, but 2003--2007 is forbidden.
  # Every draw must therefore fall in one of the two remaining pieces.
  set.seed(1)
  dat <- data.frame(
    group_id = "s1",
    sample_id = "A",
    oldest_time = dt("2000-01-01"),
    youngest_time = dt("2010-01-01"),
    exclude_start = dt("2003-01-01"),
    exclude_end = dt("2007-01-01"),
    stringsAsFactors = FALSE
  )

  draws <- replicate(50, sample_time_uncertainty(dat, method = "independent"))
  draws <- as.POSIXct(draws, origin = "1970-01-01", tz = "UTC")

  testthat::expect_true(all(draws < dt("2003-01-01") | draws > dt("2007-01-01")))
})

testthat::test_that("fixed dates inside an exclusion window are rejected", {
  dat <- data.frame(
    group_id = "s1",
    sample_id = "A",
    fixed_time = dt("2005-01-01"),
    exclude_start = dt("2000-01-01"),
    exclude_end = dt("2010-01-01"),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    sample_time_uncertainty(dat, method = "independent"),
    "excluded time window"
  )
})

# -----------------------------------------------------------------------------
# Gibbs sampler integration tests
# -----------------------------------------------------------------------------

testthat::test_that("auto uses Gibbs for constrained data and satisfies a simple chain", {
  # B is older than A. The two supports overlap, so naive rejection may waste
  # draws, but Gibbs samples each specimen inside the interval allowed by the
  # current graph neighbours.
  set.seed(10)
  dat <- data.frame(
    group_id = rep("s1", 2),
    sample_id = c("A", "B"),
    oldest_time = dt(c("1900-01-01", "1850-01-01")),
    youngest_time = dt(c("2000-01-01", "1950-01-01")),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list("B", character(0)))

  res <- sample_time_uncertainty(dat, n_iter = 100, burnin = 20)

  testthat::expect_s3_class(res, "POSIXct")
  testthat::expect_true(res["s1::B"] < res["s1::A"])
})

testthat::test_that("Gibbs sampler handles multiple older and younger neighbours", {
  # X is constrained between B and C/D: B older than X, while X is older than A.
  # There are multiple neighbours on both sides across the small graph.
  set.seed(11)
  dat <- data.frame(
    group_id = rep("s1", 4),
    sample_id = c("A", "B", "C", "X"),
    oldest_time = dt(rep("1800-01-01", 4)),
    youngest_time = dt(rep("2100-01-01", 4)),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list("X", character(0), character(0), c("B", "C")))

  res <- sample_time_uncertainty(dat, method = "gibbs", n_iter = 100, burnin = 20)

  testthat::expect_true(res["s1::B"] < res["s1::X"])
  testthat::expect_true(res["s1::C"] < res["s1::X"])
  testthat::expect_true(res["s1::X"] < res["s1::A"])
})

testthat::test_that("Gibbs sampler respects exclusion windows while satisfying constraints", {
  # A is younger than B. A's support excludes 1940--1960; the final A date must
  # avoid that window and still remain later than B.
  set.seed(12)
  dat <- data.frame(
    group_id = rep("s1", 2),
    sample_id = c("A", "B"),
    oldest_time = dt(c("1900-01-01", "1850-01-01")),
    youngest_time = dt(c("2000-01-01", "1930-01-01")),
    exclude_start = c(dt("1940-01-01"), as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")),
    exclude_end = c(dt("1960-01-01"), as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list("B", character(0)))

  res <- sample_time_uncertainty(dat, method = "gibbs", n_iter = 100, burnin = 20)

  testthat::expect_true(res["s1::B"] < res["s1::A"])
  testthat::expect_true(res["s1::A"] < dt("1940-01-01") | res["s1::A"] > dt("1960-01-01"))
})

testthat::test_that("Gibbs sampler returns retained draws when requested", {
  set.seed(13)
  dat <- data.frame(
    group_id = rep("s1", 2),
    sample_id = c("A", "B"),
    oldest_time = dt(c("1900-01-01", "1850-01-01")),
    youngest_time = dt(c("2000-01-01", "1950-01-01")),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list("B", character(0)))

  res <- sample_time_uncertainty(
    dat,
    method = "gibbs",
    n_iter = 10,
    burnin = 2,
    thin = 2,
    return_draws = TRUE,
    return_constraints = TRUE
  )

  # Iterations retained are 4, 6, 8, and 10. With two samples, this gives 8 rows.
  testthat::expect_equal(nrow(res$draws), 8L)
  testthat::expect_equal(sort(unique(res$draws$iteration)), c(4L, 6L, 8L, 10L))
  testthat::expect_true(check_time_constraints(res$sampled_times, res$constraints))
})

testthat::test_that("Gibbs sampler detects incompatible supports", {
  # A is declared younger than B, but A's entire support is earlier than B's.
  # Bound propagation should reject this before the Gibbs loop proceeds.
  dat <- data.frame(
    group_id = rep("s1", 2),
    sample_id = c("A", "B"),
    oldest_time = dt(c("1800-01-01", "1900-01-01")),
    youngest_time = dt(c("1810-01-01", "1910-01-01")),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list("B", character(0)))

  testthat::expect_error(
    sample_time_uncertainty(dat, method = "gibbs", n_iter = 10),
    "incompatible"
  )
})

# -----------------------------------------------------------------------------
# Truncated-normal sampling without requiring truncnorm package
# -----------------------------------------------------------------------------

testthat::test_that("truncated-normal sampling works through inverse-CDF intervals", {
  set.seed(14)
  dat <- data.frame(
    group_id = "s1",
    sample_id = "A",
    mean_time = dt("2000-01-01"),
    sd_time = 10,
    stringsAsFactors = FALSE
  )

  res <- sample_time_uncertainty(dat, sd_time_units = "days", method = "independent", trnorm_n_sd = 2)

  testthat::expect_s3_class(res, "POSIXct")
  testthat::expect_true(res >= dt("1999-12-12"))
  testthat::expect_true(res <= dt("2000-01-21"))
})
