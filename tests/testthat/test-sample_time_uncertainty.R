testthat::test_that("build_time_constraint_table handles list-column older and younger IDs", {
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

  testthat::expect_equal(
    constraints[order(constraints$younger_id, constraints$older_id), ],
    expected[order(expected$younger_id, expected$older_id), ],
    ignore_attr = TRUE
  )
})

testthat::test_that("build_time_constraint_table handles comma-separated IDs", {
  dat <- data.frame(
    group_id = "site1",
    sample_id = "X",
    older_ids = "D, E",
    younger_ids = "A,B",
    stringsAsFactors = FALSE
  )

  constraints <- build_time_constraint_table(dat)

  expected <- data.frame(
    group_id = rep("site1", 4),
    younger_id = c("X", "X", "A", "B"),
    older_id = c("D", "E", "X", "X"),
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(
    constraints[order(constraints$younger_id, constraints$older_id), ],
    expected[order(expected$younger_id, expected$older_id), ],
    ignore_attr = TRUE
  )
})

testthat::test_that("validate_time_constraints allows repeated sample IDs across groups", {
  dat <- data.frame(
    group_id = c("site1", "site1", "site2", "site2"),
    sample_id = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list("B", character(0), "B", character(0)))

  constraints <- build_time_constraint_table(dat)

  testthat::expect_true(validate_time_constraints(dat, constraints))
})

testthat::test_that("validate_time_constraints rejects duplicated IDs within groups", {
  dat <- data.frame(
    group_id = c("site1", "site1"),
    sample_id = c("A", "A"),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list(character(0), character(0)))
  constraints <- build_time_constraint_table(dat)

  testthat::expect_error(
    validate_time_constraints(dat, constraints),
    "unique within each group"
  )
})

testthat::test_that("validate_time_constraints rejects references missing from group", {
  dat <- data.frame(
    group_id = "site1",
    sample_id = "A",
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list("B"))
  constraints <- build_time_constraint_table(dat)

  testthat::expect_error(
    validate_time_constraints(dat, constraints),
    "older_id values do not exist"
  )
})

testthat::test_that("check_time_constraint_cycles detects cycles", {
  constraints <- data.frame(
    group_id = rep("site1", 3),
    younger_id = c("A", "B", "C"),
    older_id = c("B", "C", "A"),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    check_time_constraint_cycles(constraints),
    "cycle"
  )
})

testthat::test_that("check_time_constraints uses calendar direction: older dates are earlier", {
  constraints <- data.frame(
    group_id = "site1",
    younger_id = "A",
    older_id = "B",
    stringsAsFactors = FALSE
  )

  sampled <- lubridate::as_datetime(c("2000-01-01", "1900-01-01"))
  names(sampled) <- c("site1::A", "site1::B")

  testthat::expect_true(check_time_constraints(sampled, constraints))

  sampled_bad <- lubridate::as_datetime(c("1800-01-01", "1900-01-01"))
  names(sampled_bad) <- c("site1::A", "site1::B")

  testthat::expect_false(check_time_constraints(sampled_bad, constraints))
})

testthat::test_that("sample_time_uncertainty returns fixed constrained samples", {
  dat <- data.frame(
    group_id = rep("site1", 3),
    sample_id = c("A", "B", "X"),
    fixed_time = lubridate::as_datetime(c("2000-01-01", "1900-01-01", "1950-01-01")),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list(character(0), character(0), character(0)))
  dat$younger_ids <- I(list(character(0), c("A", "X"), character(0)))

  res <- sample_time_uncertainty(dat, return_constraints = TRUE)

  testthat::expect_s3_class(res$sampled_times, "POSIXct")
  testthat::expect_true(check_time_constraints(res$sampled_times, res$constraints))
  testthat::expect_equal(names(res$sampled_times), c("site1::A", "site1::B", "site1::X"))
})

testthat::test_that("sample_time_uncertainty fails for impossible fixed constraints", {
  dat <- data.frame(
    group_id = rep("site1", 2),
    sample_id = c("A", "B"),
    fixed_time = lubridate::as_datetime(c("1800-01-01", "1900-01-01")),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list("B", character(0)))

  testthat::expect_error(
    sample_time_uncertainty(dat, max_iter = 3),
    "Failed to sample"
  )
})

testthat::test_that("uniform sampling works with constraints", {
  set.seed(123)
  dat <- data.frame(
    group_id = rep("site1", 2),
    sample_id = c("A", "B"),
    oldest_time = lubridate::as_datetime(c("1990-01-01", "1900-01-01")),
    youngest_time = lubridate::as_datetime(c("2000-01-01", "1910-01-01")),
    stringsAsFactors = FALSE
  )
  dat$older_ids <- I(list("B", character(0)))

  res <- sample_time_uncertainty(dat, max_iter = 10)

  testthat::expect_true(res["site1::B"] < res["site1::A"])
})

testthat::test_that("truncated normal sampling works", {
  testthat::skip_if_not_installed("truncnorm")
  set.seed(123)
  dat <- data.frame(
    group_id = "site1",
    sample_id = "A",
    mean_time = lubridate::as_datetime("2000-01-01"),
    sd_time = 10,
    stringsAsFactors = FALSE
  )

  res <- sample_time_uncertainty(dat, sd_time_units = "days")

  testthat::expect_s3_class(res, "POSIXct")
  testthat::expect_equal(names(res), "site1::A")
})
