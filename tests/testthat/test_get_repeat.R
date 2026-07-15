test_that("we get the correct repeat", {
  this_ens <- get_repeat(lacerta_rep_ens, i = "rep_02")
  # check that it is a simple ensemble
  expect_true(inherits(this_ens, "simple_ensemble"))
  # now do the same by index
  this_ens2 <- get_repeat(lacerta_rep_ens, i = 2)
  expect_equal(this_ens, this_ens2)
})

test_that("get_repeat returns correct errors for invalid inputs", {
  # error when x is not a repeat ensemble
  expect_error(
    get_repeat(lacerta_ensemble, i = 1),
    "x must be a repeat ensemble object"
  )
  # error when i has length != 1
  expect_error(
    get_repeat(lacerta_rep_ens, i = c(1, 2)),
    "i must be of length 1"
  )
  # error when numeric i is NA
  expect_error(
    get_repeat(lacerta_rep_ens, i = NA_integer_),
    "i must not be NA or non-finite"
  )
  # error when numeric i is non-finite
  expect_error(
    get_repeat(lacerta_rep_ens, i = Inf),
    "i must not be NA or non-finite"
  )
  # error when numeric i is not an integer value
  expect_error(
    get_repeat(lacerta_rep_ens, i = 1.5),
    "i must be an integer"
  )
  # error when numeric i is out of range (too low)
  expect_error(
    get_repeat(lacerta_rep_ens, i = 0),
    "i is outside the range of repeats in x"
  )
  # error when numeric i is out of range (too high)
  expect_error(
    get_repeat(lacerta_rep_ens, i = 100),
    "i is outside the range of repeats in x"
  )
  # error when character i is NA
  expect_error(
    get_repeat(lacerta_rep_ens, i = NA_character_),
    "i must not be NA"
  )
  # error when character i is not a valid repeat name
  expect_error(
    get_repeat(lacerta_rep_ens, i = "rep_99"),
    "i must be a valid name of a repeat in x"
  )
  # error when i is neither numeric nor character
  expect_error(
    get_repeat(lacerta_rep_ens, i = TRUE),
    "i must be either numeric or character"
  )
})
