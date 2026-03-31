test_that("we get the correct repeat",{
  this_ens <- get_repeat(lacerta_rep_ens, i = "rep_02")
  # check that it is a simple ensemble
  expect_true(inherits(this_ens, "simple_ensemble"))
  # now do the same by index
  this_ens2 <- get_repeat(lacerta_rep_ens, i = 2)
  expect_equal(this_ens, this_ens2)
})
