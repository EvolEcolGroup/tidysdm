test_that("sdm_spec_gam", {
  # error for invalid tuning method
  expect_error(sdm_spec_gam(tune = "blah")) # automatic error from rlang
  this_spec_gam <- sdm_spec_gam()
  expect_true(inherits(this_spec_gam, "model_spec"))
  expect_true(inherits(this_spec_gam, "gen_additive_mod"))
  # for the default, which is none, there is nothing to tune
  this_spec_gam <- sdm_spec_gam(tune = "none")
  # get errors if we try the tuning options used in other sdm_spec_*
  expect_error(sdm_spec_gam(tune = "sdm"))
  expect_error(sdm_spec_gam(tune = "all"))
  expect_error(sdm_spec_gam(tune = "custom"))
})
