test_that("sdm_spec_glm", {
  # error for invalid tuning method
  expect_error(sdm_spec_glm(tune="blah")) # automatic error from rlang
  this_spec_glm <-  sdm_spec_glm()
  expect_true(inherits(this_spec_glm,"model_spec"))
  expect_true(inherits(this_spec_glm,"logistic_reg"))
  # for the default, which is none, there is nothing to tune
  # get errors if we try the tuning options used in other sdm_spec_*
  expect_error(sdm_spec_glm(tune="sdm"))
  expect_error(sdm_spec_glm(tune="all"))
  expect_error(sdm_spec_glm(tune="custom"))
})
