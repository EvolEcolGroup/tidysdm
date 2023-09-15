test_that("we can explain tidysdm objects", {
  # simple ensemble
  expect_true(inherits(explain_tidysdm(tidysdm::lacerta_ensemble), "explainer"))
  # repeated ensemble
  lacerta_rep <- repeat_ensemble() %>%
    add_repeat(list(tidysdm::lacerta_ensemble, tidysdm::lacerta_ensemble))
  expect_true(inherits(explain_tidysdm(lacerta_rep), "explainer"))
  
})
