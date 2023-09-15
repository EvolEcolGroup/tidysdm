test_that("we can explain tidysdm objects", {
  # simple ensemble
  explainer_lacerta_ens <- explain_tidysdm(tidysdm::lacerta_ensemble)
  expect_true(inherits(explainer_lacerta_ens, "explainer"))
  # repeated ensemble
  lacerta_rep <- repeat_ensemble() %>%
    add_repeat(list(tidysdm::lacerta_ensemble, tidysdm::lacerta_ensemble))
  explainer_lacerta_rep <- explain_tidysdm(lacerta_rep)
  expect_true(inherits(explainer_lacerta_rep, "explainer"))
  
})
