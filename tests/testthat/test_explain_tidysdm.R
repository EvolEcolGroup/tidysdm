test_that("we can explain tidysdm objects", {
  # simple ensemble
  expect_true(inherits(explain_tidysdm(tidysdm::lacerta_ensemble, 
                                       verbose=FALSE), "explainer"))
  # repeated ensemble
  lacerta_rep <- repeat_ensemble() %>%
    add_repeat(list(tidysdm::lacerta_ensemble, tidysdm::lacerta_ensemble))
  expect_true(inherits(explain_tidysdm(lacerta_rep, 
                                       verbose=FALSE), "explainer"))
  expect_identical(
    DALEX::model_info(lacerta_rep),
    DALEX::model_info(tidysdm::lacerta_ensemble)
  )
  # errors
  expect_error(
    explain_tidysdm("blah"),
    "no method defined for this object type"
  )
  expect_error(
    explain_tidysdm(tidysdm::lacerta_ensemble, type = "regression"),
    "type has to be classification for a tidysdm ensemble"
  )
  expect_error(
    DALEX::model_info(lacerta_rep, is_multiclass = TRUE),
    "tidysdm repeat_ensembles can not be multiclass"
  )
  expect_error(
    DALEX::model_info(tidysdm::lacerta_ensemble, is_multiclass = TRUE),
    "tidysdm simple_ensembles can not be multiclass"
  )
})
