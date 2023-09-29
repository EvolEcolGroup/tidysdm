test_that("form_resp returns correct response", {
  my_formula <- formula("x~y+1")
  expect_equal(form_resp(my_formula), "x")
  my_formula <- formula("~y+1")
  expect_null(form_resp(my_formula))
})
