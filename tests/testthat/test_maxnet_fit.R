test_that("maxnet_fit is equivalent to maxnet", {
  data("bradypus", package="maxnet")
  brad_p <- bradypus$presence
  brad_data <- bradypus[,-1]
  mod <- maxnet::maxnet(brad_p, brad_data, maxnet::maxnet.formula(brad_p, brad_data, classes="lq"))
  bradypus_tb <- tibble::as_tibble(bradypus) %>% dplyr::mutate(presence = relevel(factor(
    dplyr::case_match (presence, 1~"presence",0 ~"absence")),
    ref="presence"))

  mod2 <- maxnet_fit(presence~.,data=bradypus_tb, classes="lq")
  expect_true(identical(mod, mod2))
})
