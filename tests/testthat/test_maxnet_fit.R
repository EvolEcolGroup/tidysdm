test_that("maxnet_fit is equivalent to maxnet", {
  data("bradypus", package = "maxnet")
  brad_p <- bradypus$presence
  brad_data <- bradypus[, -1]
  mod <- maxnet::maxnet(
    brad_p, brad_data,
    maxnet::maxnet.formula(brad_p,
      brad_data,
      classes = "lq"
    )
  )
  bradypus_tb <- tibble::as_tibble(bradypus) %>%
    dplyr::mutate(presence = relevel(
      factor(
        dplyr::recode_values(presence, 1 ~ "presence", 0 ~ "absence")
      ),
      ref = "presence"
    ))

  mod2 <- maxnet_fit(presence ~ ., data = bradypus_tb, classes = "lq")
  expect_true(identical(mod, mod2))
})

test_that("maxnet_fit works with a plain data.frame", {
  data("bradypus", package = "maxnet")
  bradypus_df <- bradypus %>%
    dplyr::mutate(presence = relevel(
      factor(
        dplyr::recode_values(presence, 1 ~ "presence", 0 ~ "absence")
      ),
      ref = "presence"
    ))
  expect_true(inherits(bradypus_df, "data.frame"))
  expect_false(inherits(bradypus_df, "tbl_df"))
  mod_df <- maxnet_fit(presence ~ ., data = bradypus_df, classes = "lq")
  mod_tb <- maxnet_fit(presence ~ .,
    data = tibble::as_tibble(bradypus_df),
    classes = "lq"
  )
  expect_identical(mod_df, mod_tb)
  # and when fitting the parsnip model
  maxent_fit <- maxent(feature_classes = "lq") %>%
    fit(presence ~ ., data = bradypus_df)
  expect_true(inherits(maxent_fit$fit, "maxnet"))
})
