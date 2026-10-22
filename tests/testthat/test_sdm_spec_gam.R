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

test_that("gam_formula only uses smooths for numeric predictors", {
  lacerta_thin <- readRDS(system.file("extdata/lacerta_thin_all_vars.rds",
    package = "tidysdm"
  )) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(class, bio05, bio06)
  # with numeric predictors only
  expect_equal(
    gam_formula(recipe(lacerta_thin, formula = class ~ .)),
    class ~ s(bio05, k = 10) + s(bio06, k = 10),
    ignore_formula_env = TRUE
  )
  # with a single numeric predictor
  expect_equal(
    gam_formula(recipe(lacerta_thin, formula = class ~ bio05), k = 5),
    class ~ s(bio05, k = 5),
    ignore_formula_env = TRUE
  )
  # with factors, which are added as linear terms
  lacerta_thin$topo <- factor(lacerta_thin$bio05 > 25)
  lacerta_thin$topo2 <- factor(lacerta_thin$bio06 > 0)
  expect_equal(
    gam_formula(recipe(lacerta_thin, formula = class ~ .)),
    class ~ s(bio05, k = 10) + s(bio06, k = 10) + topo + topo2,
    ignore_formula_env = TRUE
  )
  # error if there are no numeric predictors
  expect_error(
    gam_formula(recipe(lacerta_thin, formula = class ~ topo)),
    "there are no numeric predictors"
  )
})
