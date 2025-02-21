test_that("we can explain tidysdm objects", {
  # simple ensemble
  expect_true(
    inherits(
      explain_tidysdm(tidysdm::lacerta_ensemble,
        verbose = FALSE
      ), "explainer"
    )
  )
  # repeated ensemble
  lacerta_rep <- repeat_ensemble() %>%
    add_repeat(list(tidysdm::lacerta_ensemble, tidysdm::lacerta_ensemble))
  expect_true(inherits(
    explain_tidysdm(lacerta_rep, verbose = FALSE),
    "explainer"
  ))
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



test_that("explain_tidysdm works correctly with recipes with steps", {
  # we catch the problem of data not being explicitly provided if we have steps
  # in recipe
  lacerta_thin <- terra::readRDS(
    system.file("extdata/lacerta_thin_all_vars.rds",
      package = "tidysdm"
    )
  )

  # Add a topography variable with 3 levels
  lacerta_thin$topography <- cut(lacerta_thin$altitude,
    breaks = c(-Inf, 200, 800, Inf),
    labels = c("plains", "hills", "mountains")
  )

  # Subset variables
  lacerta_thin <- lacerta_thin %>% select(
    class, bio05, bio06,
    bio12, bio15, topography
  )

  # Create recipe
  lacerta_rec <- recipe(lacerta_thin, formula = class ~ .) %>%
    step_dummy(topography)

  # Define models
  lacerta_models <-
    workflow_set(
      preproc = list(default = lacerta_rec),
      models = list(
        glm = sdm_spec_glm(),
        rf = sdm_spec_rf()
      ),
      cross = TRUE
    ) %>%
    option_add(control = control_ensemble_grid())

  lacerta_cv <- spatial_block_cv(lacerta_thin, v = 3)
  lacerta_models <-
    lacerta_models %>%
    workflow_map("tune_grid",
      resamples = lacerta_cv, grid = 3,
      metrics = sdm_metric_set(), verbose = FALSE
    )

  # Fit ensemble
  lacerta_ensemble <- simple_ensemble() %>%
    add_member(lacerta_models, metric = "boyce_cont")
  expect_error(explain_tidysdm(lacerta_ensemble), "your recipe contains steps")
  # whilst this works
  expect_no_error(
    explain_tidysdm(lacerta_ensemble, data = lacerta_thin, verbose = FALSE)
  )
})

test_that("explain_tidysdm works with response provided directly", {
  lacerta_thin <- terra::readRDS(
    system.file("extdata/lacerta_thin_all_vars.rds",
      package = "tidysdm"
    )
  )
  lacerta_preds <- lacerta_thin %>%
    select(bio15, bio05, bio13, bio06) %>%
    sf::st_drop_geometry()
  test_explainer <- explain_tidysdm(tidysdm::lacerta_ensemble,
    verbose = FALSE
  )
  test_explainer_y <- explain_tidysdm(tidysdm::lacerta_ensemble,
    y = lacerta_thin$class,
    verbose = FALSE
  )
  test_explainer_data <- explain_tidysdm(tidysdm::lacerta_ensemble,
    data = lacerta_preds,
    y = lacerta_thin$class,
    verbose = FALSE
  )
  expect_true(all.equal(test_explainer, test_explainer_y))
  expect_true(all.equal(test_explainer, test_explainer_data))
  # error if we pass a response that is not a factor
  expect_error(
    explain_tidysdm(tidysdm::lacerta_ensemble,
      y = c(1, 2, 3),
      verbose = FALSE
    ),
    "y should be a factor with presences"
  )
})
