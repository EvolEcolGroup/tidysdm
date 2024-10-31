library(sf)
test_that("sdm_recipe_sf", {
  lacerta_thin <- readRDS(system.file("extdata/lacerta_climate_sf.RDS",
    package = "tidysdm"
  ))
  lacerta_rec <- recipe(lacerta_thin, formula = class ~ .)
  expect_true(inherits(lacerta_rec, "spatial_recipe"))
  lacerta_spatial_rec <- spatial_recipe(lacerta_thin, formula = class ~ .)
  expect_true(identical(lacerta_rec, lacerta_spatial_rec))
  expect_error(
    spatial_recipe(lacerta_thin %>% sf::st_drop_geometry(),
      formula = class ~ .
    ),
    "x should be an `sf` object"
  )
  lacerta_xy <- lacerta_thin %>% dplyr::mutate(X = 1, Y = 2)
  # deal with X and Y coordinates
  expect_error(
    recipe(lacerta_xy, formula = class ~ .),
    "sf object"
  )
  lacerta_xy <- lacerta_thin %>% dplyr::bind_cols(sf::st_coordinates(lacerta_thin))
  lacerta_xy_rec <- recipe(lacerta_xy, formula = class ~ .)
  expect_true(identical(lacerta_xy_rec, lacerta_rec))
  lacerta_xy$X[3] <- 190
  expect_error(
    recipe(lacerta_xy, formula = class ~ .),
    "sf object"
  )
  # check prep methods
  lacerta_rec_prep <- prep(lacerta_rec)
  expect_true(recipes::fully_trained(lacerta_rec_prep))
  expect_false(recipes::fully_trained(lacerta_rec))
  # work if we pass an sf object
  lacerta_rec_prep <- prep(lacerta_rec, training = lacerta_thin)
  expect_true(recipes::fully_trained(lacerta_rec_prep))

  ## now bake
  expect_true(all(c("X", "Y") %in% names(bake(lacerta_rec_prep, new_data = lacerta_thin))))
  baked_no_xy <- bake(lacerta_rec_prep,
    new_data = lacerta_thin %>% sf::st_drop_geometry()
  )
  expect_true(all(c("X", "Y") %in% names(baked_no_xy)))
  # X should just be a dummy variable
  expect_true(all(is.na(baked_no_xy$X)))
})

test_that("sdm_recipe_sf works with a geometry named differently", {
  lacerta_thin <- readRDS(system.file("extdata/lacerta_climate_sf.RDS",
    package = "tidysdm"
  ))
  sf::st_geometry(lacerta_thin) <- "geom"
  lacerta_rec <- recipe(lacerta_thin, formula = class ~ .)

  lacerta_models <-
    # create the workflow_set
    workflow_set(
      preproc = list(default = lacerta_rec),
      models = list(
        # the standard glm specs
        glm = sdm_spec_glm(),
        # rf specs with tuning
        rf = sdm_spec_rf()
      ),
      # make all combinations of preproc and models,
      cross = TRUE
    ) %>%
    # tweak controls to store information needed later to create the ensemble
    option_add(control = control_ensemble_grid())
  set.seed(100)
  lacerta_cv <- spatial_block_cv(data = lacerta_thin, v = 3, n = 5)
  lacerta_models <-
    lacerta_models %>%
    workflow_map("tune_grid",
                 resamples = lacerta_cv, grid = 3,
                 metrics = sdm_metric_set(), verbose = FALSE
    )
  res <- collect_notes(.Last.tune.result) %>% dplyr::distinct(type, note)
  # expect no warnings
  expect_true(nrow(res) == 0)
})
