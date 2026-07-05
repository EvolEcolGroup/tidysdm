skip_if_not_installed("blockCV")

suppressPackageStartupMessages(library(blockCV))
test_that("blockcv2rsample conversion", {
  # we use examples from the blockcv library for spatial blocks
  points <- read.csv(system.file("extdata/", "species.csv",
    package = "blockCV"
  ))
  pa_data <- sf::st_as_sf(points, coords = c("x", "y"), crs = 7845)
  pa_data$occ <- as.factor(pa_data$occ)
  # load raster data
  path <- system.file("extdata/au/", package = "blockCV")
  files <- list.files(path, full.names = TRUE)
  covars <- terra::rast(files)

  pa_data <- pa_data %>%
    select(geometry, occ) %>%
    bind_cols(terra::extract(covars, pa_data, ID = FALSE))

  sb1 <- cv_spatial(
    x = pa_data,
    column = "occ", # the response column (binary or multi-class)
    k = 5, # number of folds
    size = 350000, # size of the blocks in metres
    selection = "random", # random blocks-to-fold
    iteration = 10,
    report = FALSE,
    progress = FALSE
  ) # find evenly dispersed folds
  sb1_rsample <- blockcv2rsample(sb1, pa_data)
  expect_true(inherits(sb1_rsample, "spatial_rset"))


  # spatial clustering
  set.seed(6)
  sc <- cv_cluster(
    x = pa_data,
    column = "occ", # optional; name of the column with response
    k = 5,
    report = FALSE
  )
  sc_rsample <- blockcv2rsample(sc, pa_data)
  expect_true(inherits(sc_rsample, "spatial_rset"))
  #'
  # environmental clustering
  set.seed(6)
  ec <- cv_cluster(
    r = covars, # if provided will be used for environmental clustering
    x = pa_data,
    column = "occ", # optional; name of the column with response
    k = 5,
    scale = TRUE,
    report = FALSE
  )
  ec_rsample <- blockcv2rsample(ec, pa_data)
  expect_true(inherits(ec_rsample, "spatial_rset"))

  # give error for unsuppored mode in blockcv
  nndm <- cv_nndm(
    x = pa_data,
    column = "occ", # optional
    r = covars,
    size = 350000, # size in metres no matter the CRS
    num_sample = 10,
    sampling = "regular",
    min_train = 0.1,
    plot = FALSE,
    report = FALSE
  )
  nndm_rsample <- blockcv2rsample(nndm, pa_data)
  expect_true(inherits(nndm_rsample, "spatial_rset"))

  # and no a cv_buffer object
  buffer_pa <- cv_buffer(
    x = pa_data,
    column = "occ",
    size = 350000, # size in metres no matter the CRS
    presence_bg = FALSE
  )
  buffer_pa_rsample <- blockcv2rsample(buffer_pa, pa_data)
  expect_true(inherits(buffer_pa_rsample, "spatial_rset"))

  # get error if x is not a blockcv object
  expect_error(
    blockcv2rsample(pa_data, pa_data),
    "this function does not support this object type"
  )


  # give error for deprecated object
  pa_data_spd <- sf::as_Spatial(pa_data)
  expect_error(
    blockcv2rsample(sb1, pa_data_spd),
    "data is a `SpatialPointsDataFrame`; this object type is deprecated"
  )

  # check blockcv2rsample object works with workflow_map

  # create example recipe
  example_rec <- recipe(pa_data, formula = occ ~ .)

  # create test model workflow
  example_models <-
    # create the workflow_set
    workflow_set(
      preproc = list(default = example_rec),
      models = list(
        # rf specs with tuning
        rf = sdm_spec_rf(),
        # boosted tree model (gbm) specs with tuning
        gbm = sdm_spec_boost_tree(),
        # maxent specs with tuning
        maxent = sdm_spec_maxent()
      ),
      # make all combinations of preproc and models,
      cross = TRUE
    ) %>%
    # tweak controls to store information needed later to create the ensemble
    option_add(control = control_ensemble_grid())

  # workflow_map with the blockcv2rsample object - cv_spatial
  example_workflow_spatial <- example_models %>%
    workflow_map("tune_grid",
      resamples = sb1_rsample, grid = 1,
      metrics = sdm_metric_set(), verbose = TRUE
    )

  # class of results should include "tune_results"
  expect_true("tune_results" %in% class(example_workflow_spatial$result[[1]]))

  # workflow_map with the blockcv2rsample object - cv_cluster
  example_workflow_cluster <- example_models %>%
    workflow_map("tune_grid",
      resamples = sc_rsample, grid = 1,
      metrics = sdm_metric_set(), verbose = TRUE
    )

  # class of results should include "tune_results"
  expect_true("tune_results" %in% class(example_workflow_cluster$result[[1]]))
})
