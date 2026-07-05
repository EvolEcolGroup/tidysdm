skip_if_not_installed("blockCV")

suppressPackageStartupMessages(library(blockCV))

test_that("blockcv2initial_split conversion", {
  # example data
  points <- read.csv(system.file(
    "extdata/",
    "species.csv",
    package = "blockCV"
  ))

  pa_data <- sf::st_as_sf(
    points,
    coords = c("x", "y"),
    crs = 7845
  )

  pa_data$occ <- as.factor(pa_data$occ)

  path <- system.file("extdata/au/", package = "blockCV")
  files <- list.files(path, full.names = TRUE)
  covars <- terra::rast(files)

  pa_data <- pa_data %>%
    dplyr::select(geometry, occ) %>%
    dplyr::bind_cols(
      terra::extract(covars, pa_data, ID = FALSE)
    )

  # spatial blocks
  sb1 <- cv_spatial(
    x = pa_data,
    column = "occ",
    k = 5,
    size = 350000,
    selection = "random",
    iteration = 10,
    report = FALSE,
    progress = FALSE
  )

  sb1_split <- blockcv2initial_split(sb1, pa_data)

  expect_s3_class(sb1_split, "spatial_initial_split")

  # training/testing extraction works
  expect_true(nrow(training(sb1_split)) > 0)
  expect_true(nrow(testing(sb1_split)) > 0)

  # clustering
  set.seed(6)

  sc <- cv_cluster(
    x = pa_data,
    column = "occ",
    k = 5,
    report = FALSE
  )

  sc_split <- blockcv2initial_split(sc, pa_data)

  expect_s3_class(sc_split, "spatial_initial_split")

  # environmental clustering
  set.seed(6)

  ec <- cv_cluster(
    r = covars,
    x = pa_data,
    column = "occ",
    k = 5,
    scale = TRUE,
    report = FALSE
  )

  ec_split <- blockcv2initial_split(ec, pa_data)

  expect_s3_class(ec_split, "spatial_initial_split")

  # nearest neighbour distance matching
  nndm <- cv_nndm(
    x = pa_data,
    column = "occ",
    r = covars,
    size = 350000,
    num_sample = 10,
    sampling = "regular",
    min_train = 0.1,
    plot = FALSE,
    report = FALSE
  )

  nndm_split <- blockcv2initial_split(nndm, pa_data)

  expect_s3_class(nndm_split, "spatial_initial_split")

  # buffered CV
  buffer_pa <- cv_buffer(
    x = pa_data,
    column = "occ",
    size = 350000,
    presence_bg = FALSE
  )

  buffer_pa_split <- blockcv2initial_split(
    buffer_pa,
    pa_data
  )

  expect_s3_class(buffer_pa_split, "spatial_initial_split")

  # invalid object
  expect_error(
    blockcv2initial_split(pa_data, pa_data),
    "this function does not support this object type"
  )

  # deprecated SpatialPointsDataFrame
  pa_data_spd <- sf::as_Spatial(pa_data)

  expect_error(
    blockcv2initial_split(sb1, pa_data_spd),
    "data is a `SpatialPointsDataFrame`; this object type is deprecated"
  )
})
