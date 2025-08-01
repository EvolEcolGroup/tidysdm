skip_if_not_installed("ranger")
test_that("niche_overlap quantifies difference between rasters", {
  climate_present <- terra::readRDS(
    system.file("extdata/lacerta_climate_present_10m.rds",
      package = "tidysdm"
    )
  )

  climate_future <- terra::readRDS(
    system.file("extdata/lacerta_climate_future_10m.rds",
      package = "tidysdm"
    )
  )
  lacerta_present <- predict_raster(lacerta_ensemble, climate_present)
  lacerta_future <- predict_raster(lacerta_ensemble, climate_future)
  overlap_list <- niche_overlap(lacerta_present, lacerta_future)
  expect_true(all(unlist(overlap_list) > 0.75))

  expect_error(
    niche_overlap(climate_present, lacerta_future),
    "x and y are expected to each contain one layer"
  )
})
