lacerta_ensemble

climate_future <- terra::readRDS(
  system.file("extdata/lacerta_climate_future_10m.rds",
              package = "tidysdm"
  )
)

prediction_future <- predict_raster(lacerta_ensemble, climate_future)
#prediction_future_chunk <- predict_raster(lacerta_ensemble, climate_future, n= 10^5)
prediction_future_chunk <- predict_raster_old(lacerta_ensemble, climate_future)
# check that the values are all correct
expect_true(all.equal(as.matrix(prediction_future), as.matrix(prediction_future_chunk), na.rm=TRUE))
# check that the names are the same
expect_equal(names(prediction_future), names(prediction_future_chunk))
# check that varnames are the same
expect_equal(terra::varnames(prediction_future),
             terra::varnames(prediction_future_chunk))
# check that time is the same
expect_equal(terra::time(prediction_future),
             terra::time(prediction_future_chunk))
# check that timeinfo is the same
expect_equal(terra::timeInfo(prediction_future),
             terra::timeInfo(prediction_future_chunk))

# now check when using members
prediction_future_m <- predict_raster(lacerta_ensemble, climate_future, members = TRUE)
prediction_future_m_chunk <- predict_raster(lacerta_ensemble, climate_future, members = TRUE, n= 10^5)
# check that the values are all correct
expect_true(all.equal(as.matrix(prediction_future_m), as.matrix(prediction_future_m_chunk), na.rm=TRUE,
                      tolerance = 1e-5))
# check that the names are the same
expect_equal(names(prediction_future_m), names(prediction_future_m_chunk))
# check that varnames are the same
expect_equal(terra::varnames(prediction_future_m),
             terra::varnames(prediction_future_m_chunk))
# check that time is the same
expect_equal(terra::time(prediction_future_m),
             terra::time(prediction_future_m_chunk))
# check that timeinfo is the same
expect_equal(terra::timeInfo(prediction_future_m),
             terra::timeInfo(prediction_future_m_chunk))

# calibrate the ensemble
#lacerta_ensemble_cal <- 
)

prediction_future_b_chunk <- predict_raster_old(lacerta_ensemble_cal,
                                            climate_future,
                                            type = "class",
                                            class_thresh = c("tss_max"),
                                            metric_thresh = c("boyce_cont", 0.5)
)

prediction_future_b <- predict_raster(lacerta_ensemble_cal,
                                               climate_future,
                                               type = "class",
                                               class_thresh = c("tss_max"),
                                               metric_thresh = c("boyce_cont", 0.5)
)
