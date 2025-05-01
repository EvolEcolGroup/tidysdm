lacerta_ensemble

climate_future <- terra::readRDS(
  system.file("extdata/lacerta_climate_future_10m.rds",
              package = "tidysdm"
  )
)

prediction_future <- predict_raster(lacerta_ensemble, climate_future)


prediction_future2 <- predict_raster_old(lacerta_ensemble, climate_future)

prediction_future_m <- predict_raster(lacerta_ensemble, climate_future, members = TRUE)

prediction_future_m2 <- predict_raster(lacerta_ensemble, climate_future, members = TRUE)



prediction_future_binary2 <- predict_raster_old(lacerta_ensemble_cal,
                                            climate_future,
                                            type = "class",
                                            class_thresh = c("tss_max"),
                                            metric_thresh = c("boyce_cont", 0.5)
)

prediction_future_binary <- predict_raster(lacerta_ensemble_cal,
                                               climate_future,
                                               type = "class",
                                               class_thresh = c("tss_max"),
                                               metric_thresh = c("boyce_cont", 0.5)
)
