devtools::load_all()
library(terra)
library(ggplot2)
library(tidyterra)

climate_future <- terra::readRDS(
  system.file("extdata/lacerta_climate_future_10m.rds",
    package = "tidysdm"
  )
)

# this should just work, checking, and for comparison with the binarised
prediction_future <- predict_raster(
  object = lacerta_rep_ens,
  raster = climate_future,
  metric_thresh = c("boyce_cont", 0.5),
  fun = "median"
)

# first we calibrate the model
lacerta_rep_ens <- calib_class_thresh(lacerta_rep_ens,
  class_thresh = "tss_max"
)

lacerta_rep_ens <- calib_class_thresh(
  lacerta_rep_ens,
  class_thresh = "tss_max",
  metric_thresh = c("boyce_cont", 0.5)
)

prediction_future_binary <- predict_raster(
  object = lacerta_rep_ens,
  raster = climate_future,
  metric_thresh = c("boyce_cont", 0.5),
  type = "class",
  fun = "median",
  class_fun = "majority",
  class_thresh = "tss_max"
)

ggplot() +
  geom_spatraster(data = prediction_future_binary, aes(fill = binary_median.majority)) +
  scale_fill_discrete(na.value = "transparent")


# check calibration
attr(lacerta_rep_ens, "class_thresholds_list")
attr(lacerta_rep_ens, "class_thresholds_list", exact = TRUE)[[2]]
