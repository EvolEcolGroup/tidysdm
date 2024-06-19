library(sf)
test_that("spatial_initial_split", {
lacerta_thin <- readRDS(system.file("extdata/lacerta_climate_sf.RDS",
                                    package = "tidysdm"
))
set.seed(1005)
lacerta_initial <- spatial_initial_split(lacerta_thin,
                                         prop = 1 / 5, spatial_block_cv
)
expect_true(inherits(lacerta_initial,"spatial_initial_split"))
# expect the proportion of training to full dataset to be ~4/5 (within 5% of it
# due to the grouping into spatial blocks)
expect_true((length(lacerta_initial$in_id)/nrow(lacerta_thin)-4/5)<0.05)

# lacerta_initial_autoplot <- autoplot(lacerta_initial)
# vdiffr::expect_doppelganger("Autoplot of initial spatial split", lacerta_initial_autoplot)

# check error if we give incorrect strategy
expect_error(spatial_initial_split(lacerta_thin,
                                   prop = 1 / 5, spatial_block_cv2),
             "spatial_block_cv2 is not a function in spatialsample")
# check error for proportion out of bounds
expect_error(spatial_initial_split(lacerta_thin,
                                   prop = 6 / 5, spatial_block_cv),
             "`prop` must be a number between 0 and 1")
}

)
