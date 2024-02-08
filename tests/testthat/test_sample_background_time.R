# set up a small world
# small grid
library(terra)
grid_raster <- terra::rast(matrix(1:64, ncol = 8, byrow = TRUE),
  extent = terra::ext(c(-2, 2, -2, 2)),
  crs = "lonlat"
)
# make two rows much more likely than the rest due to sampling bias
grid_raster[c(6,8),] <- 100*grid_raster[c(6,8),]
# crs="epsg:4326")
grid_raster[c(1:5, 15, 16, 23, 24)] <- NA

# create a raster with multiple layers for each time step
grid_raster <- c(grid_raster, grid_raster, grid_raster, grid_raster, grid_raster)
names(grid_raster) <- paste0("var1.",0:4)
pastclim::time_bp(grid_raster)<-0:4

# locations (first isolated, two closer to each other)
locations <- data.frame(
  lon = c(0.8, 1.9, 0.7, 0.4),
  lat = c(1.3, -1.8, -0.9, 1.2),
  id = 1:4,
  time = c(2,2,3,4)
)

# points in poygons function
# (from https://stackoverflow.com/questions/72384038/point-in-polygon-using-terra-package-in-r)
pts_in_polys <- function(pts, polys) {
  e <- terra::extract(polys, pts)
  e[!is.na(e[, 2]), 1]
}

n_pt <- c(0,0, 10,10,5)
buf_dist <- 80000
test_that("sample_background_time samples in the right places", {
  # error if time is not a posix object
  expect_error(sample_background_time(locations,
                                      n = 25, raster = grid_raster),
               "time is not a date")
  set.seed(123)
  
  # STOP something is not working here
  
  pa_random <- sample_background_time(locations,
    n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
    method = c("dist_max", buf_dist),
    return_pres = FALSE
  )
  # we have the right number of pseudoabsences per time
  expect_true(all(table(locations$time)*n_pt==table(pa_random$time_step)))
  # none are within the buffer at a given time step
  min_buffer <- terra::buffer(terra::vect(locations %>%
                                            dplyr::filter(time==2), crs = "lonlat"), buf_dist)
  expect_true(length(pts_in_polys(terra::vect(pa_random %>% 
                                                dplyr::filter(time_step==as.Date("1952-01-01"))), min_buffer)) == 0)
  # but they ignore presences from other time steps
  min_buffer <- terra::buffer(terra::vect(locations %>%
                                            dplyr::filter(time==3), crs = "lonlat"), buf_dist)
  expect_false(length(pts_in_polys(terra::vect(pa_random %>% 
                                                 dplyr::filter(time_step==as.Date("1952-01-01"))), min_buffer)) == 0)
  min_buffer <- terra::buffer(terra::vect(locations %>%
                                            dplyr::filter(time==4), crs = "lonlat"), buf_dist)
  expect_false(length(pts_in_polys(terra::vect(pa_random %>% 
                                                 dplyr::filter(time_step==as.Date("1952-01-01"))), min_buffer)) == 0)
  
  # now set the time buffer so that we allow presences to impact absences in other time steps
  set.seed(123)
  pa_random <- sample_background_time(locations,
                                     n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
                                     method = c("dist_min", buf_dist),
                                     return_pres = FALSE,
                                     time_buffer = y2d(1)
  )  
  # we have the right number of pseudoabsences per time
  expect_true(all(table(locations$time)*n_pt==table(pa_random$time_step)))
  # none are within the buffer at a given time step
  min_buffer <- terra::buffer(terra::vect(locations %>%
                                            dplyr::filter(time==2), crs = "lonlat"), buf_dist)
  expect_true(length(pts_in_polys(terra::vect(pa_random %>% 
                                                dplyr::filter(time_step==as.Date("1952-01-01"))), min_buffer)) == 0)
  # none are within the buffer of a location in the time buffer
  min_buffer <- terra::buffer(terra::vect(locations %>%
                                            dplyr::filter(time==3), crs = "lonlat"), buf_dist)
  expect_true(length(pts_in_polys(terra::vect(pa_random %>% 
                                                dplyr::filter(time_step==as.Date("1952-01-01"))), min_buffer)) == 0)
  # but they ignore presences from time steps outside the buffer
  min_buffer <- terra::buffer(terra::vect(locations %>%
                                            dplyr::filter(time==4), crs = "lonlat"), buf_dist)
  expect_false(length(pts_in_polys(terra::vect(pa_random %>% 
                                                 dplyr::filter(time_step==as.Date("1952-01-01"))), min_buffer)) == 0)
  
  
  # now check that we return the right number of presences
  set.seed(123)
  pa_random <- sample_background_time(locations,
                                     n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
                                     method = c("dist_min", buf_dist),
                                     return_pres = TRUE,
                                     time_buffer = y2d(1)
  )  
  expect_true(table(pa_random$class)[1]==4)
  expect_true(table(pa_random$class)[2]==n_pt*table(pa_random$class)[1])
  
})

# sample code to plot points
# i <- 2
# plot(grid_raster[[i]],colNA="darkgray")
# polys(terra::as.polygons(grid_raster[[i]]))
# points(vect(locations %>% filter(time==i)), col="red", cex=2)
# points(terra::vect(pa_random %>% 
#               dplyr::filter(time_step==as.Date("1952-01-01"))), col="blue")
# polys(min_buffer)
