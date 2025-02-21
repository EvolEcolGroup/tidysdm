# set up a small world
# small grid
library(terra)
grid_raster <- terra::rast(matrix(1:64, ncol = 8, byrow = TRUE),
  extent = terra::ext(c(-2, 2, -2, 2)),
  crs = "lonlat"
)
# make two rows much more likely than the rest due to sampling bias
grid_raster[c(6, 8), ] <- 100 * grid_raster[c(6, 8), ]
# crs="epsg:4326")
grid_raster[c(1:5, 15, 16, 23, 24)] <- NA

# create a raster with multiple layers for each time step
grid_raster <- c(
  grid_raster, grid_raster, grid_raster, grid_raster,
  grid_raster
)
names(grid_raster) <- paste0("var1.", 0:4)
pastclim::time_bp(grid_raster) <- 0:4

# locations (first isolated, two closer to each other)
locations <- data.frame(
  lon = c(0.8, 1.9, 0.7, 0.4),
  lat = c(1.3, -1.8, -0.9, 1.2),
  id = 1:4,
  time = c(2, 2, 3, 4)
)

# nolint start
# points in poygons function
# (from https://stackoverflow.com/questions/72384038/point-in-polygon-using-terra-package-in-r)
# nolint end
pts_in_polys <- function(pts, polys) {
  e <- terra::extract(polys, pts)
  e[!is.na(e[, 2]), 1]
}

n_pt <- c(0, 0, 5, 6, 5)
buf_dist <- 80000
test_that("sample_background_time samples in the right places", {
  # error if time is not a posix object
  expect_error(
    sample_background_time(locations,
      n = 25, raster = grid_raster
    ),
    "time is not a date"
  )
  set.seed(123)

  # STOP something is not working here

  bg_dist_max <- sample_background_time(locations,
    n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
    method = c("dist_max", buf_dist),
    return_pres = FALSE
  )
  # we have the right number of pseudoabsences per time
  expect_true(all(n_pt[3:5] == table(bg_dist_max$time_step)))
  # all are within the buffer at a given time step (note that time==2 is the
  # THIRD time step, as we start with zero)
  max_buffer <- terra::buffer(
    terra::vect(
      locations %>%
        dplyr::filter(time == 2),
      crs = "lonlat"
    ), buf_dist
  )
  expect_true(
    length(pts_in_polys(
      terra::vect(
        bg_dist_max %>%
          dplyr::filter(time_step == as.Date("1952-01-01"))
      ), max_buffer
    )) == n_pt[3]
  )
  # but not in the buffer of the presence for the next time step
  max_buffer <- terra::buffer(
    terra::vect(
      locations %>%
        dplyr::filter(time == 3),
      crs = "lonlat"
    ), buf_dist
  )
  expect_true(
    length(pts_in_polys(
      terra::vect(
        bg_dist_max %>%
          dplyr::filter(time_step == as.Date("1952-01-01"))
      ), max_buffer
    )) == 0
  )

  # now set the time buffer so that we allow presences to impact background in
  # other time steps
  set.seed(123)
  bg_dist_max <- sample_background_time(locations,
    n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
    method = c("dist_max", buf_dist),
    return_pres = FALSE,
    time_buffer = y2d(1)
  )
  # we have the right number of background points per time
  expect_true(all(n_pt[3:5] == table(bg_dist_max$time_step)))
  # now we have points in the buffer of the presence for the next time step
  max_buffer <- terra::buffer(
    terra::vect(
      locations %>%
        dplyr::filter(time == 3),
      crs = "lonlat"
    ), buf_dist
  )
  expect_true(
    length(
      pts_in_polys(
        terra::vect(bg_dist_max %>%
          dplyr::filter(
            time_step == as.Date("1952-01-01")
          )), max_buffer
      )
    ) > 0
  )

  # now check that the bias method works
  set.seed(123)
  bg_bias <- sample_background_time(locations,
    n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
    method = c("bias"),
    return_pres = FALSE
  )
  # we have the right number of background points per time
  expect_true(all(n_pt[3:5] == table(bg_bias$time_step)))
  # almost all cells in those two rows should include points
  expect_true(sum(bg_bias$lat %in% c(-0.75, -1.75)) > 14)

  # and now a couple of error messages
  expect_error(
    sample_background_time(locations,
      n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
      method = c("bias"),
      return_pres = FALSE,
      time_buffer = y2d(1)
    ), "'time_buffer' should only be set with method 'dist_max'"
  )

  n_pt <- c(1, 0, 5, 6, 5)
  expect_error(
    sample_background_time(locations,
      n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
      method = c("dist_max", buf_dist),
      return_pres = FALSE,
      time_buffer = y2d(1)
    ), "for time 1950-01-01 there no presences when 1 background"
  )
})

# nolint start
# sample code to plot points
# i <- 3
# plot(grid_raster[[i]],colNA="darkgray")
# polys(terra::as.polygons(grid_raster[[i]]))
# points(vect(locations %>% filter(time==i-1)), col="red", cex=2)
# points(terra::vect(pa_random %>%
#               dplyr::filter(time_step==as.Date("1952-01-01"))), col="blue")
# polys(max_buffer)
# nolint end



test_that("sample_background_time treats time correctly", {
  # change time of raster to POSIX
  time(grid_raster) <- lubridate::date_decimal(time(grid_raster))
  expect_true(inherits(time(grid_raster), "POSIXct"))
  # this should work just fine
  set.seed(123)
  bg_dist_max <- sample_background_time(locations,
    n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
    method = c("dist_max", buf_dist),
    return_pres = FALSE
  )
  # we have the right number of pseudoabsences per time
  expect_true(all(n_pt[3:5] == table(bg_dist_max$time_step)))

  # Now change it to dates
  time(grid_raster) <- as.Date(time(grid_raster))
  expect_true(inherits(time(grid_raster), "Date"))
  set.seed(123)
  bg_dist_max <- sample_background_time(locations,
    n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
    method = c("dist_max", buf_dist),
    return_pres = FALSE
  )
  # we have the right number of pseudoabsences per time
  expect_true(all(n_pt[3:5] == table(bg_dist_max$time_step)))

  # now set the time to raw units to trigger error
  pastclim::time_bp(grid_raster) <- 0:4
  time(grid_raster) <- time(grid_raster)
  expect_true(timeInfo(grid_raster)$step == "raw")
  expect_error(
    sample_background_time(locations,
      n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
      method = c("dist_max", buf_dist),
      return_pres = FALSE
    ), "the units of the time axis of the raster are not defined"
  )
})
