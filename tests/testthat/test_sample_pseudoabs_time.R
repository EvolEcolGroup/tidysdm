# set up a small world
# small grid
library(terra)
grid_raster <- terra::rast(matrix(1:64, ncol = 8, byrow = TRUE),
  extent = terra::ext(c(-2, 2, -2, 2)),
  crs = "lonlat"
)
# crs="epsg:4326")
grid_raster[c(1:5, 15, 16, 23, 24)] <- NA

# create a raster with multiple layers for each time step
grid_raster <- c(
  grid_raster, grid_raster, grid_raster,
  grid_raster, grid_raster
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

n_pt <- 10
buf_dist <- 80000
test_that("sample_pseudoabs_time samples in the right places", {
  # error if time is not a posix object
  expect_error(
    sample_pseudoabs_time(locations,
      n = 25, raster = grid_raster
    ),
    "time is not a date"
  )
  set.seed(123)
  pa_random <- sample_pseudoabs_time(locations,
    n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
    method = c("dist_min", buf_dist),
    return_pres = FALSE
  )
  # we have the right number of pseudoabsences per time
  expect_true(all(table(locations$time) * n_pt == table(pa_random$time_step)))
  # none are within the buffer at a given time step
  min_buffer <- terra::buffer(
    terra::vect(
      locations %>%
        dplyr::filter(time == 2),
      crs = "lonlat"
    ), buf_dist
  )
  expect_true(
    length(
      pts_in_polys(
        terra::vect(pa_random %>% dplyr::filter(
          time_step == as.Date("1952-01-01")
        )), min_buffer
      )
    ) == 0
  )
  # but they ignore presences from other time steps
  min_buffer <- terra::buffer(
    terra::vect(
      locations %>%
        dplyr::filter(time == 3),
      crs = "lonlat"
    ), buf_dist
  )
  expect_false(
    length(
      pts_in_polys(
        terra::vect(pa_random %>%
          dplyr::filter(
            time_step == as.Date("1952-01-01")
          )), min_buffer
      )
    ) == 0
  )
  min_buffer <- terra::buffer(
    terra::vect(
      locations %>%
        dplyr::filter(time == 4),
      crs = "lonlat"
    ), buf_dist
  )
  expect_false(
    length(
      pts_in_polys(
        terra::vect(pa_random %>%
          dplyr::filter(
            time_step == as.Date("1952-01-01")
          )), min_buffer
      )
    ) == 0
  )

  # now set the time buffer so that we allow presences to impact absences in
  # other time steps
  set.seed(123)
  pa_random <- sample_pseudoabs_time(locations,
    n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
    method = c("dist_min", buf_dist),
    return_pres = FALSE,
    time_buffer = y2d(1)
  )
  # we have the right number of pseudoabsences per time
  expect_true(all(table(locations$time) * n_pt == table(pa_random$time_step)))
  # none are within the buffer at a given time step
  min_buffer <- terra::buffer(
    terra::vect(
      locations %>%
        dplyr::filter(time == 2),
      crs = "lonlat"
    ), buf_dist
  )
  expect_true(
    length(
      pts_in_polys(
        terra::vect(pa_random %>%
          dplyr::filter(
            time_step == as.Date("1952-01-01")
          )), min_buffer
      )
    ) == 0
  )
  # none are within the buffer of a location in the time buffer
  min_buffer <- terra::buffer(
    terra::vect(
      locations %>%
        dplyr::filter(time == 3),
      crs = "lonlat"
    ), buf_dist
  )
  expect_true(
    length(
      pts_in_polys(
        terra::vect(pa_random %>%
          dplyr::filter(
            time_step == as.Date("1952-01-01")
          )), min_buffer
      )
    ) == 0
  )
  # but they ignore presences from time steps outside the buffer
  min_buffer <- terra::buffer(
    terra::vect(
      locations %>%
        dplyr::filter(time == 4),
      crs = "lonlat"
    ), buf_dist
  )
  expect_false(
    length(
      pts_in_polys(
        terra::vect(pa_random %>%
          dplyr::filter(
            time_step == as.Date("1952-01-01")
          )), min_buffer
      )
    ) == 0
  )


  # now check that we return the right number of presences
  set.seed(123)
  pa_random <- sample_pseudoabs_time(locations,
    n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
    method = c("dist_min", buf_dist),
    return_pres = TRUE,
    time_buffer = y2d(1)
  )
  expect_true(table(pa_random$class)[1] == 4)
  expect_true(table(pa_random$class)[2] == n_pt * table(pa_random$class)[1])
})

test_that("sample_pseudoabs_time works with stars", {
  set.seed(123)

  grid_stars <- stars::st_as_stars(grid_raster, as_attributes = FALSE)
  d <- stars::st_dimensions(grid_stars)
  d$time$refsys <- terra::timeInfo(grid_raster)$step[1]
  stars::st_dimensions(grid_stars) <- d
  expect_no_error(sample_pseudoabs_time(locations,
    n = n_pt,
    raster = grid_stars,
    lubridate_fun = pastclim::ybp2date,
    method = c("dist_min", buf_dist),
    return_pres = FALSE
  ))
})

# nolint start
# sample code to plot points
# i <- 2
# plot(grid_raster[[i]],colNA="darkgray")
# polys(terra::as.polygons(grid_raster[[i]]))
# points(vect(locations %>% filter(time==i)), col="red", cex=2)
# points(terra::vect(pa_random %>%
#               dplyr::filter(time_step==as.Date("1952-01-01"))), col="blue")
# polys(min_buffer)
# nolint end

test_that("sample_pseudoabs_time treats time correctly", {
  # change time of raster to POSIX
  time(grid_raster) <- lubridate::date_decimal(time(grid_raster))
  expect_true(inherits(time(grid_raster), "POSIXct"))
  # this should work just fine
  set.seed(123)
  pa_random <- sample_pseudoabs_time(locations,
    n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
    method = c("dist_min", buf_dist),
    return_pres = FALSE
  )
  # we have the right number of pseudoabsences per time
  expect_true(all(table(locations$time) * n_pt == table(pa_random$time_step)))

  # Now change it to dates
  time(grid_raster) <- as.Date(time(grid_raster))
  expect_true(inherits(time(grid_raster), "Date"))
  set.seed(123)
  pa_random <- sample_pseudoabs_time(
    locations,
    n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
    method = c("dist_min", buf_dist),
    return_pres = FALSE
  )
  # we have the right number of pseudoabsences per time
  expect_true(all(table(locations$time) * n_pt == table(pa_random$time_step)))

  # now set the time to raw units to trigger error
  pastclim::time_bp(grid_raster) <- 0:4
  time(grid_raster) <- time(grid_raster)
  expect_true(timeInfo(grid_raster)$step == "raw")
  expect_error(
    sample_pseudoabs_time(locations,
      n = n_pt, raster = grid_raster, lubridate_fun = pastclim::ybp2date,
      method = c("dist_min", buf_dist),
      return_pres = FALSE
    ), "the units of the time axis of the raster are not defined"
  )
})
