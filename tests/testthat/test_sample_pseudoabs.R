# set up a small world
# small grid
library(terra)
grid_raster <- terra::rast(matrix(1:64, ncol=8,byrow=TRUE),
                    extent=terra::ext(c(-2,2,-2,2)),
                    crs="lonlat")
                    # crs="epsg:4326")
grid_raster[c(1:5,15,16,23,24)]<-NA

# locations (first isolated, two closer to each other)
locations <- data.frame(lon=c( 0.8,  1.9, 0.7),
                        lat=c(1.3, -1.8, -0.9),
                        id = 1:3)
min_buffer <- terra::buffer(terra::vect(locations, crs="lonlat"),60000)
max_buffer <- terra::buffer(terra::vect(locations, crs="lonlat"),90000)


# points in poygons function
#(from https://stackoverflow.com/questions/72384038/point-in-polygon-using-terra-package-in-r)
pts_in_polys <- function(pts, polys){
  e <- terra::extract(polys, pts)
  e[!is.na(e[,2]), 1]
}

test_that("sample_pseudoabs samples in the right places", {
  set.seed(123)
  pa_random <- sample_pseudoabs(locations, n=25, raster=grid_raster,
                                return_pres = FALSE)
  # we should now check that some points are in the buffers
  expect_true(length(pts_in_polys(terra::vect(pa_random), min_buffer))>0)
  expect_true(length(pts_in_polys(terra::vect(pa_random), max_buffer))>0)
  # there should be no presences
  expect_true(unique(pa_random$class)=="pseudoabs")

  # now use a minimum buffer
  set.seed(123)
  pa_min <- sample_pseudoabs(locations, n=15, raster=grid_raster,
                             method=c("dist_min",60000),
                                return_pres = FALSE)
  # none should be within the minimum buffer
  expect_true(length(pts_in_polys(terra::vect(pa_min), min_buffer))==0)

  # now use a maximum buffer
  set.seed(123)
  pa_max <- sample_pseudoabs(locations, n=5, raster=grid_raster,
                                            method=c("dist_max",90000),
                                            return_pres = FALSE)
  # all are within the max buffer
  expect_true(length(pts_in_polys(terra::vect(pa_max), max_buffer))==nrow(pa_max))

  # and now use a disc
  set.seed(123)
  pa_disc <- sample_pseudoabs(locations, n=5, raster=grid_raster,
                             method=c("dist_disc",60000, 90000),
                             return_pres = FALSE)
  # all are within the max buffer
  expect_true(length(pts_in_polys(terra::vect(pa_disc), max_buffer))==nrow(pa_disc))
  # none should be within the minimum buffer
  expect_true(length(pts_in_polys(terra::vect(pa_disc), min_buffer))==0)

  pa_pres <- sample_pseudoabs(locations, n=25, raster=grid_raster)
  expect_true(all(levels(pa_pres$class)==c("presence","pseudoabs")))

  # now confirm that it all works if we use an sf object
  set.seed(123)
  locations_sf <-  sf::st_as_sf(locations, coords=c("lon","lat")) %>% sf::st_set_crs(4326)
  pa_random_sf <- sample_pseudoabs(locations_sf, n=25, raster=grid_raster,
                                return_pres = FALSE)
  expect_true(inherits(pa_random_sf,"sf"))
  expect_true(inherits(pa_random_sf,"data.frame")) # it is also a df!
  expect_true(all(pa_random$id ==pa_random_sf$id))

  # test error messages
  expect_error(sample_pseudoabs(locations_sf, n=25, raster=grid_raster,method="blah"),
               "method has to be")
  expect_error(sample_pseudoabs(locations_sf, n=25, raster=grid_raster,method=c("blah",25)),
               "method has to be")
})

# sample code to plot points in and buffers
# plot(grid_raster,colNA="darkgray")
# polys(terra::as.polygons(grid_raster))
# points(vect(locations), col="red", cex=2)
# points(vect(pa_disc), col="blue", cex=2)
# polys(min_buffer)
# polys(max_buffer)