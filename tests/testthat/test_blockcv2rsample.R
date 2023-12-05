library(blockCV)
test_that("blockcv2rsample conversion", {
  
  # we use examples from the blockcv library for spatial blocks
  points <- read.csv(system.file("extdata/", "species.csv", package = "blockCV"))
  pa_data <- sf::st_as_sf(points, coords = c("x", "y"), crs = 7845)
  sb1 <- cv_spatial(
    x = pa_data,
    column = "occ", # the response column (binary or multi-class)
    k = 5, # number of folds
    size = 350000, # size of the blocks in metres
    selection = "random", # random blocks-to-fold
    iteration = 10, 
    report = FALSE,
    progress = FALSE
  ) # find evenly dispersed folds
  sb1_rsample <- blockcv2rsample(sb1, pa_data)
  expect_true(inherits(sb1_rsample,"spatial_rset"))


# load raster data
path <- system.file("extdata/au/", package = "blockCV")
files <- list.files(path, full.names = TRUE)
covars <- terra::rast(files)
#'
# spatial clustering
set.seed(6)
sc <- cv_cluster(x = pa_data,
                 column = "occ", # optional; name of the column with response
                 k = 5, 
                 report = FALSE)
sc_rsample <- blockcv2rsample(sc, pa_data)
expect_true(inherits(sc_rsample,"spatial_rset"))
#'
# environmental clustering
set.seed(6)
ec <- cv_cluster(r = covars, # if provided will be used for environmental clustering
                 x = pa_data,
                 column = "occ", # optional; name of the column with response
                 k = 5,
                 scale = TRUE, 
                 report = FALSE)
ec_rsample <- blockcv2rsample(ec, pa_data)
path <- system.file("extdata/au/bio_5.tif", package = "blockCV")
expect_true(inherits(ec_rsample,"spatial_rset"))

# give error for unsuppored mode in blockcv  
covar <- terra::rast(path)  
nndm <- cv_nndm(x = pa_data,
                column = "occ", # optional
                r = covar,
                size = 350000, # size in metres no matter the CRS
                num_sample = 10,
                sampling = "regular",
                min_train = 0.1,
                plot = FALSE, 
                report = FALSE)
expect_error(blockcv2rsample(nndm, pa_data),
             "this function does support this object type")



})
