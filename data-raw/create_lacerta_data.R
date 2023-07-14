# download presences
library(rgbif)
# download file
occ_download_get(key = "0068808-230530130749713", path=tempdir())
# unzip file
unzip(file.path(tempdir(),"0068808-230530130749713.zip"), overwrite=TRUE,
      exdir=tempdir(), unzip="internal")
# read file
#distrib <- data.table::fread(file.path(tempdir(),"0068808-230530130749713.csv"))
distrib <- read.table(file.path(tempdir(),"0068808-230530130749713.csv"),
                    row.names = NULL)
distrib <- readr::read_delim(file.path(tempdir(),"0068808-230530130749713.csv"))

# keep the necessary columns
lacerta <- distrib[,c("gbifID","decimalLatitude", "decimalLongitude")]
names(lacerta) <- c("ID","latitude",  "longitude")

usethis::use_data(lacerta, overwrite=TRUE)
#saveRDS(lacerta, file="./inst/extdata/lacerta_coords.RDS")


###############################
### landmask
###############################

library(pastclim)
land_mask <- get_land_mask(time_ce=1985, dataset="WorldClim_2.1_10m")
# Iberia peninsula extension
iberia_poly <- terra::vect("POLYGON((-9.8 43.3,-7.8 44.1,-2.0 43.7,3.6 42.5,3.8 41.5,1.3 40.8,0.3 39.5,0.9 38.6,-0.4 37.5,-1.6 36.7,-2.3 36.3,-4.1 36.4,-4.5 36.4,-5.0 36.1,-5.6 36.0,-6.3 36.0,-7.1 36.9,-9.5 36.6,-9.4 38.0,-10.6 38.9,-9.5 40.8,-9.8 43.3))")

crs(iberia_poly)<-"lonlat"
# crop the extent
land_mask <- crop(land_mask, iberia_poly)
# and mask to the polygon
land_mask <- mask(land_mask, iberia_poly)
gdal(warn=3)
writeCDF(land_mask, "./inst/extdata/lacerta_land_mask.nc",
         compression=9,split=TRUE, overwrite=TRUE)
# fix time axis (this is a workaround if we open the file with sf)
nc_in <- ncdf4::nc_open("./inst/extdata/lacerta_land_mask.nc", write=TRUE)
ncdf4::ncatt_put(nc_in, varid="time", attname="axis", attval = "T")
ncdf4::nc_close(nc_in)


climate_vars <- get_vars_for_dataset("WorldClim_2.1_10m")
climate_present<-pastclim::region_slice(time_ce = 1985,
                                        bio_variables = climate_vars,
                                        data="WorldClim_2.1_10m",
                                        crop=iberia_poly)

writeCDF(climate_present,"./inst/extdata/lacerta_climate_present_10m.nc",
         compression=9,split=TRUE, overwrite=TRUE)
# fix time axis (this is a workaround if we open the file with sf)
nc_in <- ncdf4::nc_open("./inst/extdata/lacerta_climate_present_10m.nc", write=TRUE)
ncdf4::ncatt_put(nc_in, varid="time", attname="axis", attval = "T")
ncdf4::nc_close(nc_in)


vars_uncor<-c("bio15", "bio05", "bio13", "bio06")

climate_future<-pastclim::region_slice(time_ce = 2090,
                                       bio_variables = vars_uncor,
                                       data="WorldClim_2.1_HadGEM3-GC31-LL_ssp245_10m",
                                       crop=iberia_poly)
writeCDF(climate_present,"./inst/extdata/lacerta_climate_future_10m.nc",
         compression=9,split=TRUE, overwrite=TRUE)
# fix time axis (this is a workaround if we open the file with sf)
nc_in <- ncdf4::nc_open("./inst/extdata/lacerta_climate_future_10m.nc", write=TRUE)
ncdf4::ncatt_put(nc_in, varid="time", attname="axis", attval = "T")
ncdf4::nc_close(nc_in)

#####

