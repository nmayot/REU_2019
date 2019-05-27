# Prepare the dataset, with the climatological Chla values, for the kmeans analysis
#
#
# --- Create dataset --------------------------------------------------
#
# dataset == List of 3
#   $ CHL: num [1:..., 1:46]
#   $ lat : num [1:...]
#   $ lon : num [1:...]

rm(list=ls()) # clear all variable
library("ncdf4") # package to open NetCDF file

folder <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/SeaWiFS/"

nweeks <- 46 # there are 46 8-day weeks in one year
jdays <- seq(1,
chl_clim <- c()

for (n in 1:nweeks) {
  # open files and create a weekly climatology
  chl_week <- c()
  filename <- paste("...-", sprintf("%02d",n),".nc",sep="")
  filename <- list.files(path = folder, pattern = filename, full.names = T) # filename
  nc <- nc_open(filename)
  
  # open latitudes and longitudes data only one time
  if (n == 1) {
    lat <- ncvar_get(nc, "lat")
    lon <- ncvar_get(nc, "lon")
    
    LON <- matrix(rep(lon,length(lat)), ncol = length(lat), byrow = F)
    LAT <- matrix(rep(lat,length(lon)), ncol = length(lat), byrow = T)
    
    LON <- as.vector(LON)
    LAT <- as.vector(LAT)
  }
  
  # Subset at the location wanted
  CHL <- ncvar_get(nc, varid="Chl...")
  CHL <- as.vector(CHL)
  chl_clim <- cbind(chl_clim, CHL) # from matrix to vector
}

#----- Build dataset and save
dataset <- c()
dataset$CHL <- chl_clim
dataset$lon <- LON
dataset$lat <- LAT
dataset$mask <- mask_SW
save(dataset, file = "C:/Users/nmayot/Documents/...rdata")

