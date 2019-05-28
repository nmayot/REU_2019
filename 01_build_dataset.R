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
library("ncdf4") # package to open NetCDF

folder <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/SeaWiFS/" # folder with data
filenames <- list.files(path = folder) # all filenames into this folder
nweeks <- 46 # there are 46 weeks of 8-day in one year
jdays <- seq(1,366,8) # julian day of the first day of each 8-day week (1, 9, 17..., 361)
years <- 1997:2007 # years of the climato
load("C:/Users/nmayot/Documents/PostDoc/data/satellite/SeaWiFS/mask_SW") # mask of the MedSea for SeaWiFS data

chl_clim <- c() # to save the CHL data

# open the data, for each week and year
for (j in jdays) {  
  
  chl_week <- c() # to save the CHL data of each year before average
  
  for (y in years) {
    
    fname <- filenames[substr(filenames,2,8) == paste(toString(y),sprintf("%03d",j),sep="")] # search filename by year and date
    
    if (length(fname) != 0) {
      filename <- list.files(path = folder, pattern = fname, full.names = T)  # search filename in directory
      nc <- nc_open(filename)
      
      # open latitudes and longitudes data only one time
      if (!exists("lon")) {
        lat <- ncvar_get(nc, "lat")
        lon <- ncvar_get(nc, "lon")
        
        # search in the NetCDF file the locations of pixel in a square around the MedSea 
        startlon <- min(which(lon >= -6 & lon <= 36.5))
        countlon <- abs(min(which(lon >= -6 & lon <= 36.5)) - max(which(lon >= -6 & lon <= 36.5))) + 1
        startlat <- min(which(lat >= 30 & lat <= 46))
        countlat <- abs(min(which(lat >= 30 & lat <= 46)) - max(which(lat >= 30 & lat <= 46))) + 1
        
        # lon and lat vectors for the map
        lon <- lon[lon >= -6 & lon <= 36.5]
        lat <- lat[lat >= 30 & lat <= 46]
        
        # lon and lat vectors for each pixel
        LON <- matrix(lon, length(lon), length(lat))
        LAT <- matrix(lat, length(lon), length(lat), byrow = TRUE)
        LON <- as.vector(LON)
        LAT <- as.vector(LAT)
      }
      
      # Subset the NetCDF file at the locations wanted
      chla <- ncvar_get(nc, "chlor_a", start = c(startlon,startlat), count = c(countlon,countlat))
      chla[mask_SW == 0] <- NA # pixel oustside MedSea removed
      chla <- as.vector(chla)
      
      # save into the matrix before weekly average
      chl_week <- cbind(chl_week, chla)
      
      # close NetCDF file
      nc_close(nc)
    }
    
  }
  chl_week <- rowMeans(chl_week, na.rm = FALSE) # weekly climatology
  chl_clim <- cbind(chl_clim, chl_week) # save into matrix (row = pixel, column = week)
}

#----- Build dataset and save
dataset <- c()
dataset$CHL <- chl_clim
dataset$lon <- LON
dataset$lat <- LAT
dataset$mask <- mask_SW
save(dataset, file = "C:/Users/nmayot/Documents/...rdata")

