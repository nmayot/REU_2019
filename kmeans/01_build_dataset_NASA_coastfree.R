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
source("C:/Users/nmayot/Documents/PostDoc/teaching supervise/REU 2019/scripts/REU_2019/00_functions.R") # functions

folder_S <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/SeaWiFS/" # folder with data
filenames <- list.files(path = folder_S) # all filenames into this folder

folder_M <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/MODIS-Aqua/" # folder with data
filenames <- c(filenames, list.files(path = folder_M)) # all filenames into this folder

nweeks <- 46 # there are 46 weeks of 8-day in one year
jdays <- seq(1,366,8) # julian day of the first day of each 8-day week (1, 9, 17..., 361)
jdays <- jdays[c(24:46,1:23)] # summer months at the end and beginning of the time series
years <- 1997:2017 # years of the climato
load(paste(folder_M,"mask_MODIS.Rdata",sep="")) # mask of the MedSea for SeaWiFS data

chl_clim <- c() # to save the CHL data

# open the data, for each week and year
for (j in jdays) {  
  
  chl_week <- c() # to save the CHL data of each year before average
  
  for (y in years) {
    
    if (y < 2007) {
      folder <- folder_S
    } else if (y > 2007) {
      folder <- folder_M
    } else if (y == 2007 & j <= 177) {
      folder <- folder_S
    } else if (y == 2007 & j > 177) {
      folder <- folder_M
    } else {
      stop('issue with folder name')
    }
    
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
  chl_week <- rowMeans(chl_week, na.rm = TRUE) # weekly climatology
  chl_clim <- cbind(chl_clim, chl_week) # save into matrix (row = pixel, column = week)
}
dimnames(chl_clim) <- c() # remove dimension names (e.g., colnames)

#----- remove pixel with a bathy shallower than 200 m
# regrid bathy
file <- "C:/Users/nmayot/Documents/PostDoc/data/etopo/med_1min.xyz.gz"
bathy_med <- read.table(file, col.names=c("lon", "lat", "z"))
LON_z <- bathy_med$lon
LAT_z <- bathy_med$lat
z <- bathy_med$z
bathy <- regrid(LON, LAT, lon, lat, LON_z, LAT_z, z)

# remove if shallower than 200m
chl_clim[bathy >= -200,] <- NA
chl_clim[is.na(chl_clim)] <- NA # replace NaN by NA

#----- Build the dataset and save it
dataset <- c()
dataset$CHL <- chl_clim # rows = pixels, column = 8-day weeks (x46)
dataset$LON <- LON # rows = longitude of each pixel
dataset$LAT <- LAT # rows = latitude of each pixel
dataset$mask <- mask_SW # mask of the MedSea used
dataset$lon_map <- lon # longitude of the MedSea map
dataset$lat_map <- lat # latitude of the MedSea map
if (min(years) == 1997 & max(years) == 2017) {
  save(dataset, file = paste(folder_M,"NASA_climato_1997_2017_8D_CHL_coastfree.rdata",sep="")) # save rdata into the same folder
} else if (min(years) == 1997 & max(years) == 2007) {
  save(dataset, file = paste(folder_S,"SW_climato_1997_2007_8D_CHL_coastfree.rdata",sep="")) # save rdata into the same folder
} else if (min(years) == 2007 & max(years) == 2017) {
  save(dataset, file = paste(folder_M,"MODIS_climato_2007_2017_8D_CHL_coastfree.rdata",sep="")) # save rdata into the same folder
}