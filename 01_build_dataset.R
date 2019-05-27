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
filenames <- list.files(path = folder)
nweeks <- 46 # there are 46 8-day weeks in one year
jdays <- seq(1,366,8) # julian day of the first day of each 8-day week (1, 9, 17..., 361)
years <- 1997:2007 # years of the climato

chl_clim <- c() # to save the CHL data

# open the data, for each year and week
for (y in years) {  
  
  chl_week <- c() # to save the CHL data of each year before average
  
  for (j in jdays) {
    
    # open files and create a weekly climatology
    chl_week <- c()
    fname <- filenames[substr(filenames,2,8) == paste(toString(y),sprintf("%03d",j),sep="")] # search filename by year and date
    filename <- list.files(path = folder, pattern = fname, full.names = T)  # search filename in directory
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
    
  }
  chl_clim <- cbind(chl_clim, CHL) # from matrix to vector
}
#----- Build dataset and save
dataset <- c()
dataset$CHL <- chl_clim
dataset$lon <- LON
dataset$lat <- LAT
dataset$mask <- mask_SW
save(dataset, file = "C:/Users/nmayot/Documents/...rdata")

