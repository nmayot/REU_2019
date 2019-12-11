# Prepare the dataset, regridded climatological values with coastal areas removed
#
# Grid = the one of SeaWiFS (9 km)
# Coastal areas removed = pixels where bathy >= -200m (data from Ayata - etopo? - 1 min)
#
# --- Create dataset --------------------------------------------------
# List of 6
#     $ CHL     : num [1:97920, 1:46]
#     $ LON     : num [1:97920]
#     $ LAT     : num [1:97920]
#     $ lon_map : num [1:510(1d)]
#     $ lat_map : num [1:192(1d)]
#     $ bathy   : num [1:97920]


rm(list=ls()) # clear all variable
library("ncdf4") # package to open NetCDF
#regrid function
regrid <- function(LON_px, LAT_px, lon_px, lat_px, LON_z, LAT_z, z) {
  #
  # regrid by averaging value z according to satellite grid
  #
  #---INPUTS
  #
  # LON_px & LAT_px: all paired of longitudes and latitudes, one paire per pixel
  # lon_px & lat_px: unique values of longitudes and latitudes ( length(lat_px) * length(lat_px) = length(LON_px) = length(LAT_px) )
  # z: values of the variable to be regrided
  # LON_z & LAT_z: all paired of longitudes and latitudes, one paire per value of z
  #
  #
  #---OUTPUTS
  #
  # z_px: z values for each wanted pixel
  #
  #
  #---INFO
  #
  # The grid of z should has a higher resolution than the one from satellite, ans similar borders (max/min values of lon and lat)
  #
  
  id_LON <- sapply(LON_z, function(x) which.min(abs(x-lon_px))) # index of the nearest lon
  id_LAT <- sapply(LAT_z, function(x) which.min(abs(x-lat_px))) # index of the nearest lat
  
  id_LON_px <- sapply(LON_px, function(x) which.min(abs(x-lon_px))) # index of the nearest lon
  id_LAT_px <- sapply(LAT_px, function(x) which.min(abs(x-lat_px))) # index of the nearest lat
  
  wh <- nchar(trunc(length(lon_px))) + 1 # the maximum number of digits
  grp <- paste(formatC(id_LON, width=wh, flag="0"),formatC(id_LAT, width=wh, flag="0"),sep="") # create one index from two indices (id_lon and id_lat)
  grp_map <- paste(formatC(id_LON_px, width=wh, flag="0"),formatC(id_LAT_px, width=wh, flag="0"),sep="") # create one index from two indices
  
  # regrid
  for (w in 1:dim(z)[2]) {
    z_regridded <- aggregate(z[,w], list(grp), FUN = mean, na.rm=T) # average value of z per paired of lat/lon (px grid)
    id_z <- match(z_regridded[,1],grp_map) # which value correspond to wanted pixel
    if (!exists("z_px")) {
      z_px <- array(dim=c(length(id_z),dim(z)[2]))
    }
    z_px[id_z,w] <- z_regridded[,2]
  }
  return(z_px)
}

#----- read the data
folder <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/MedOC4AD4/" # folder with data
filenames <- list.files(path = folder) # all filenames into this folder
nweeks <- 46 # there are 46 weeks of 8-day in one year
jdays <- seq(1,366,8) # julian day of the first day of each 8-day week (1, 9, 17..., 361)
years <- 1997:2017 # 2years of the climato

first_y <- years[1]
final_y <- years[length(years)]
first_j <- 185
final_j <- 177

chl_clim <- c() # to save the CHL data

# open the data, for each week and year
for (j in jdays) {  
  print(j)
  chl_week <- c() # to save the CHL data of each year before average
  
  for (y in years) {
    
    # Determine if these year and jday should be considered
    if (y == first_y & j >= first_j) { # In case of the first year
      flag_time <- TRUE
    } else if(y == final_y & j <= final_j) { # In case of the final year
      flag_time <- TRUE
    } else if(y > first_y & y < final_y) { # In case of inbetween first and final years
      flag_time <- TRUE
    } else {
      flag_time <- FALSE
    }
    
    if (flag_time == TRUE) {
      print(y)
      dtime <- (as.Date(paste("01-01-", y, sep=""),"%d-%m-%Y") + j) - 1
      filename <- list.files(path = paste(folder, y, "/", sep=""), pattern = paste(format(dtime,"%Y%m%d"),"_8d",sep=""), full.names = T) # search filename by year and date
      
      if (length(filename) != 0) {
        nc <- nc_open(filename)
        
        # open latitudes and longitudes data only one time
        if (!exists("lon")) {
          lat <- ncvar_get(nc, "lat")
          lon <- ncvar_get(nc, "lon")
          
          # lon and lat vectors for each pixel
          LON <- matrix(lon, length(lon), length(lat))
          LAT <- matrix(lat, length(lon), length(lat), byrow = TRUE)
          LON <- as.vector(LON)
          LAT <- as.vector(LAT)
        }
        
        # Subset the NetCDF file at the locations wanted
        chla <- ncvar_get(nc, "CHL")
        chla <- as.vector(chla)
        
        # save into the matrix before weekly average
        chl_week <- cbind(chl_week, chla)
        
        # close NetCDF file
        nc_close(nc)
      }
      
    }
  }
  chl_week <- rowMeans(chl_week, na.rm = TRUE) # weekly climatology
  chl_clim <- cbind(chl_clim, chl_week) # save into matrix (row = pixel, column = week)
}
dimnames(chl_clim) <- c() # remove dimension names (e.g., colnames)

#----- Regrid
load("C:/Users/nmayot/Documents/PostDoc/data/satellite/SeaWiFS/SeaWiFS_climato_1997_2007_8D_CHL_CHLnorm.rdata") # load dataset (row = pixels, column = variables)
dataset_SW <- dataset
rm(dataset)

LON_z <- LON
LAT_z <- LAT
LON <- dataset_SW$LON
LAT <- dataset_SW$LAT
lon <- dataset_SW$lon
lat <- dataset_SW$lat

chl_clim_new <- c()
for (w in 1:46) {
  z <- chl_clim[,w]
  chl_clim_new <- regrid(LON, LAT, lon, lat, LON_z, LAT_z, z)
}

########################################################
rm(list=setdiff(ls(), c("chl_clim", "dataset_SW")))

#----- Bathy
file <- "C:/Users/nmayot/Documents/PostDoc/data/etopo/med_1min.xyz.gz"
d <- read.table(file, col.names=c("lon", "lat", "z"))
LON <- d$lon
LAT <- d$lat

# prepare for regrid
lon_map <- dataset_SW$lon_map # longitudes of the new map (of the wanted grid)
lat_map <- dataset_SW$lat_map # latitudes of the new map (of the wanted grid)
LON_map <- dataset_SW$LON # all longitudes of all pixels the new map (of the wanted grid)
LAT_map <- dataset_SW$LAT # all latitudes of all pixels the new map (of the wanted grid)

id_LON <- sapply(LON, function(x) which.min(abs(x-lon_map))) # index of the nearest lon
id_LAT <- sapply(LAT, function(x) which.min(abs(x-lat_map))) # index of the nearest lat

id_LON_map <- sapply(LON_map, function(x) which.min(abs(x-lon_map))) # index of the nearest lon
id_LAT_map <- sapply(LAT_map, function(x) which.min(abs(x-lat_map))) # index of the nearest lat

wh <- nchar(trunc(abs(length(lon_map)))) + 1 # the maximum number of digits
grp <- paste(formatC(id_LON, width=wh, flag="0"),formatC(id_LAT, width=wh, flag="0"),sep="") # create one index from two indices (id_lon and id_lat)
grp_map <- paste(formatC(id_LON_map, width=wh, flag="0"),formatC(id_LAT_map, width=wh, flag="0"),sep="") # create one index from two indices (id_lon and id_lat)

# regrid
z_regridded <- aggregate(d$z, list(grp), FUN = mean, na.rm=T)
id_z <- match(z_regridded[,1],grp_map)
z_new <- array(dim=length(id_z))
z_new[id_z] <- z_regridded[,2]
bathy <- z_new
rm(list=setdiff(ls(), c("chl_clim", "dataset_SW", "bathy")))

#----- remove pixel with a bathy shallower than 200 m
chl_clim[bathy >= -200,] <- NA
chl_clim[is.na(chl_clim)] <- NA # replace NaN by NA
rm(list=setdiff(ls(), c("chl_clim", "dataset_SW", "bathy")))

#----- Build the dataset and save it
dataset <- c()
dataset$CHL <- chl_clim # rows = pixels, column = 8-day weeks (x46)
dataset$LON <- dataset_SW$LON # rows = longitude of each pixel
dataset$LAT <- dataset_SW$LAT # rows = latitude of each pixel
dataset$lon_map <- dataset_SW$lon_map # longitude of the MedSea map
dataset$lat_map <- dataset_SW$lat_map # latitude of the MedSea map
dataset$bathy <- bathy
save(dataset, file = "C:/Users/nmayot/Documents/PostDoc/data/satellite/MedOC4AD4/MedOC_clim_1997_2017_8D_9km_CHL_CoastFree.rdata") # save rdata into the same folder

