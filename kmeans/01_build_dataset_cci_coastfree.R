# Prepare the dataset, with the climatological Chla values, for the kmeans analysis
#
#
# --- Create dataset --------------------------------------------------
#
# dataset == List of 3
#   $ CHL: num [1:..., 1:46]
#   $ lat : num [1:...]
#   $ lon : num [1:...]
#
#

rm(list=ls()) # clear all variable
library("ncdf4") # package to open NetCDF

folder <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/occci-v3.1/" # folder with data
filename_1 <- "CCI_ALL-v4.0-8DAY_1997_2007.nc" #"CCI_ALL-v3.1-8DAY_2007_2017.nc"
filename_2 <- "CCI_ALL-v4.0-8DAY_2007_2017.nc" #"CCI_ALL-v3.1-8DAY_2007_2017.nc"

nweeks <- 46 # there are 46 weeks of 8-day in one year
jdays <- seq(1,366,8) # julian day of the first day of each 8-day week (1, 9, 17..., 361)
years <- 2007:2017 # years of the climato

load(file = paste(folder,"mask_cci.Rdata",sep="")) # mask of the MedSea for SeaWiFS data


#---- Open the time variable
dtime  <- c()
jtime  <- c()
yyyy   <- c()
decade <- c()

for (f in 1:2) {
  
  nc <- nc_open(file = paste(folder,eval(parse(text=paste("filename_",f,sep=""))),sep=""))
  
  dt <- ncvar_get(nc, "time")
  dt <- as.Date(as.Date("01-01-1970","%d-%m-%Y")+dt)
  jt <- dt - as.Date(paste("01-01-", format(dt, "%Y"), sep=""),"%d-%m-%Y") + 1
  idbadjtime <- which(!is.element(jt, jdays))
  if (length(idbadjtime) != 0) {
    badjtime <- as.numeric(jt[!is.element(jt, jdays)])
    jtime_modified <- jt
    for (i in 1:length(badjtime)) {
      c <- jdays[hist(badjtime[i], breaks = jdays, right = F, plot = F)$counts == 1]
      jtime_modified[idbadjtime[i]] <- c
    }
    dtime_modified <- as.Date(paste("01-01-", format(dt, "%Y"), sep=""),"%d-%m-%Y")+jtime_modified-1
    dt <- dtime_modified
    jt <- jtime_modified
  }
  
  dtime <- c(dtime, as.Date(dt))
  jtime <- c(jtime, jt)
  yyyy <- c(yyyy, as.numeric(format(dt, "%Y")))
  decade <- c(decade, rep(f,length(dt)))
  
  nc_close(nc)
}
class(dtime) <- "Date"


#---- Open the latitudes, longitudes and chlor_a variable --------------------
nc <- nc_open(file = paste(folder,filename_1,sep=""))
chlor_a_1 <- ncvar_get(nc, "chlor_a") # 3D matrice of satellite images = lon x lat x time (1020 x 385 x 453)
nc_close(nc)

nc <- nc_open(file = paste(folder,filename_2,sep=""))
chlor_a_2 <- ncvar_get(nc, "chlor_a") # 3D matrice of satellite images = lon x lat x time (1020 x 385 x 453)

# lon and lat vectors for each pixel
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
LON <- matrix(lon, length(lon), length(lat))
LAT <- matrix(lat, length(lon), length(lat), byrow = TRUE)
LON <- as.vector(LON)
LAT <- as.vector(LAT)
nc_close(nc)


#---- Build climatology ------------------------------------------------------
chl_clim <- c() # to save the CHL data

# open the data, for each week and year
for (j in jdays) {  
  
  chl_week <- c() # to save the CHL data of each year before average
  
  for (y in years) {
    
    if (y < 2007) {
      chlor_a <- chlor_a_1
      d <- 1
    } else if (y > 2007) {
      chlor_a <- chlor_a_2
      d <- 2
    } else if (y == 2007 & j <= 177) {
      chlor_a <- chlor_a_1
      d <- 1
    } else if (y == 2007 & j > 177) {
      chlor_a <- chlor_a_2
      d <- 2
    } else {
      stop('issue with file name')
    }
    
    
    ftime <- which(yyyy[decade == d] == y & jtime[decade == d] == j) # search time wanted by year and julian day
    
    if (length(ftime) != 0) {
      # Subset the chlor_a variable at the time wanted
      chla <- chlor_a[,,ftime]
      chla[mask_cci == 0] <- NA # pixel oustside MedSea removed
      chla <- as.vector(chla)
      
      # save into the matrix before weekly average
      chl_week <- cbind(chl_week, chla)
    }
    
  }
  chl_week <- rowMeans(chl_week, na.rm = TRUE) # weekly climatology
  chl_clim <- cbind(chl_clim, chl_week) # save into matrix (row = pixel, column = week)
}
dimnames(chl_clim) <- c() # remove dimension names (e.g., colnames)


# ---- regrid -----------------------------------------------------------------
source("C:/Users/nmayot/Documents/PostDoc/teaching supervise/REU 2019/scripts/REU_2019/00_functions.R") # functions

folder_S <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/SeaWiFS/" # folder with data
filename_S <- "SW_climato_1997_2007_8D_CHL_coastfree.rdata" # Name of the dataset
load(file = paste(folder_S,filename_S,sep = "")) # Load dataset
chl_clim <- regrid(dataset$LON, dataset$LAT, dataset$lon_map, dataset$lat_map, LON, LAT, chl_clim)
LON <- dataset$LON
LAT <- dataset$LAT
lon <- dataset$lon_map
lat <- dataset$lat_map

#----- remove pixel with a bathy shallower than 200 m -------------------------
# regrid bathy
file <- "C:/Users/nmayot/Documents/PostDoc/data/etopo/med_1min.xyz.gz"
bathy_med <- read.table(file, col.names=c("lon", "lat", "z"))
LON_z <- bathy_med$lon
LAT_z <- bathy_med$lat
z <- bathy_med$z
bathy <- regrid(LON, LAT, lon, lat, LON_z, LAT_z, as.matrix(z))

# remove if shallower than 200m
chl_clim[bathy >= -200,] <- NA
chl_clim[is.na(chl_clim)] <- NA # replace NaN by NA

#----- Build the dataset and save it ------------------------------------------
dataset <- c()
dataset$CHL <- chl_clim # rows = pixels, column = 8-day weeks (x46)
dataset$LON <- LON # rows = longitude of each pixel
dataset$LAT <- LAT # rows = latitude of each pixel
dataset$mask <- mask_cci # mask of the MedSea used
dataset$lon_map <- lon # longitude of the MedSea map
dataset$lat_map <- lat # latitude of the MedSea map
save(dataset, file = paste(folder,"CCI_climato_2007_2017_8D_CHL_coastfree.rdata",sep="")) # save rdata into the same folder

