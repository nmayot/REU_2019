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


folder <- "C:/Users/mricc/OneDrive/Documents/2019 REU/DATA/cclold" # folder with data
filename <- "CCI_ALL-v3.1-8DAY_1997_2007.nc" #"CCI_ALL-v3.1-8DAY_2007_2017.nc"
filenames <- list.files(path = folder) # all filenames into this folder
filename <- list.files(path = folder, pattern = filename, full.names = T)  # search filename in directory
nc <- nc_open(filename)

nweeks <- 46 # there are 46 weeks of 8-day in one year
jdays <- seq(1,366,8) # julian day of the first day of each 8-day week (1, 9, 17..., 361)
years <- 1997:2007 # years of the climato
# FinalYear <- 2007

load("C:/Users/mricc/OneDrive/Documents/2019 REU/DATA/cclold/mask_cci.Rdata") # mask of the MedSea for SeaWiFS data

# Open the time variable > Convert it to julian days > Get the years
dtime <- ncvar_get(nc, "time")
dtime <- as.Date(as.Date("01-01-1970","%d-%m-%Y")+dtime)
jtime <- dtime - as.Date(paste("01-01-", format(dtime, "%Y"), sep=""),"%d-%m-%Y") + 1
idbadjtime <- which(!is.element(jtime, jdays))
if (length(idbadjtime) != 0) {
  badjtime <- as.numeric(jtime[!is.element(jtime, jdays)])
  jtime_modified <- jtime
  for (i in 1:length(badjtime)) {
    c <- jdays[hist(badjtime[i], breaks = jdays, right = F, plot = F)$counts == 1]
    jtime_modified[idbadjtime[i]] <- c
  }
  dtime_modified <- as.Date(paste("01-01-", format(dtime, "%Y"), sep=""),"%d-%m-%Y")+jtime_modified-1
  dtime <- dtime_modified
  jtime <- jtime_modified
}
yyyy <- as.numeric(format(dtime, "%Y"))


# Open the latitudes, longitudes and chlor_a variable
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
chlor_a <- ncvar_get(nc, "chlor_a") # 3D matrice of satellite images = lon x lat x time (1020 x 385 x 453)
nc_close(nc)

# lon and lat vectors for each pixel
LON <- matrix(lon, length(lon), length(lat))
LAT <- matrix(lat, length(lon), length(lat), byrow = TRUE)
LON <- as.vector(LON)
LAT <- as.vector(LAT)


#-- build the climatology
chl_clim <- c() # to save the CHL data

# open the data, for each week and year
for (j in jdays) {  
  
  chl_week <- c() # to save the CHL data of each year before average
  
  for (y in years) {
    
    ftime <- which(yyyy == y & jtime == j) # search time wanted by year and julian day
    
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

#----- Build the dataset and save it
dataset <- c()
dataset$CHL <- chl_clim # rows = pixels, column = 8-day weeks (x46)
dataset$LON <- LON # rows = longitude of each pixel
dataset$LAT <- LAT # rows = latitude of each pixel
dataset$mask <- mask_cci # mask of the MedSea used
dataset$lon_map <- lon # longitude of the MedSea map
dataset$lat_map <- lat # latitude of the MedSea map
save(dataset, file = paste(folder,"CCI_climato_1997_2007_8D_CHL.rdata",sep="")) # save rdata into the same folder

