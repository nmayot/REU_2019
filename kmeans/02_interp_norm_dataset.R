### Interp. and Norm. before Clustering analysis ---------------------------------
#
#
# --- Open dataset --------------------------------------------------
#
# dataset == List of 6
#   $ CHL    : num [1:97920, 1:46] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
#   $ LON    : num [1:97920] -5.96 -5.87 -5.79 -5.71 -5.62 ...
#   $ LAT    : num [1:97920] 46 46 46 46 46 ...
#   $ mask   : num [1:510, 1:192] 0 0 0 0 0 0 0 0 0 0 ...
#   $ lon_map: num [1:510(1d)] -5.96 -5.87 -5.79 -5.71 -5.62 ...
#   $ lat_map: num [1:192(1d)] 46 45.9 45.8 45.7 45.6 ...

rm(list=ls()) # clear all variable

#--- Read dataset ----
folder <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/MODIS-Aqua/" # folder with data
filename <- "NASA_climato_1997_2017_8D_CHL.rdata" # Name of the dataset

load(file = paste(folder,filename,sep = "")) # Load dataset

CHL <- dataset$CHL # climatological dataset (row = pixel, column = weeks)
nweeks <- dim(CHL)[2] # number of weeks (= number of columns)


# --- Interpollation and Normalization ----------------------------------------------
#
#
# Linear interpolation to remove NA, and divide each time series by its maximal value 

CHL_norm <- array(dim = dim(CHL)) # same dimension as the non-normalized dataset
CHL_max <- array(dim = c(dim(CHL)[1],1)) # maximal value for each time series (one timeserie per pixel)

# loop over each pixel (= timeserie)
for (i in 1:nrow(CHL)) {

  r <- CHL[i,] # row timeserie
  n <- which(!is.na(r)) # locations of non-NAs
  diff_n <- n[2:length(n)] - n[1:length(n)-1] # number of weeks inbetween non-NAs
  
  if (length(n) >= round(nweeks/2) & length(which(diff_n >= 5)) == 0) { # at least a half weeks of data with a data point every 4 weeks 
    r_interp <- approx(1:nweeks,r,xout=1:nweeks,rule=2)$y # linearly interpolate (extrapolation repeat the closest known values)
    CHL_max[i] <- max(r_interp) # save max value of the timeserie
    CHL_norm[i,] <- r_interp/CHL_max[i] # normalization (divide by maximal value)
  }
}

#--- Save dataset ----
dataset$CHL_norm <- CHL_norm # latitude of the MedSea map
dataset$CHL_max <- CHL_max # latitude of the MedSea map
dataset$gp <- which(!is.na(CHL_norm[,1])) # pixels (timeseries) without NA to be clusterized (gp = good points)
save(dataset, file = paste(folder,"MODIS_climato_2007_2017_8D_CHL_CHLnorm.rdata",sep="")) # save rdata into the same folder

