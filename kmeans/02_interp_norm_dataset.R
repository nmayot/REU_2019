### Interp. and Norm. before Clustering analysis ---------------------------------
#
#
#

rm(list=ls()) # clear all variable

# --- Read dataset ----
folder_S <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/SeaWiFS/" # folder with data
folder_M <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/MODIS-Aqua/" # folder with data
folder_C <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/occci-v3.1/" # folder with data

filename_S <- "SW_climato_1997_2007_8D_CHL_coastfree.rdata" # Name of the dataset
filename_M <- "MODIS_climato_2007_2017_8D_CHL_coastfree.rdata" # Name of the dataset
filename_C1 <- "CCI_climato_1997_2007_8D_CHL_coastfree.rdata"
filename_C2 <- "CCI_climato_2007_2017_8D_CHL_coastfree.rdata"

load(file = paste(folder_S,filename_S,sep = "")) # Load dataset
load(file = paste(folder_M,filename_M,sep = "")) # Load dataset
load(file = paste(folder_C,filename_C1,sep = "")) # Load dataset
load(file = paste(folder_C,filename_C2,sep = "")) # Load dataset

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
    r_interp <- approx(1:nweeks,r_interp[c(24:46,1:23)],xout=1:nweeks,rule=1)$y # repeat for extrapolation (move summer in the middle)
    r_interp <- r_interp[c(24:46,1:23)] # move back winter in the middle of the time series
    CHL_max[i] <- max(r_interp) # save max value of the timeserie
    CHL_norm[i,] <- r_interp/CHL_max[i] # normalization (divide by maximal value)
  }
}

#--- Save dataset ----
dataset$CHL_norm <- CHL_norm # latitude of the MedSea map
dataset$CHL_max <- CHL_max # latitude of the MedSea map
dataset$gp <- which(!is.na(CHL_norm[,1])) # pixels (timeseries) without NA to be clusterized (gp = good points)

save(dataset, file = paste(folder_S,"SW_climato_1997_2007_8D_CHL_coastfree_norm.rdata",sep="")) # save rdata into the same folder
save(dataset, file = paste(folder_M,"MODIS_climato_2007_2017_8D_CHL_coastfree_norm.rdata",sep="")) # save rdata into the same folder
save(dataset, file = paste(folder_C,"CCI_climato_1997_2007_8D_CHL_coastfree_norm.rdata",sep="")) # save rdata into the same folder
save(dataset, file = paste(folder_C,"CCI_climato_2007_2017_8D_CHL_coastfree_norm.rdata",sep="")) # save rdata into the same folder

