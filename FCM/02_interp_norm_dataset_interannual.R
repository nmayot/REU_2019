### Interp. and Norm. before Clustering analysis ---------------------------------
#
#
# --- Used dataset --------------------------------------------------
#


rm(list=ls()) # clear all variable

#--- Read dataset ----
folder <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/MedOC4AD4/" # folder with data
filename <- "MedOC_clim_1997_2017_8D_9km_CHL_CoastFree.rdata" # Name of the dataset

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
cpt <- 0 # counter of timeserie
for (i in 1:nrow(CHL)) {
  
  cpt <- cpt + 1 # iterate counter
  
  r <- CHL[i,] # row timeserie
  n <- which(!is.na(r)) # locations of non-NAs
  diff_n <- n[2:length(n)] - n[1:length(n)-1] # number of weeks inbetween non-NAs
  
  if (length(n) >= round(nweeks/2) & length(which(diff_n >= 5)) == 0) { # at least a half weeks of data with a data point every 4 weeks 
    r_interp <- approx(1:nweeks,r,xout=1:nweeks,rule=1)$y # linearly interpolate
    r_interp <- approx(1:nweeks,r_interp[c(24:46,1:23)],xout=1:nweeks,rule=1)$y # repeat for extrapolation (move winter in the middle)
    r_interp <- r_interp[c(24:46,1:23)] # move back summer in the middle of the time series
    CHL_max[cpt] <- max(r_interp) # save max value of the timeserie
    CHL_norm[cpt,] <- r_interp/CHL_max[cpt] # normalization (divide by maximal value)
  }
}

#--- Save dataset ----
dataset$CHL_norm <- CHL_norm # latitude of the MedSea map
dataset$CHL_max <- CHL_max # latitude of the MedSea map
dataset$gp <- which(!is.na(CHL_norm[,1])) # pixels (timeseries) without NA to be clusterized (gp = good points)
save(dataset, file = paste(folder,"MedOC_clim_1997_2017_8D_9km_CHL_CoastFree.rdata",sep="")) # save rdata into the same folder

