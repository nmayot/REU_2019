### Interp. and Norm. before Clustering analysis ---------------------------------
#
#
# --- Open dataset --------------------------------------------------
#
# Here mr.rdata => dataset == List of 5
#   $ data: num [1:138240, 1:46] NA NA NA NA NA NA NA NA NA NA ...
#   $ lat : num [1:288, 1:480] 30 30 30 30 30 ...
#   $ lon : num [1:288, 1:480] 32 32.1 32.1 32.1 32.2 ...
#   $ xo  : int [1:138240] 1 2 3 4 5 6 7 8 9 10 ...
#   $ yo  : int [1:138240] 1 1 1 1 1 1 1 1 1 1 ...

rm(list=ls()) # clear all variable
library("fpc")
# library("fields")

load(file="C:/Users/nmayot/Documents/side works/201903-David-A/Australia/CHL_Month.rdata")

adresults <- data$CHL
nweeks <- dim(adresults)[2]


# --- Interpollation and Normalization ----------------------------------------------
#
#
# Linear interpolation to remove NA, and divide each time series by its maximal value 

adresults_norm <- array(dim = dim(adresults)) # Colonne = Time (variables), Line = Pixel (stations)
maxo_row <- array(dim = c(dim(adresults)[1],1))

cpt <- 0
for (i in 1:nrow(adresults)) {
  cpt <- cpt + 1
  r <- adresults[i,] # Time series
  d <- which(is.finite(r) == T) # Locations NA
  diff_d <- d[2:length(d)] - d[1:length(d)-1] # number of weeks btw NA
  
  if (length(d) >= round(nweeks/2) & length(which(diff_d >= 5)) == 0) { # at least a half weeks of data with a data point every 4 weeks 
    row_value <-approx(1:nweeks,r,xout=1:nweeks,rule=2)$y # linearly interpolate
    maxo_row[cpt] <- max(row_value) # save max value
    adresults_norm[cpt,] <- row_value/maxo_row[cpt] # Normalization (divide by max)
  }
}


adresults <- adresults_norm # save data for clustering
good_points <- which(is.na(adresults[,1]) == F) # pixels without NA to be cluterized
bad_points <- which(is.na(adresults[,1]) == T) # pixels with NA