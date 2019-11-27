### Clustering the dataset ---------------------------------
#
#
# --- Used dataset --------------------------------------------------
#
# dataset == List of 9
#   $ CHL     : num [1:97920, 1:46] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
#   $ LON     : num [1:97920] -5.96 -5.87 -5.79 -5.71 -5.62 ...
#   $ LAT     : num [1:97920] 46 46 46 46 46 ...
#   $ mask    : num [1:510, 1:192] 0 0 0 0 0 0 0 0 0 0 ...
#   $ lon_map : num [1:510(1d)] -5.96 -5.87 -5.79 -5.71 -5.62 ...
#   $ lat_map : num [1:192(1d)] 46 45.9 45.8 45.7 45.6 ...
#   $ CHL_norm: num [1:97920, 1:46] NA NA NA NA NA NA NA NA NA NA ...
#   $ CHL_max : num [1:97920, 1] NA NA NA NA NA NA NA NA NA NA ...
#   $ gp      : int [1:36692] 1760 1761 1762 1764 1765 1766 2268 2269 2270 2271 ...


rm(list=ls()) # clear all variable
library("fpc") # for clustering
library("ggplot2") # for plotting
library("RColorBrewer")  # for colors of the plots
library("gridExtra") # for displaying subplots

folder <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/MODIS-Aqua/" # folder with data
# filename <- "SeaWiFS_climato_1997_2007_8D_CHL_CHLnorm.rdata" # Name of the dataset
filename <- "MODIS_climato_2007_2017_8D_CHL_CHLnorm.rdata" # Name of the dataset

load(file = paste(folder,filename,sep="")) # load dataset (row = pixels, column = variables)

CHL <- dataset$CHL_norm # dataset to be cluterized
gp <- dataset$gp # pixel (observations) to be clusterized


# --- Clustering --------------------------------------------------

nclu <- 4 # number of clusters wanted
nruns <- 10 # number of time you should do the clustering
niter <- 500 # number of iteration before to stop the kmeans process

cl<-kmeansCBI(CHL[gp,],k=nclu,scaling=T,runs=nruns, iter.max=niter, algorithm="Lloyd") # clustering Kmeans, runs: Number of starts of the k-means

dataset$cluster <- cl$result$cluster

# filename <- "MODIS-Aqua_climato_2007_2017_8D_CHL_CHLnorm_cl.rdata" # Name of the dataset
filename <- "SeaWiFS_climato_1997_2007_8D_CHL_CHLnorm_cl.rdata" # Name of the dataset
save(dataset, file = paste(folder,filename,sep=""))