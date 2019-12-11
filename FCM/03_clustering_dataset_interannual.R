### Clustering the dataset ---------------------------------
#
#
# --- Used dataset --------------------------------------------------



rm(list=ls()) # clear all variable
library("fpc") # for clustering

folder <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/MedOC4AD4/" # folder with data
filename <- "MedOC_clim_1997_2017_8D_9km_CHL_CoastFree.rdata" # Name of the dataset

load(file = paste(folder,filename,sep="")) # load dataset (row = pixels, column = variables)

CHL <- dataset$CHL_norm # dataset to be cluterized
gp <- dataset$gp # pixel (observations) to be clusterized


# --- Clustering --------------------------------------------------

nclu <- 4 # number of clusters wanted
nruns <- 40 # number of time you should do the clustering
niter <- 1000 # number of iteration before to stop the kmeans process

cl<-kmeansCBI(CHL[gp,],k=nclu,scaling=F,runs=nruns, iter.max=niter, algorithm="Lloyd") # clustering Kmeans, runs: Number of starts of the k-means

dataset$cluster <- cl$result$cluster

# filename <- "MODIS-Aqua_climato_2007_2017_8D_CHL_CHLnorm_cl.rdata" # Name of the dataset
filename <- "MedOC_clim_1997_2017_8D_9km_CHL_CoastFree.rdata" # Name of the dataset
save(dataset, file = paste(folder,filename,sep=""))
