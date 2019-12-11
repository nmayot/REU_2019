### Clustering the dataset ---------------------------------
#
#


rm(list=ls()) # clear all variable
library("fpc") # for clustering

# --- Open dataset ------------------------------------------------
folder_S <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/SeaWiFS/" # folder with data
folder_M <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/MODIS-Aqua/" # folder with data
folder_C <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/occci-v3.1/" # folder with data

filename_S <- "SW_climato_1997_2007_8D_CHL_coastfree_norm.rdata" # Name of the dataset
filename_M <- "MODIS_climato_2007_2017_8D_CHL_coastfree_norm.rdata" # Name of the dataset
filename_C1 <- "CCI_climato_1997_2007_8D_CHL_coastfree_norm.rdata"
filename_C2 <- "CCI_climato_2007_2017_8D_CHL_coastfree_norm.rdata"

load(file = paste(folder_S,filename_S,sep="")) # load dataset (row = pixels, column = variables)
load(file = paste(folder_M,filename_M,sep="")) # load dataset (row = pixels, column = variables)
load(file = paste(folder_C,filename_C1,sep="")) # load dataset (row = pixels, column = variables)
load(file = paste(folder_C,filename_C2,sep="")) # load dataset (row = pixels, column = variables)

CHL <- dataset$CHL_norm # dataset to be cluterized
gp <- dataset$gp # pixel (observations) to be clusterized


# --- Clustering --------------------------------------------------

nclu <- 4 # number of clusters wanted
nruns <- 10 # number of time you should do the clustering
niter <- 500 # number of iteration before to stop the kmeans process

tschl <- scale(CHL[gp,], scale=T, center=T) # scaled data
# tschl <- CHL[gp,] # non-scaled data

cl<-kmeansCBI(tschl,k=nclu,scaling=F,runs=nruns, iter.max=niter, algorithm="Lloyd") # clustering Kmeans, runs: Number of starts of the k-means

# --- Save results ------------------------------------------------
dataset$cluster <- cl$result$cluster

save(dataset, file = paste(folder_S,"SW_climato_1997_2007_8D_CHL_coastfree_norm_cl.rdata",sep="")) # save rdata into the same folder
save(dataset, file = paste(folder_M,"MODIS_climato_2007_2017_8D_CHL_coastfree_norm_cl.rdata",sep="")) # save rdata into the same folder
save(dataset, file = paste(folder_C,"CCI_climato_1997_2007_8D_CHL_coastfree_norm_cl.rdata",sep="")) # save rdata into the same folder
save(dataset, file = paste(folder_C,"CCI_climato_2007_2017_8D_CHL_coastfree_norm_cl.rdata",sep="")) # save rdata into the same folder

# --- Match cluster vector ----------------------------------------
source("C:/Users/nmayot/Documents/PostDoc/teaching supervise/REU 2019/scripts/REU_2019/00_functions.R") # functions

folder_S <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/SeaWiFS/" # folder with data
folder_M <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/MODIS-Aqua/" # folder with data
folder_C <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/occci-v3.1/" # folder with data

load(file = paste(folder_S,"SW_climato_1997_2007_8D_CHL_coastfree_norm_cl.rdata",sep="")) # save rdata into the same folder
clus_01 <- dataset$cluster
load(file = paste(folder_M,"MODIS_climato_2007_2017_8D_CHL_coastfree_norm_cl.rdata",sep="")) # save rdata into the same folder
load(file = paste(folder_C,"CCI_climato_1997_2007_8D_CHL_coastfree_norm_cl.rdata",sep="")) # save rdata into the same folder
load(file = paste(folder_C,"CCI_climato_2007_2017_8D_CHL_coastfree_norm_cl.rdata",sep="")) # save rdata into the same folder
clus_02 <- dataset$cluster

clus_new <- match_cluster(clus_01, clus_02)
dataset$cluster <- clus_new
save(dataset, file = paste(folder_M,"MODIS_climato_2007_2017_8D_CHL_coastfree_norm_cl.rdata",sep="")) # save rdata into the same folder
save(dataset, file = paste(folder_C,"MODIS_climato_1997_2007_8D_CHL_coastfree_norm_cl.rdata",sep="")) # save rdata into the same folder
save(dataset, file = paste(folder_C,"MODIS_climato_2007_2017_8D_CHL_coastfree_norm_cl.rdata",sep="")) # save rdata into the same folder
