rm(list=ls()) # clear all variable
library(e1071)
library(fpc) # for clustering

folder <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/MedOC4AD4/" # folder with data
filename <- "MedOC_clim_1997_2017_8D_9km_CHL_CoastFree.rdata" # Name of the dataset
load(file = paste(folder,filename,sep="")) # load dataset (row = pixels, column = variables)

# CHL <- dataset$CHL_norm[,c(24:46,1:23)] # dataset to be cluterized
# gp <- dataset$gp # pixel (observations) to be clusterized
# tschl <- CHL[gp,]
# 
# # --- conduct PCA
# pca <- prcomp(tschl, retx=TRUE, center=TRUE, scale=TRUE)
# explvar <- round(pca$sdev^2/sum(pca$sdev^2)*100) # percent explained variance
#
#
# # --- Save PCA results
# dataset$pca <- pca
# dataset$explvar <- explvar
# 
# 
# # --- Determined threshold (of membership) with a cmeans on climatological dataset
# pred <- predict(pca, newdata=CHL[gp,])
# pcs <- 4
# centers <- apply(pred[,1:pcs],2,function(x) {aggregate(x,list(dataset$cluster),mean)[,2]})
# climcmeans <- cmeans(pred[,1:pcs], centers[,1:pcs])
# 
# climmbs <- apply(climcmeans$membership,1,max)
# threshold <- aggregate(climmbs,list(climcmeans$cluster),function(x) {quantile(x,.1)})[,2]
# 
# # --- Save PCA results obtained from climatology
# dataset$pca <- pca
# dataset$centers <- centers
# dataset$pcs <- pcs
# dataset$threshold <- threshold
# 
# save(dataset, file = paste(folder,filename,sep=""))

# --- load pca results
pca <- dataset$pca
pcs <- dataset$pcs
centers <- dataset$centers
# threshold <- dataset$threshold


# - - - Annual clustering
folder <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/MedOC4AD4/" # folder with data
filename <- "MedOC_year_1998_2017_8D_9km_CHL_CoastFree.rdata" # Name of the dataset
load(file = paste(folder,filename,sep="")) # load dataset (row = pixels, column = variables)

# --- Annual cmeans
dataset$cluster <- array(NA,c(dim(dataset$CHL_norm)[1],dim(dataset$CHL_norm)[3])) # 2D matrix of data (CHL data x nb of years)
dataset$mbs <- array(NA,c(dim(dataset$CHL_norm)[1],dim(dataset$CHL_norm)[3])) # 2D matrix of membership

for (y in 1:dim(dataset$CHL_norm)[3]) {
  # annual data modified with climatological pca coordinates
  gp <- dataset$gp[[y]]
  pred <- predict(pca, newdata=dataset$CHL_norm[gp,,y])
  
  # annual data clusterized
  a <- cmeans(pred[,1:pcs], centers[,1:pcs])
  yearmbs <- apply(a$membership,1,max)
  dataset$cluster[gp,y] <- a$cluster
  dataset$mbs[gp,y] <- yearmbs
}


# --- Which timeseries have a membership lower than the threshold?
bad_gp <- list()
bad_mbs <- c()
CHL_norm_bad <- c()
for (y in 1:dim(dataset$CHL_norm)[3]) {
  bad_gp[[y]] <- dataset$gp[[y]][which(dataset$mbs[dataset$gp[[y]],y]<threshold[dataset$cluster[dataset$gp[[y]],y]])]
  CHL_norm_bad <- rbind(CHL_norm_bad, dataset$CHL_norm[bad_gp[[y]],,y])
  bad_mbs <- c(bad_mbs, dataset$mbs[bad_gp[[y]],y])
}

# --- Kmeans with bad pixel timeseries
# best number of cluster with a silhouette analysis

nclu <- 3
nruns <- 5 # number of time you should do the clustering
niter <- 150  # number of iteration before to stop the kmeans process
cl<-kmeansCBI(CHL_norm_bad,k=nclu,scaling=F,runs=nruns, iter.max=niter, algorithm="Lloyd") # clustering Kmeans, runs: Number of starts of the k-means


p <- silhouette(CHL_norm_bad, cl$result$cluster,.01)

pred <- predict(pca, newdata=CHL_norm_bad)
centers_bad <- apply(pred[,1:pcs],2,function(x) {aggregate(x,list(cl$result$cluster),mean)[,2]})
a <- cmeans(pred[,1:pcs], centers[,1:pcs])
mbs_bad <- apply(a$membership,1,max)


test <- apply(dataset$cluster, 1, function(x) {uniqx <- unique(x); uniqx[which.max(tabulate(match(x, uniqx)))]})




mbs <- apply(a$membership, 1, max)

mbs <- apply(climcmeans$membership, 1, max)
clus <- dataset$cluster[gp,y]

result <- data.frame(mbs,clus)
ggplot(result, aes(y=mbs, x = factor(clus))) + geom_boxplot()


lon <- dataset$LON[gp]
lat <- dataset$LAT[gp]

med <- read.csv("C:/Users/nmayot/Documents/PostDoc/data/gshhg/gshhg_medit_i.csv") # file to draw the coast
coastd <- geom_polygon(data=med, aes(x=lon, y=lat), fill="black",colour=0)


clus2 <- clus
# clus2 <- dataset$cluster
clus2[mbs < .7] <- NA

y <- 1
# gp <- dataset$gp[[y]]
# clus <- dataset$cluster[gp,y]
clus <- apply(dataset$cluster, 1, function(x) {as.numeric(names(sort(table(x, useNA="always"),decreasing=T))[1])})
gp <- which(!is.na(clus))
clus <- clus[gp]
# mbs <-dataset$mbs[gp,y]
lon <- dataset$LON[gp]
lat <- dataset$LAT[gp]

# # for the map
# ggplot() +
#   # geom_tile(aes(x = lon, y = lat, fill = mbs)) +
#   geom_tile(aes(x = lon, y = lat, fill = factor(clus), alpha=mbs)) +
#   scale_fill_brewer(type="qual", palette = "Dark2") +
#   coord_quickmap() +  # Prevents stretching when resizing
#   theme_bw() +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   theme(legend.title = element_blank()) +
#   coastd

# for the map
ggplot() +
  # geom_tile(aes(x = lon, y = lat, fill = mbs)) +
  geom_tile(aes(x = lon, y = lat, fill = factor(clus))) +
  scale_fill_brewer(type="qual", palette = "Dark2") +
  coord_quickmap() +  # Prevents stretching when resizing
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.title = element_blank()) +
  coastd






fq <- array(NA,dim = c(dim(dataset$cluster)[1],5))
for (p in 1:dim(dataset$cluster)[1]) {
  
  a <- (table(dataset$cluster[p,], useNA="always")/19)*100
  b <- names(table(dataset$cluster[p,], useNA="always"))
  b[length(b)] <- "5"
  
  fq[p,as.numeric(b)] <- a
}

clus <- apply(dataset$cluster, 1, function(x) {as.numeric(names(sort(table(x, useNA="always"),decreasing=T))[1])})
gp <- which(!is.na(clus))
fq <- fq[gp,]
lon <- dataset$LON[gp]
lat <- dataset$LAT[gp]

ggplot() +
  # geom_tile(aes(x = lon, y = lat, fill = mbs)) +
  geom_tile(aes(x = lon, y = lat, fill = fq[,4])) +
  scale_fill_continuous(limits=c(0,100)) +
  coord_quickmap() +  # Prevents stretching when resizing
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.title = element_blank()) +
  coastd

  