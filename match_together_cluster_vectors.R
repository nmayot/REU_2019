#-- cluster vectors to be matched together
#
#
# Necessary information:
#     - Two vectors of integers indicating the cluster to which each time series (pixel) is allocated
#     - The two vectors should have the same length (because locations -lat & lon- should match)
#     - The two vectors can contain NA values

clus_01 <-  # First vector of integers indicating the cluster: the master (e.g., from SeaWiFS)
clus_02 <-  # Second vector of integers indicating the cluster: the one to modify (e.g., from MODIS)

nclu <- max(clus_01, na.rm = T) # The number of clusters
conf <- c() # initialize confusion matrix
# build the confusion matrix
for (n in 1:nclu) {
  conf <- c(conf, hist(clus_02[clus_01 == n], breaks = seq(.5,((nclu)+.5)), plot=FALSE)$count)
}
conf <- matrix(conf,nrow = nclu,byrow = T)

# modify the second cluster vector
clus_02_new  <- clus_02 # initialize the new cluster vector
for (n in 1:nclu) {
  pastcl <- which(conf == max(conf, na.rm = T), arr.ind = TRUE)[1,2]
  newcl <- which.max(conf[,pastcl])
  clus_02_new[clus_02 == pastcl] <- newcl
  conf[newcl,] <- NA
}
clus_02 <- clus_02_new # modify the second cluster vector
