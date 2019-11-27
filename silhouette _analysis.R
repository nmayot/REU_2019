# Silhouette analysis with a subset on the dataset
#
#
# Necessary information:
#   - X: The numeric matrix (the data matrix) used as input into the Kmeans function (cases*variables).
#   - clus: The vector of integers indicating the cluster to which each point (cases) is allocated


X <- # the data matrix = the chlorophyll time series nomarlized
clus <- # The vector of integers indicating the cluster to which each time series (pixel) is allocated

nclu <- max(clus, na.rm = T) # The number of clusters


# subset the dataset
subsetsize <- # percentage of the dataset to be sampled (between 0 and 1 = 0-100%; 0.01=1%, 0.1=10%, 0.2=20%...)
subloc <- sample(1:dim(X)[1], round(subsetsize*dim(X)[1]), replace=F)
subX <- X[subloc,]
subclus <- clus[subloc]


distX <- as.matrix(dist(subX)) # distance matrix between all the time series of the subset

# estimate the silhouette value of each point (time serie)
siX <- c()
for (l in 1:dim(subX)[1]) {
  value <- aggregate(distX[l,],list(subclus),mean,simplify = T)
  a <- value[subclus[l],2]
  b <- min(value[subclus[l] != 1:nclu,2])
  s <- (b - a)/max(a,b)
  siX <- c(siX,s)
}
  
# order silhouette values (by cluster and from the maximum to the minimum values)
all_idx <- c()
for (n in 1:nclu) {
  idx <- which(n == subclus)
  idx <- idx[order(siX[idx],decreasing = T)]
  all_idx <- c(all_idx,idx) 
}
subloc <- subloc[all_idx]
subX <- subX[all_idx,]
subclus <- subclus[all_idx]
siX <- siX[all_idx]

# The plot showing silhouette scores 
require("ggplot2")
require("RColorBrewer")

col = brewer.pal(nclu, "Dark2")

SI <- data.frame(id = 1:length(subclus), si = siX, cluster = subclus)
si_plot <-ggplot(SI) +
            geom_bar(aes(x=id, y=si, fill=factor(cluster)), stat="identity") +
            scale_fill_manual(values = col[1:nclu])
