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

folder <- "C:/Users/nmayot/Documents/PostDoc/data/satellite/SeaWiFS/" # folder with data
filename <- "SeaWiFS_climato_1997_2007_8D_CHL_CHLnorm.rdata" # Name of the dataset

load(file = paste(folder,filename,sep="")) # load dataset (row = pixels, column = variables)

CHL <- dataset$CHL_norm # dataset to be cluterized
gp <- dataset$gp # pixel (observations) to be clusterized


# --- Clustering --------------------------------------------------

CLUS <- c()
nclu <- 7 # number of clusters wanted
nruns <- 10 # number of time you should do the clustering
niter <- 500 # number of iteration before to stop the kmeans process

cl<-kmeansCBI(CHL[gp,],k=nclu,scaling=F,runs=nruns, iter.max=niter, algorithm="Lloyd") # clustering Kmeans, runs: Number of starts of the k-means

# --- quick plots of the results
clus <- cl$result$cluster
lon <- dataset$LON[gp]
lat <- dataset$LAT[gp]
col <- brewer.pal(7, "Dark2")

# for the map
map_clus <- ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = factor(clus)), interpolate = FALSE) +
  scale_fill_manual(values = col[1:max(clus)]) +
  coord_quickmap() +  # Prevents stretching when resizing
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  xlim(-6,36.5) +
  ylim(30,46) +
  theme(legend.title = element_blank())

map_clus # plot map

# for the timeseries
X <- CHL[gp,c(24:46,1:23)] # From July to June
jdays <- seq(1,366,8) # x-axis in julian days
centers <- aggregate(X,list(clus),mean,simplify = T) # cluster centers (average values)
centerssd <- aggregate(X,list(clus),sd,simplify = T) # STD around cluster centers (std values)
centers <- as.matrix(centers[,2:dim(centers)[2]]) # matrix of centers
centerssd <- as.matrix(centerssd[,2:dim(centerssd)[2]])
centersplus <- centers + centerssd  # matrix of centers + STD
centersminus <- centers - centerssd  # matrix of centers - STD

d = data.frame(t(centers)) # results into a single data.frame
d1 = data.frame(t(centersminus))
d2 = data.frame(t(centersplus))
colnames(d2) <- c("Z1","Z2","Z3","Z4","Z5","Z6","Z7")
colnames(d1) <- c("Y1","Y2","Y3","Y4","Y5","Y6","Y7")
d = cbind(d,d1,d2)

# grid locations (half time of each month)
mjdate <- c("15-07-2000","15-08-2000","15-09-2000","15-10-2000","15-11-2000","15-12-2000","15-01-2001","15-02-2001","15-03-2001","15-04-2001","15-05-2001","15-06-2001")
mjb <- as.numeric(as.Date(mjdate,"%d-%m-%Y")-as.Date("03-07-2000","%d-%m-%Y"))+1

# text for the timeseries plots
for (nclu in 1:7) {
  txt_plot <- paste("ts_",toString(nclu)," <-ggplot(d, aes(x=jdays)) +",
                    "geom_line(aes(y=X",toString(nclu),"),colour = col[",toString(nclu),"]) +",
                    "geom_line(aes(y=Y",toString(nclu),"),colour = col[",toString(nclu),"]) +",
                    "geom_line(aes(y=Z",toString(nclu),"),colour = col[",toString(nclu),"]) +",
                    "ylab(",noquote("\"Chl norm\""), ") +",
                    "scale_y_continuous(limits = c(0, 1.1), breaks = seq(0,1,.2), minor_breaks = seq(0.0001,1.0001,.2)) +",
                    "scale_x_continuous(limits = c(-1, 366), breaks = mjb, minor_breaks = mjb+15.25,",
                    "labels= format(as.Date(mjdate,",noquote("\"%d-%m-%Y\""),"),",noquote("\"%b\""),")) +",
                    "theme(panel.grid.major = element_blank(),axis.title.x=element_blank())", sep="")
  eval(parse(text= txt_plot))
}

grid.arrange(ts_1,ts_2,ts_3,ts_4,ts_5,ts_6,ts_7, nrow = 3, ncol = 3) # plot timeseries