# Functions written and used for this project


#---------------------------------------------------------------
# regrid function
#---------------------------------------------------------------

regrid <- function(LON_px, LAT_px, lon_px, lat_px, LON_z, LAT_z, z) {
  #
  # regrid by averaging value z according to satellite grid
  #
  #---INPUTS
  #
  # LON_px & LAT_px: all paired of longitudes and latitudes, one paire per pixel
  # lon_px & lat_px: unique values of longitudes and latitudes ( length(lat_px) * length(lat_px) = length(LON_px) = length(LAT_px) )
  # z: values of the variable to be regrided
  # LON_z & LAT_z: all paired of longitudes and latitudes, one paire per value of z
  #
  #
  #---OUTPUTS
  #
  # z_px: z values for each wanted pixel
  #
  #
  #---INFO
  #
  # The grid of z should has a higher resolution than the one from satellite, ans similar borders (max/min values of lon and lat)
  #
  
  id_LON <- sapply(LON_z, function(x) which.min(abs(x-lon_px))) # index of the nearest lon
  id_LAT <- sapply(LAT_z, function(x) which.min(abs(x-lat_px))) # index of the nearest lat
  
  id_LON_px <- sapply(LON_px, function(x) which.min(abs(x-lon_px))) # index of the nearest lon
  id_LAT_px <- sapply(LAT_px, function(x) which.min(abs(x-lat_px))) # index of the nearest lat
  
  wh <- nchar(trunc(length(lon_px))) + 1 # the maximum number of digits
  grp <- paste(formatC(id_LON, width=wh, flag="0"),formatC(id_LAT, width=wh, flag="0"),sep="") # create one index from two indices (id_lon and id_lat)
  grp_map <- paste(formatC(id_LON_px, width=wh, flag="0"),formatC(id_LAT_px, width=wh, flag="0"),sep="") # create one index from two indices
  
  # regrid
  for(w in 1:dim(z)[2]) {
    z_regridded <- aggregate(z[,w], list(grp), FUN = mean, na.rm=T) # average value of z per paired of lat/lon (px grid)
    id_z <- match(z_regridded[,1],grp_map) # which value correspond to wanted pixel
    if (!exists("z_px")) {
      z_px <- array(dim=c(length(id_z),dim(z)[2]))
    }
    z_px[id_z,w] <- z_regridded[,2]
  }
  return(z_px)
}



#---------------------------------------------------------------
# Match cluster vectors
#---------------------------------------------------------------

match_cluster <- function(clus_01, clus_02) {
  #
  # match by comparing two vectors
  #
  #---INPUTS
  #
  # clus_01 & clus_02: Two vectors of integers indicating the cluster to which each time series (pixel) is allocated
  #
  #
  #---OUTPUTS
  #
  # clus_02: z values for each wanted pixel
  #
  #
  #---INFO
  #
  # The two vectors should have the same length (because locations -lat & lon- should match)
  # The two vectors can contain NA values.
  # Only clus_02 will be modified.
  #
  
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
    conf[,pastcl] <- NA
  }
  clus_02 <- clus_02_new # modify the second cluster vector
  
  return(clus_02)
}
