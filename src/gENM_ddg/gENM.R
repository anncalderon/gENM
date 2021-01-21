#Anna M Calderon
#Matthew K Lau
#Harvard Forest
#gENM-Data Set Up
#Part 0
#28 July 2016

#Anna M Calderon
#Matthew K Lau
#Harvard Forest
#gENM-Data Set Up
#Part 0
#8 July 2016

#SET WORKING DIRECTORY
## wd <- '/Users/annacalderon/Desktop/gENM/src'
## setwd(wd)

#SOURCE FUCNTIONS
source("helpers.R")

#IMPORTING DATA
#neClim <- stack("../data/neClim.grd")
mintemp.2006  <- raster("../data/01_01_2006.tiff")
mintemp.2050 <- raster("../data/01_01_2050.tiff")
mintemp.2099 <- raster("../data/01_01_2099.tiff")

gsp <-read.csv("../data/RICTMEdukesnantucket.csv")
if (is.matrix(gsp) == FALSE){gsp <- data.matrix(gsp)}

#CLUSTERS

clust <- gClust(x=gsp, vp=mintemp.2006)
out <- gENM(x=gsp, clust=clust, p=mintemp.2006) 
gAnalysis(x=out)

#HISTOGRAMS and GRAPHS
gDensCurv(x=gsp, p=mintemp.2006,pr=mintemp.2006,gc)
gDensCurv(x=gsp, p=mintemp.2006,pr=mintemp.2050,gc)
gDensCurv(x=gsp, p=mintemp.2006,pr=mintemp.2099,gc)

