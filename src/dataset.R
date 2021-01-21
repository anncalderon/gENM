#Anna M Calderon
#Matthew K Lau
#Harvard Forest
#gENM-Data Set Up
#Part 0
#8 July 2016

## Step 0. Set a working directory and File Paths

wd <- '../src'
setwd(wd)

## Step 1. Source Helpers Script
source("helpers.R")

## Step 2. Importing Climate Variables for NE
neClim <- stack("../data/neClim.grd")

## Step 3.  Getting climate change projections
library(maptools)
library(rgdal)
vepPolygon <- polygon_from_extent(raster::extent(-73.70833, -66.95833, 41, 47.45833),
                                  proj4string="+proj=longlat +ellps=WGS84 +datum=WGS84")
IDs <- sapply(slot(vepPolygon, "polygons"), function(x) slot(x, "ID"))
df <- data.frame(rep(0, length(IDs)), row.names=IDs)
SPDFxx <- SpatialPolygonsDataFrame(vepPolygon, df)
#tf <- tempfile()
#writePolyShape(SPDFxx, tf)
#getinfo.shape(tf)

## shape <- readOGR('../data/neExtent',layer='neExtent')
writeOGR(SPDFxx,dsn='../data/neExtent',layer='neExtent',driver='ESRI Shapefile',overwrite_layer=TRUE)

## Step 4. Downloading Species Presence Data
gspecies <- ''

prespoints <- read.csv('http://harvardforest.fas.harvard.edu/data/p14/hf147/hf147-13-antData_use4R_snappedToClim.csv')
if (gspecies == ''){gspecies <- "aphrud"}
colnames(prespoints) = c("spcode", "lon","lat")
gspecies <- prespoints[grep(gspecies,as.character(prespoints$spcode)),]
gspecies$spcode <- NULL

if (identical(colnames(gspecies),c( "lat", "lon"))){gspecies <- gspecies[,c('lon','lat')]}
if (is.matrix(gspecies) == FALSE){gspecies <- data.matrix(gspecies)}

## Step 5. Making Clusters and running gENMs

clust <- gClust(x=gspecies, p=neClim$bio1)
gENM(x=gspecies)



