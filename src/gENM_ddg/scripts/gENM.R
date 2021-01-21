source('helpers.R')

mintemp.2006  <- raster("../data/01_01_2006.tiff")
mintemp.2050 <- raster("../data/01_01_2050.tiff")
mintemp.2099 <- raster("../data/01_01_2099.tiff")

gspecies <-read.csv("../data/RICTMEdukesnantucket.csv")
if (is.matrix(gspecies) == FALSE){gspecies <- data.matrix(gspecies)}

mt.l <- list(mintemp.2006,mintemp.2050,mintemp.2099)
gc <- gClust(gspecies,mt.l[[1]])
mt.enm <- lapply(mt.l,gENM,x=gspecies,clust=gc)

mt.2006 <- extract(mintemp.2006,gspecies)
mt.c <- split(mt.2006,gc)

par(mfrow=c(1,3))
lapply(mt.c,hist,xlim=range(mt.2006),main='')
