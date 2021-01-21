locs <- data.frame(lon=sort(c(-70.84345, -70.75806)), 
                   lat= sort(c(43.57888, 43.64014)))
gsp <- gsp[gsp[,2] <= 43 | ((gsp[,1] >= locs[1,1] & gsp[,1] <= locs[2,1]) & 
                (gsp[,2] >= locs[1,2] & gsp[,2] <= locs[2,2])),]
minT06  <- raster("../data/01_01_2006.tiff")

## par(mfrow=c(1,2))
## plot(minT06)
## points(apply(gsp,2,jitter),cex=0.25,pch=19)
## enhance <- drawExtent()
## plot(minT06,ext=enhance)
## points(apply(gsp,2,jitter),cex=0.25,pch=19)

par(mfrow=c(1,1))
plot(minT06)
points(gsp,pch=19,cex=0.1)

