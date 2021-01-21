
## Plotting ENM


plot(x@polygons, axes=T, col=rgb(0,0,0,0.1), border=NA, add=T)


e
me

plot(pred_me, 1, cex=0.5, legend=T, mar=par("mar"), xaxt="n", yaxt="n", main="Predicted Species Distribution")
map("state", xlim=c(leftlon,rightlon), ylim=c(lowerlat,upperlat), fill=F, col="black", add=T)
<<<<<<< HEAD
points(random,col="black",pch=1,cex=0.2)
points(Obs, Gspecies$lat, col="darkgreen", pch=20, cex=0.5)

=======
points(random,col="snow",pch=1,cex=0.2)
points(species$lon, Gspecies$lat, col="darkgreen", pch=20, cex=0.5)
>>>>>>> mkl

# add axes
axis(1,las=1)
axis(2,las=1)
box()


###

#Plotting NED

#NED.plot(NED = NED)
#p.points(gspecies = gspecies)

## 
#Plotting clusters

plot(Fstm.igP,vertex.color=rainbow(max(gclust))[gclust],
     vertex.label=NA,vertex.size=2,vertex.frame.color=NA)

points(do.call(rbind,gObs), col=rainbow(max(gclust)+1)[gclust], pch=20, cex=0.5)
###
