
jpeg(filename = "../results/gENM.jpeg", width = 1700, height = 1700,
     units = "px", pointsize = 35, quality = 90,
     bg="black")
par(oma=rep(0,4),omi=rep(0,4), bty = 'n',mar=rep(0.01,4),mai=rep(0,4))
plot(out$`3`$pred)
yellow_blue <- c("black", colorRampPalette(c("black", "snow"))(10))
plot(out$`3`$pred, axes=F, zlim=c(0, 1), col=yellow_blue,legend=T, box=F)
#points(gsp, col="wheat1", cex=1.0,pch =19) 


# df.gsp <- data.frame(gsp)
# groups <-  split(df.gsp, clust)

#points(groups$`1`, col="yellowgreen", cex=1.3,pch =19) 
 #points(groups$`2`, col="pink2", cex=1.3,pch =19)
#points(groups$`3`, col="lightskyblue1", cex=1.3,pch =19)
dev.off()


