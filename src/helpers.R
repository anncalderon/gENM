packs <- c("gdistance", "fossil" , "igraph", "rgbif","mapproj","mapdata","sp",
           "maptools","dismo","rJava","rgdal", "rgeos", "raster", "reshape2", "ggplot2","FedData")

lapply(packs[!(packs %in% installed.packages()[,'Package'])],install.packages)
all(unlist(lapply(packs, require, character.only = TRUE,quietly=TRUE)))

### Creates a symmetric matrix comprised of 
### the sum of the upper and lower triangles.
### MKLau - 06July2016

symSum <- function(x='matrix',zero.diag=TRUE){
  if (zero.diag == TRUE){diag(x) <- 0}
  sum.lu <- t(x)[lower.tri(x)]  + x[lower.tri(x)]
  x[lower.tri(x)] <- sum.lu
  x <- t(x)
  x[lower.tri(x)] <- sum.lu
  if (isSymmetric(x)){x}else{
    warning('Output matrix is not symmetirc.')
  }
}

### 
altDiff <- function(x){x[2] - x[1]}

###
m <- function(scd){(1-(scd/max(scd)))}

###

### gClust - models genetic clusters based on landscape features using circuit 
### theory based landscape resistance. Returns a list of observation matrices 
### grouped by the 'genetic' clusters.

### MKLau and ACalderon - Summer 2016

### x = Distribution data for a given organism using lon and lat coordinates.
### p = Environmental 
### N = effective population size

gClust <- function(x='coordinates',vp='vector predictor',N=1,km=TRUE,kcmax=10,kcstart=10,kcthresh=0.005){
  set.seed(1)
  if (!km){
    # Conductance matrix  used to produce                                        # an initial matrix of "flow" between observations
    if (!(is.matrix(x))){x <- as.matrix(x)}
    hd <- transition(vp, altDiff, 8, symm=FALSE)
    slope <- geoCorrection(hd)
    adj <- adjacent(vp, cells=1:ncell(vp), pairs=TRUE, directions=8)
    speed <- slope
    speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
    Conductance <- geoCorrection(speed)
    cd <- costDistance(Conductance, x)
                                          # Summing over assymmetry
    scd <- symSum(cd)
                                          # Re-scaling using basic population
                                          # demography to approximate migration.
                                          # N = effective population size, by 
                                          # default this is set to one for mathematical
                                          # convenience. The diagonal is set to zero 
                                          # because each observation should be 
                                          # genetically identical to itself.
                                          # See Conner & Hartl pg. 84
    Fst <- 0.20*(1/(1+4*N*m(scd))) 
    diag(Fst) <- 0
                                          # Invert Fst so that it is a similarity rather
                                          # than a dissimilarity matrix and find
                                          # the minimally connected graph to focus 
                                          # on the most important connections.
    Fst.g <- 1  - Fst
    diag(Fst.g) <- 0
    Fst.mg <- dino.mst(Fst.g)
    Fst.ig <- graph.adjacency(Fst.mg,weighted=TRUE,mode='undirected')
                                        # Determine clusters using a graph theoretic 
                                        # module/cluster detection algorithm.
    fg.mP <- fastgreedy.community(Fst.ig)
    gc <- fg.mP$membership
    names(gc) <- rownames(Fst)
  }else{
    y <- extract(vp,x)
    kc <- lapply(1:kcmax,kmeans,x=y,nstart=kcstart)
    wss <- unlist(lapply(kc,function(x) x$tot.withinss))
    dw <- abs(diff(wss / max(wss)))
    if (all(dw > kcthresh)){nc <- kcmax}else{
      nc <- max((1:(kcmax - 1))[dw >= kcthresh])
    }
    kc <- kc[[nc]]
    gc <- kc$cluster
  }
                                        # Output observations in a format for the 
                                        # gENM. 
  return(gc)
}

######

### Creates an environmental niche model
### based on MaxEnt alogrithims
### to predict habitat suitability.

### ACalderon and MKLau - 15July2016

### x = Distribution data for a given organism using lon and lat coordinates.
### p = Environmental predictor

ENM <- function(x="coordinates", p="predictors",c.rad=50000,seed=123,n=1000){
  if (class(p) == 'RasterLayer'){p <- stack(p)}
  set.seed(seed)
  circ <- circles(x, d=c.rad, lonlat=T)
  random <- spsample(circ@polygons, n, type='random', iter=100)
                                              # Makes circles with a 5K radius of each
                                              # point and adds 1000 randomized points.
  
  gsp_bc <-  extract(p, x) 
  gsp_bc <-  data.frame(cbind(x,env=gsp_bc))
                                              # Extracts the climate variables which 
                                              # correspond to each presence point
                                              # of a cluster. Binds climate variables
                                              # with their resepctive coordinates, 
                                              # then turns that matrix into a list. 
                                              # And renames the columnames as 
                                              # "lon", "lat", and "clim.var"
  
  random_bc <- extract(p, random) 
  random  <- random@coords
  colnames(random) <- c("lon","lat")
                                              # Extracts the climate variables which 
                                              # which correspond to each random point
                                              # of a cluster. Coordinates of random are 
                                              # saved as a matrix. 
  
  random_bc <-  data.frame(cbind(random,env=random_bc))
  random_bc  <-  random_bc[!is.na(random_bc[,3]), ] 
                                              # Binds the random point coordinates and 
                                              # and the extracted climate variables 
                                              # that correspond to those random points
                                              # in order to create a list.
                                              # And renames the columnames as 
                                              # "lon", "lat", and "clim.var"
                                              # Also removes any NAs in the list. 
  
  me <- maxent(p, gsp_bc[,c("lon", "lat")], random_bc[,c("lon", "lat")])
  e <- evaluate(gsp_bc[,c("lon", "lat")], random_bc[,c("lon", "lat")], me, p)
  pred_me <- predict(me, p) 
                                              # Build a "MaxEnt" (Maximum Entropy) species
                                              # distribution model based on predictors 
                                              # and produces a model that is used by 
                                              # the predict() fucntion to predict
                                              # the suitability of other locations.
  out <- list(eval = e, pred = pred_me, model = me)
  return(out)
}

########

### Applies an environmental niche model
### to clusters of a population as well 
### as to the entire species, and predicts
### habitat suitability for each cluster. 

### ACalderon and MKLau - 15July2016

### x = Distribution data for a given organism using lon and lat coordinates.
### clust = genetic clusters simulated from the gClust function


gENM <- function(x='coordinates', clust='gen clusters', p="Environmental"){
  df.gsp <- data.frame(x)
  groups <-  split(df.gsp, clust)
  analysis <- (lapply(groups, ENM, p))
  enm.all <- list(ENM(do.call(rbind,groups),p))
  out <- append(enm.all, analysis)
}

########

### Applies an environmental niche model
### to clusters of a population as well 
### as to the entire species, and predicts
### habitat suitability for each cluster. 

### ACalderon and MKLau - 15July2016

### x = Distribution data for a given organism using lon and lat coordinates.
### clust = genetic clusters simulated from the gClust function

gAnalysis <- function(x="gENM output", filename= "../results/gENM.jpeg",
                      mfrow=c(3,3),ext=extent(-73.70833, -66.95833, 41, 47.45833),open.file=TRUE){
  jpeg(filename = filename, width = 1700, height = 1700,
       units = "px", pointsize = 35, quality = 90,
       bg="white")
  par(mfrow=mfrow, oma=rep(0,4),omi=rep(0,4), bty = 'n',mar=rep(0.01,4),mai=rep(0,4))
  for(i in 1: length(x)){zoom(x[[i]]$pred, ext=ext, xaxt='n', yaxt='n', new=FALSE, asp=1)}
  dev.off()
  auc <- unlist(lapply(x,function(x) x$eval@auc))
  cor <- unlist(lapply(x,function(x) x$eval@cor))
  out <- data.frame(auc,cor)
  return(out)
  if (open.file){system(paste('open',filename))}
}

##### gDensCurv: plots density curves

gDensCurv <- function(x='coordinates',  p='mintemp.2006',pr='temp for range',gc='clusters'){
    mt.2006 <- extract(p,x)
    mt.c <- split(mt.2006,gc)
    mt.r <- extract(pr,x)
    r.c <- split(mt.r,gc)
    mt.range <- do.call(rbind,lapply(r.c,range))
    mt.range <- melt(mt.range)
    mt.range <- data.frame(mt.range)
    mt.range[,1:2] <- apply(mt.range[,1:2],2,factor)
    mt.mc <- melt(mt.c)
    colnames(mt.mc) <- c('MinTemp','Cluster')
                                        #    
    mt.mcplot <- ggplot(mt.mc, aes(MinTemp,colour = Cluster, fill=Cluster)) +
        geom_density(alpha = 0.7) +
    scale_x_continuous(breaks=seq(250.8, 275.9, 4))+
    scale_color_manual(values=c("greenyellow", "pink2", "lightskyblue1"))+
    scale_fill_manual(values=c("chartreuse", "lightcoral", "steelblue1"))+
    theme(panel.background = element_rect(fill = "gray8"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none",
              axis.line = element_line(colour = "white"),
              axis.title = element_text(color = "white"),
              axis.ticks = element_line(size = 2, colour = "black"),
              axis.text = element_text(colour = "white"),
              axis.text.x = element_text(colour="black",size=15),
              axis.text.y = element_text(colour="black",size=15))
     mt.mcplot + geom_vline(data=mt.range,aes(xintercept=value,colour=Var1))
}
