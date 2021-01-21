#Anna M Calderon
#Matthew K Lau
#Harvard Forest
#gENM-ENM Modeling
#Part 2
#11 July 2016

ENM <- function(x="genetic cluster coordinates"){

set.seed(123)                                          
circ=circles(x, d=50000, lonlat=T)
random <- spsample(circ@polygons, 1000, type='random', iter=1000)
                                  # Makes circles with a 5K radius of each
                                  # point and adds 1000 randomized points.

clust_bc <-  extract(p, x) 
clust_bc <-  data.frame(cbind(x,clust_bc))
                                  # Extracts the climate variables which 
                                  # which correspond to each presence point
                                  # of a cluster. Binds climate variables
                                  # with their resepctive coordinates, 
                                  # Then turns that matrix into a list. 
                                  
random_bc <- extract(p, random) 
random  <- random@coords
colnames(random) <- c("lon","lat")
                                  # Extracts the climate variables which 
                                  # which correspond to each random point
                                  # of a cluster. Coordinates of random are 
                                  # saved as a matrix. 

random_bc <-  data.frame(cbind(random,random_bc))
random_bc  <-  random_bc[!is.na(random_bc[,"bio1"]), ] 
                                  # Binds the random point coordinates and 
                                  # and the extracted climate variables 
                                  # that correspond to those random points
                                  # in order to create a list. Also removes
                                  # any NAs in the list. 

me <- maxent(p, clust_bc[,c("lon", "lat")], random_bc[,c("lon", "lat")])
e <- evaluate(clust_bc[,c("lon", "lat")], random_bc[,c("lon", "lat")], me, p)
pred_me <- predict(me, p) 
                                  # Build a "MaxEnt" (Maximum Entropy) species
                                  # distribution model based on neCLim 
                                  # and produces a model that is used by 
                                  # the predic() fucntion to predict
                                  # the suitability of other locations.
plot(pred_me)
}
                                  


