# Anna M Calderon
# Matthew K Lau
# Harvard Forest
# gENM-Clustering
# Part 1
# 8 July 2016

### x = Distribution data for a given organism using lon and lat coordinates.
### y = Environmental 
### N = effective population size

gClust <- function(x='distribution',y='environment',N=1){
                                        # Conductance matrix is used to produce 
                                        # an initial matrix of "flow" between observations
hd <- transition(y, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(y, cells=1:ncell(y), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)
cd <- costDistance(Conductance, x)
                                        # Summing over asymmetry
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
Fst.g <- 1-Fst
diag(Fst.g) <- 0
Fst.mg <- dino.mst(Fst.g)
Fst.ig <- graph.adjacency(Fst.mg,weighted=TRUE,mode='undirected')
                                        # Determine clusters using a graph theoretic 
                                        # module/cluster detection algorithm.
fg.mP <- fastgreedy.community(Fst.ig)
gclust <- fg.mP$membership
names(gc) <- rownames(Fst.mst)
                                        # Output observations in a format for the 
                                        # gENM. 
gObs <- split(x,gcl)
gObs <- lapply(gObs,matrix,ncol=2)
for (i in 1:length(gObs)){colnames(gObs[[i]]) <- c("lon","lat")}
return(gObs)
}



