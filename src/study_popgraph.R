## library(RDataTracker)
## ddg.run("study_popgraph.R",load = TRUE)

### Studying popgraph package for use in the gENM
### project. Main question is can we determine 
### genetic clusters using an Fst matrix.
### installed.packages("popgraph")
library(popgraph)
require(ggplot2)
require(gstudio) # install_github('dyerlab/gstudio')

data(arapat)
data <- unique(arapat[, c(2, 3, 6, 5)])
centroid <- apply(data[, 3:4], 2, mean)

map <- get_map(location,maptype="satellite", zoom=6)
map <- population_map(data)
ggmap(map) + geom_point(aes(x = Longitude, y = Latitude, color = Cluster), data = data, size = 4)

data <- arapat[arapat$Species == "Mainland", c(3, 13)]

dist_cgd(data)

data <- to_mv(arapat)
pops <- arapat$Population
graph <- popgraph(x = data, groups = (pops))

plot(graph)


p <- ggmap( map ) 
p <- p + geom_edgeset( aes(x=Longitude,y=Latitude), lopho, color="white" ) 
p <- p + geom_nodeset( aes(x=Longitude, y=Latitude, color=Region, size=size), lopho) 
p + xlab("Longitude") + ylab("Latitude")


A <- matrix(0, nrow=5, ncol=5)
A[1,2] <- A[2,3] <- A[1,3] <- A[3,4] <- A[4,5] <- 1
A <- A + t(A)
A

g <- as.popgraph( A )
V(g)$name <- c("Olympia","Bellingham","St. Louis","Ames","Richmond")
V(g)$group <- c("West","West", "Central","Central","East")
V(g)$color <- "#cca160"
list.vertex.attributes( g )
V(g)$name
E(g)
E(g)$color <- c("red","red", "red", "blue","dark green")
list.edge.attributes( g )
data(lopho)
class(lopho)

lopho

data(baja)
summary(baja)

lopho <- decorate_graph( lopho, baja, stratum="Population")
lopho

plot(g)
plot(g, edge.color="black", vertex.label.color="darkred", vertex.color="#cccccc", vertex.label.dist=1)

layout <- layout.circle( g )
plot( g, layout=layout)

layout <- layout.fruchterman.reingold( g )
plot( g, layout=layout)




p <- ggplot() 
p <- p + geom_edgeset( aes(x=Longitude,y=Latitude), lopho ) 
p

p <- p +  geom_nodeset( aes(x=Longitude, y=Latitude), lopho, size=4)
p

p <- ggplot() + geom_edgeset( aes(x=Longitude,y=Latitude), lopho,
color="darkgrey" )
p <- p + geom_nodeset( aes(x=Longitude, y=Latitude, color=Region,
size=size), lopho)
p <- p + xlab("Longitude") + ylab("Latitude")
p + theme_empty()

c <- layout.fruchterman.reingold( lopho )
V(lopho)$x <- c[,1]
V(lopho)$y <- c[,2]
p <- ggplot() + geom_edgeset( aes(x,y), lopho, color="darkgrey" )
p <- p + geom_nodeset( aes(x, y, color=Region, size=size), lopho)
p + theme_empty()

c <- layout.fruchterman.reingold( lopho )
V(lopho)$x <- c[,1]
V(lopho)$y <- c[,2]
p <- ggplot() + geom_edgeset( aes(x,y), lopho, color="darkgrey" )
p <- p + geom_nodeset( aes(x, y, color=Region, size=size), lopho) 
p + theme_empty()
p


graph <- read.popgraph( "thegraph.pgraph" )

## mapping
require(maps)
require(ggmap)
require(raster)

V(g)$Latitude <- c( 47.15, 48.75,38.81, 42.26, 37.74 )
V(g)$Longitude <- c(-122.89,-122.49,-89.98, -93.47, -77.16 )

map( "state" )
overlay_popgraph(g)

location <- c( mean(V(lopho)$Longitude), mean(V(lopho)$Latitude))
location

map <- get_map(location,maptype="satellite", zoom=6)

dim(map)
map[1:4,1:4]

p <- ggmap( map ) 
p <- p + geom_edgeset( aes(x=Longitude,y=Latitude), lopho, color="white" ) 
p <- p + geom_nodeset( aes(x=Longitude, y=Latitude, color=Region, size=size), lopho) 
p + xlab("Longitude") + ylab("Latitude")

data(alt)
plot(alt)

lopho.nodes <- to_SpatialPoints(lopho)
lopho.nodes

plot( alt )
plot( lopho.edges, add=TRUE, col="#555555" )
plot( lopho.nodes, add=TRUE, col="black", cex=1.5 )
plot( lopho.nodes, add=TRUE, col=V(lopho)$color, pch=16, cex=1.5 )

df.nodes <- data.frame(Pop=V(lopho)$name, Latitude=V(lopho)$Latitude, Longitude=V(lopho)$Longitude)

df.nodes$Elevation <- raster::extract( alt, lopho.nodes )
summary(df.nodes)

df.edge <- data.frame(Weight=E(lopho)$weight )
summary(df.edge)
plot(alt)
plot(lopho.edges[3],add=TRUE,col="red",lwd=3)

Elevation <- extract( alt, lopho.edges[3] )[[1]]
e <- extent( lopho.edges[3] )
e

Latitude <- seq(ymin(e),ymax(e),length.out=length(Elevation))
qplot( Latitude, Elevation, geom="line" )
