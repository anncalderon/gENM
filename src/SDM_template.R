
### ENM using MaxEnt with GBIF and BioClim data
### A. Calderon and M.K. Lau
### 17June2016

### Inputs

## 1. working directory                                        
wd <- '/Users/annacalderon/Desktop/gENM/data' # default is the current working directory

## 2. choose the species
genus <- ''
species <- ''

## 3. Select a window for the range 

leftlon <- -71
rightlon <- -71.8
lowerlat <- 42.5
upperlat <- 42.8

## 4. Define filename and file paths
path <- ("") #data folder path

if (path == ""){path <- "../data"}
filename <- ("GspeciesBC_2.5.grd")
paste(path,filename, sep="/")
croppeddata<- paste(path,filename, sep="/")

## 5. Landscape resistance

## 6. Genetic distance

## 7. Genetic clusters


##############################     Setting your work station    #############################

setwd(wd)

packs<-c("rgbif","mapproj","mapdata","sp","maptools","dismo","rJava","rgdal", "rgeos")
## Load package dependencies
# if (!require("pacman")){install.packages("pacman")}
#  library(pacman)
#  pacman::p_load(packs)

unlist(lapply(packs, require, character.only = TRUE))


###################################    SETTING UP YOUR DATA      ##############################

 if (leftlon == ''){leftlon <- -99.2}
 if (rightlon == ''){rightlon <- -63}
 if (lowerlat == ''){lowerlat <- 23.6}
 if (upperlat == ''){ upperlat <- 45.5}

if (genus == ''){genus <- 'Aphaenogaster';species <- 'picea'}
rawdata <- gbif(genus = genus, species = species) 
na.omit(rawdata[,c('lat','lon')])
Gspecies <- na.omit(rawdata[,c('lat','lon')])

######################    PLOTTING PRESENCE AND ABSENCE POINTS  ############################

data(stateMapEnv)
plot(c(leftlon, rightlon), c(lowerlat, upperlat), mar=par("mar"), xlab="longitude",
     ylab="latitude", xaxt="n", yaxt="n", type="n", main="Presence and Absence Points")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col="lightcyan")
map("state", xlim=c(leftlon, rightlon), ylim=c(lowerlat, upperlat), fill=T, col="honeydew", add=T)
# plot the points
points(Gspecies$lon, Gspecies$lat, col="darkolivegreen4", pch=20, cex=0.5)
axis(1,las=1)
axis(2,las=1)
box()

# create sequences of latitude and longitude values to define the grid
longrid = seq(leftlon, rightlon,0.05)
latgrid = seq(lowerlat, upperlat,0.05)

# identify points within each grid cell, draw one at random
subs = c()
for(i in 1:(length(longrid)-1)){
  for(j in 1:(length(latgrid)-1)){
    gridsq = subset(Gspecies, lat > latgrid[j] & lat < latgrid[j+1] & lon > longrid[i] & lon < longrid[i+1])    
    if(dim(gridsq)[1]>0){
      subs = rbind(subs, gridsq[sample(1:dim(gridsq)[1],1 ), ])
    }
  }
}


dim(subs) 



x=circles(subs[,c("lon","lat")], d=50000, lonlat=T)
plot(x@polygons, axes=T, col=rgb(0,0,0,0.1), border=NA, add=T)
random <- spsample(x@polygons, 1000, type='random', iter=1000)
points(random,col="khaki4",pch=1,cex=0.3)


################################    HANDLING CLIMATE DATA     #############################


require(raster)
BClim = getData("worldclim", var="bio", res=2.5, path="")

#crop data
GspeciesRange = extent(leftlon, rightlon,lowerlat, upperlat)
BClim = crop(BClim, GspeciesRange)
writeRaster(BClim, filename=croppeddata, overwrite=T)
BClim = brick(croppeddata)


#################################PULLING BIOCLIM VALUE######################################
##################################????????????????????######################################

Gspecies_bc = extract(BClim, subs[,c("lon","lat")]) 
random_bc = extract(BClim, random) 
Gspecies_bc = data.frame(lon=subs$lon, lat=subs$lat, Gspecies_bc)

randompnts = random@coords
colnames(randompnts) = c("lon","lat")
random_bc = data.frame(cbind(randompnts,random_bc))
length(which(is.na(random_bc$bio1))) 
random_bc = random_bc[!is.na(random_bc$bio1), ] 


####################################  BUILDIG YOUR SDM  ############################################

me = maxent(BClim, Gspecies_bc[,c("lon", "lat")], random_bc[,c("lon", "lat")])
e = evaluate(Gspecies_bc[,c("lon", "lat")], random_bc[,c("lon", "lat")], me, BClim)
e
pred_me = predict(me, BClim) 
me

plot(pred_me, 1, cex=0.5, legend=T, mar=par("mar"), xaxt="n", yaxt="n", main="Predicted Species Distribution")
map("state", xlim=c(leftlon,rightlon), ylim=c(lowerlat,upperlat), fill=F, col="black", add=T)
points(random,col="snow",pch=1,cex=0.2)
points(Gspecies$lon, Gspecies$lat, col="darkgreen", pch=20, cex=0.5)


# add axes
axis(1,las=1)
axis(2,las=1)
box()

#source("/Users/annacalderon/Desktop/gENM/src/RDataTracker.R")
#ddg.run("/Users/annacalderon/Desktop/gENM/src/BClimBug.R")
