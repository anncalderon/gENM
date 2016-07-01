
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

leftlon <- ''
rightlon <- ''
lowerlat <- ''
upperlat <- ''

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

packs<-c("rgbif","mapproj","mapdata","sp","maptools","dismo","rJava","rgdal")
## Load package dependencies
if (!require("pacman")){install.packages("pacman")}
 library(pacman)
 pacman::p_load(packs)
 lapply(packs, require, character.only = TRUE)


###################################    SETTING UP YOUR DATA      ##############################

if (leftlon == ''){leftlon <- -99.2}
if (rightlon == ''){rightlon <- -63}
if (lowerlat == ''){lowerlat <- 23.6}
if (upperlat == ''){upperlat <- 45.5}

if (genus == ''){genus <- 'Aphaenogaster';species <- 'picea'}
rawdata <- gbif(genus = genus, species = species) 
na.omit(rawdata[,c('lat','lon')])
Gspecies <- na.omit(rawdata[,c('lat','lon')])

data(stateMapEnv)

######################    PLOTTING PRESENCE AND ABSENCE POINTS  ############################

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
bg = spsample(x@polygons, 1000, type='random', iter=1000)
points(bg,col="khaki4",pch=1,cex=0.3)



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
bg_bc = extract(BClim, bg) 
Gspecies_bc = data.frame(lon=subs$lon, lat=subs$lat, Gspecies_bc)

bgpoints = bg@coords
colnames(bgpoints) = c("lon","lat")
bg_bc = data.frame(cbind(bgpoints,bg_bc))
length(which(is.na(bg_bc$bio1))) 
bg_bc = bg_bc[!is.na(bg_bc$bio1), ] 
group_p = kfold(Gspecies_bc, 5) 
group_a = kfold(bg_bc, 5) 

####################################  BUILDIG YOUR SDM  ############################################
test=3
train_p = Gspecies_bc[group_p!=test, c("lon","lat")]
train_a = bg_bc[group_a!=test, c("lon","lat")]
test_p = Gspecies_bc[group_p==test, c("lon","lat")]
test_a = bg_bc[group_a==test, c("lon","lat")]
me = maxent(BClim, p=train_p, a=train_a)
e = evaluate(test_p, test_a, me, BClim)
e
pred_me = predict(me, BClim) 


plot(pred_me, 1, cex=0.5, legend=T, mar=par("mar"), xaxt="n", yaxt="n", main="Predicted Species Distribution")
map("state", xlim=c(leftlon,rightlon), ylim=c(lowerlat,upperlat), fill=F, col="black", add=T)
points(bg,col="snow",pch=1,cex=0.2)
points(Gspecies$lon, Gspecies$lat, col="darkgreen", pch=20, cex=0.5)


# add axes
axis(1,las=1)
axis(2,las=1)
box()

#source("/Users/annacalderon/Desktop/gENM/src/RDataTracker.R")
#ddg.run("/Users/annacalderon/Desktop/gENM/src/BClimBug.R")