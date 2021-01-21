## R code for generating habitat suitability for different scenarios and occurrence maps.

################################################################################################################################
# Author : François Guilhaumon ### 01/04/2014
# Code to generate all the maps and corresponding tables of the  paper : The effects of phenotypic plasticity and local adaptation on forecasts of species range shifts under climate change #
# Fernando Valladares, Silvia Matesanz, Miguel B. Araujo1, Luis Balaguer, Marta Benito-Garzón, Will Cornwell, Ernesto Gianoli, Francois Guilhaumon, Mark van Kleunen, Daniel E. Naya, Adrienne B. Nicotra, Hendrik Poorter, and  Miguel A. Zavala
# This code was developed under Linux and may not work with Windows or even Mac OS. This code is provided for the sake of exemplification and comes with no warranty or support
#################################################################################################################################

## clean the workspace rm(list=ls())
## load required libraries
library(parallel)
library(lattice)
library(gtools)
library(caTools)

## loading the environmental data
clim <- read.table("../data/a1fi_Hadcm3_max_Temp_warmest.txt",sep="\t",header=T)
xy <- clim[,1:2]

curClim <- clim$maxTwarmest.baseline
futClim <- clim$maxTwarmest.2050

## loading the fitness data
fitness <- read.csv("fitData_final.csv",dec=",")
rownames(fitness) <- fitness[,1]
colnames(fitness) <- c("clim",paste0("curve_",1:46))

#plot fitness
nColours <- 45
jet.colors <- colorRampPalette(c("#27A8D7","#00B050","#FFFF00","#F5970F","#FF0000"))
colour <- jet.colors(nColours)
names(colour) <- names(fitness)[3:47]
plot(fitness[,2]~fitness[,1],type="n",ylab="Fitness",xlab="Climate",main="Fitness ~ Climate")
lapply(names(fitness)[3:47],function(x){points(fitness[,x]~fitness[,1],type="l",col=colour[x],lwd=1.5)})

## scenarios tables and list
#gives which curve correspond to which subpopulation for each scenario
simMat1 <- matrix(0,5,46)
simMat1[,1] <- 1
colnames(simMat1) <- paste0("curve_",1:46)

simMat2 <- matrix(0,5,46)
simMat2[1,2:10] <- 1
simMat2[2,11:19] <- 1
simMat2[3,20:28] <- 1
simMat2[4,29:37] <- 1
simMat2[5,38:46] <- 1
colnames(simMat2) <- paste0("curve_",1:46)

simMat3 <- matrix(0,5,46)
simMat3[1,2:14] <- 1
simMat3[2,15:21] <- 1
simMat3[3,22:26] <- 1
simMat3[4,27:33] <- 1
simMat3[5,34:46] <- 1
colnames(simMat3) <- paste0("curve_",1:46)

simMat4 <- matrix(0,5,46)
simMat4[1,2:5] <- 1
simMat4[2,6:14] <- 1
simMat4[3,15:33] <- 1
simMat4[4,34:42] <- 1
simMat4[5,43:46] <- 1
colnames(simMat4) <- paste0("curve_",1:46)

simMat5 <- matrix(0,5,46)
simMat5[1,2:16] <- 1
simMat5[2,17:28] <- 1
simMat5[3,29:38] <- 1
simMat5[4,39:43] <- 1
simMat5[5,44:46] <- 1
colnames(simMat5) <- paste0("curve_",1:46)

scenList <- list(scen.1=simMat1,scen.2=simMat2,scen.3=simMat3,scen.4=simMat4,scen.5=simMat5)

rm(simMat1,simMat2,simMat3,simMat4,simMat5)

########################## source the function for a threshold source("doThresh.R")

###### do it
doThresh(thresh=.3)
doThresh(thresh=.05)

-----------------------------------------------------------------------------------------------------------------------------
doThresh <- function(thresh=.3){
	#creating and moving to a wd for te threshold
	wd.init <- getwd()
	dirName <- paste("res_",substr(as.character(thresh),2,nchar(as.character(thresh))),sep="")
	dir.create(dirName)
	setwd(paste(wd.init,"/",dirName,sep=""))
	################################################### CURRENT
	cat("################################################### CURRENT\n")
	###### HS
	fitnessCur <- fitness[as.character(curClim),-1]
	fitnessCur <- cbind(xy,fitnessCur)
	write.csv(fitnessCur,file="fitnessCurrent.csv")
	png("currentFitnes.png",2000,2000)
	form <- as.formula(paste(paste(names(fitnessCur)[-c(1,2,3)],collapse="+"),"X+Y",sep="~"))
				print(levelplot(form,fitnessCur,colorkey=list(col=rev(heat.colors(100))),col.regions=rev(heat.colors(100)),cuts=99,par.strip.text=list(cex=0.6),panel = function(...) {
			   	panel.fill(col = "#6EB4D2")
			    	panel.levelplot(...)
			    	
		}))
	dev.off()
	png("currentFitnesCurve1.png",600,600)
	form <- as.formula(paste(paste(names(fitnessCur)[3],collapse="+"),"X+Y",sep="~"))
				print(levelplot(form,fitnessCur,colorkey=list(col=rev(heat.colors(100))),col.regions=rev(heat.colors(100)),cuts=99,par.strip.text=list(cex=0.6),panel = function(...) {
			   	panel.fill(col = "#6EB4D2")
			    	panel.levelplot(...)
			
		}))
	dev.off()
	# checking which population is present in each cell
	fitnessThreshCur <- fitnessCur[,3:48]
	fitnessThreshCur <- apply(fitnessThreshCur,2,function(x){
		x[x<thresh] <- 0
		x
	} )
	simNames <- paste("scen.",1:5,sep="")
	# chossing a population in each cell (1000 times)
	nRes <- 1000
	whichPopsCur <- lapply(simNames,function(x){
		cat("#### Scen: ",x)
				
		res <- mclapply(1:nRes,function(r){
			cat(".")
			
			curves <- lapply(1:5,function(pop){colnames(scenList[[x]])[scenList[[x]][pop,]==1]})
							
			if (x=="scen.1") {
				fitMat <- sapply(curves,function(c)fitnessThreshCur[,c] )
			} else {
				fitMat <- sapply(lapply(curves,function(c)fitnessThreshCur[,c] ),function(m) apply(m,1,max))
			}
			apply(fitMat,1,function(y){
				y <- unlist(y)
				if(sum(y)==0) { 0 } else { which(rmultinom(1,1,y)==1) }
			})
		},mc.cores=10)
		
		res <- do.call(cbind,res)
		
		cat("\n")
		
		res
		
	})
	whichPopsCur <- lapply(whichPopsCur,function(x){data.frame(xy,x)})
	names(whichPopsCur) <- paste("scen.",1:5,sep="")

	#statistics for each population
	popStatsCur <- mclapply(names(whichPopsCur),function(x){
		cat(x,"\n")
		tab <- matrix(NA,1000,6)
				
		for (i in 1:1000) for (j in 0:5) tab[i,j+1] <- sum(whichPopsCur[[x]][,i+2]==j)
		colnames(tab) <- paste("pop",0:5,sep="_")
		tab <- data.frame(tab)
		averages <- sapply(tab,mean)
		sds <- sapply(tab,sd)
			list(tab=tab,stats=data.frame(averages,sds))
	})
	names(popStatsCur) <- paste("scen.",1:5,sep="")

	#export
	lapply(names(whichPopsCur),function(x){
		t <- popStatsCur[[x]]$stats
		fName <- paste(x,"statsPopsCurrent.csv",sep="_")
		write.csv(t,file=fName)
	})
	#plot
	png("boxplot_areas_current.png",2000,400)
	 
	par(mfrow=c(1,5))
	par(mar=c(3.1,2.1,2.1,1))
	lapply(names(whichPopsCur),function(x){
		boxplot(popStatsCur[[x]]$tab[,-1],main=x,col="#C3C3C3",border=c("blue","green","yellow","orange","red"),ylim=c(0,22000))#
	})
	dev.off()
	# couting the populations
	countPops <- lapply(names(whichPopsCur),function(x){
		cat(x,"\n")
		res <- mclapply(rownames(whichPopsCur[[x]]),function(row){
			#cat(".")
			r <- table(unlist(whichPopsCur[[x]][row,-c(1,2)]))
			r1 <- rep(0,5)
			names(r1) <- seq(1,5)
			if(length(names(r))!=1) r1[names(r)] <- r
			if(length(names(r))==1 & names(r)[1]!="0") r1[names(r)] <- r
			r1
		  
		},mc.cores=10)
		#cat("\n")
		do.call(rbind,res)
	})
	names(countPops) <- names(whichPopsCur)
	lapply(names(countPops), function(n){
		fName <- paste(n,"countPopsCurrent.csv",sep="_")
		write.csv(countPops[[n]],file=fName)
	})#eo lapply
	
	#for the plot
	whichPopsCur1Res <- data.frame(whichPopsCur[[1]][,c(1,2)],apply(countPops[[1]],1,function(x){
	
		if(sum(x)==0){ 0 }else{ which.max(x) }
	
	}))
	for (i in 2:5) whichPopsCur1Res <- cbind(whichPopsCur1Res,apply(countPops[[i]],1,function(x){
	
		if(sum(x)==0){ 0 }else{ which.max(x) }
	
	}))
	names(whichPopsCur1Res) <- c("X","Y",names(whichPopsCur))
	write.csv(whichPopsCur1Res,file="popscurrentToPlot.csv")
	
	png("currentPops.png",1162,800)
	form <- as.formula(paste(paste(names(whichPopsCur1Res)[-c(1,2)],collapse="+"),"X+Y",sep="~"))
				print(levelplot(form,whichPopsCur1Res,colorkey=list(col=c("white","blue","green","yellow","orange","red"),at=seq(0.5,6.5),labels=list(labels=as.character(seq(0,5)))),col.regions=c("white","blue","green","yellow","orange","red"),cuts=5,par.strip.text=list(cex=0.9),panel = function(...) {
			   	panel.fill(col = "#6EB4D2")
			    	panel.levelplot(...)
			    	
		}))
	dev.off()
	#### fitness for 5 pops
	cat("#### fitness for 5 pops\n")
	fitPopsCur <- lapply(simNames,function(x){
		cat("#### Scen: ",x)
		curves <- lapply(1:5,function(pop){colnames(scenList[[x]])[scenList[[x]][pop,]==1]})
				
		if (x=="scen.1") {
			fitMat <- sapply(curves,function(c)fitnessThreshCur[,c] )
		} else {
			fitMat <- sapply(lapply(curves,function(c)fitnessThreshCur[,c] ),function(m) apply(m,1,max))
		}
		colnames(fitMat) <- paste("population_",1:5,sep="")
		fitMat <- cbind(xy,fitMat)
		fName <- paste(x,"_currentFitnesPops.csv",sep="")
		write.csv(fitMat,fName)
		cat("\n")
		fitMat
	
	})
	names(fitPopsCur) <- paste("scen.",1:5,sep="")

	#plots
	lapply(names(fitPopsCur),function(n){
		fName <- paste(n,"_currentFitnesPops.png",sep="")
		png(fName,1162,800)
		form <- as.formula(paste(paste(names(fitPopsCur[[n]])[-c(1,2)],collapse="+"),"X+Y",sep="~"))
					print(levelplot(form,fitPopsCur[[n]],colorkey=list(col=rev(heat.colors(100))),col.regions=rev(heat.colors(100)),cuts=99,par.strip.text=list(cex=0.9),panel = function(...) {
				   	panel.fill(col = "#6EB4D2")
				    	panel.levelplot(...)
				    	lpoints(coast$X,coast$Y,type="l",col="black")
			}))
		dev.off()
	})#eo lapply

	##################################################################################     FUTURE
	###################################################cat("################################     FUTURE\n")############################################### HScat("###### HS\n")
	#########################################

	fitnessFut <- fitness[as.character(futClim),-1]
	fitnessFut <- cbind(xy,fitnessFut)
	# checking which population is present in each cell
	fitnessThreshFut <- fitnessFut[,3:48]
	fitnessThreshFut <- apply(fitnessThreshFut,2,function(x){
	  x[x<thresh] <- 0
	  x
	} )
	
	#export
	write.csv(fitnessFut,file="fitnessFuture.csv")
	#plots
	png("futureFitnes.png",2000,2000)
	form <- as.formula(paste(paste(names(fitnessFut)[-c(1,2,3)],collapse="+"),"X+Y",sep="~"))
			print(levelplot(form,fitnessFut,colorkey=list(col=rev(heat.colors(100))),col.regions=rev(heat.colors(100)),cuts=99,par.strip.text=list(cex=0.9),panel = function(...) {
		   	panel.fill(col = "#6EB4D2")
		    	panel.levelplot(...)
		    	
	}))
	dev.off()
	png("futureFitnesCurve1.png",600,600)
	form <- as.formula(paste(paste(names(fitnessFut)[3],collapse="+"),"X+Y",sep="~"))
			print(levelplot(form,fitnessFut,colorkey=list(col=rev(heat.colors(100))),col.regions=rev(heat.colors(100)),cuts=99,par.strip.text=list(cex=0.9),panel = function(...) {
		   	panel.fill(col = "#6EB4D2")
		    	panel.levelplot(...)
		    	
	}))
	dev.off()

	#### fitness for 5 pops
	cat("#### fitness for 5 pops\n")
	fitPopsFut <- lapply(simNames,function(x){
	cat("#### Scen: ",x)
	curves <- lapply(1:5,function(pop){colnames(scenList[[x]])[scenList[[x]][pop,]==1]})
			
	if (x=="scen.1") {
		fitMat <- sapply(curves,function(c)fitnessThreshFut[,c] )
	} else {
		fitMat <- sapply(lapply(curves,function(c)fitnessThreshFut[,c] ),function(m) apply(m,1,max))
	}
	colnames(fitMat) <- paste("population_",1:5,sep="")
	fitMat <- cbind(xy,fitMat)
	fName <- paste(x,"_futureFitnesPops.csv",sep="")
	write.csv(fitMat,fName)
	cat("\n")
	fitMat
	})
	names(fitPopsFut) <- paste("scen.",1:5,sep="")

	#plots
	lapply(names(fitPopsFut),function(n){
	fName <- paste(n,"_futureFitnesPops.png",sep="")
	png(fName,1162,800)
	form <- as.formula(paste(paste(names(fitPopsFut[[n]])[-c(1,2)],collapse="+"),"X+Y",sep="~"))
				print(levelplot(form,fitPopsFut[[n]],colorkey=list(col=rev(heat.colors(100))),col.regions=rev(heat.colors(100)),cuts=99,par.strip.text=list(cex=0.9),panel = function(...) {
			   	panel.fill(col = "#6EB4D2")
			    	panel.levelplot(...)
			    	
		}))
	dev.off()
	})#eo lapply

	###### Difference in HS
	library(gplots)
	fitnessDiff <- fitnessFut[,-c(1,2)] - fitnessCur[,-c(1,2)]
	fitnessDiff <- cbind(xy,fitnessDiff)
	write.csv(fitnessDiff,file="fitnessDiff.csv")
	png("delta_fitness.png",1999,1998)
	form <- as.formula(paste(paste(names(fitnessDiff)[-c(1,2)],collapse="+"),"X+Y",sep="~"))
			print(levelplot(form,fitnessDiff,colorkey=list(col=rev(rich.colors(100))),col.regions=rev(rich.colors(100)),cuts=99,par.strip.text=list(cex=0.9),panel = function(...) {
		   	panel.fill(col = "#D4D4D4")
		    	panel.levelplot(...)
		    	
	}))
	dev.off()

	#### fitness diff for 5 pops
	lapply(names(fitPopsCur),function(n){
	fitnessPopsDiff <- fitPopsFut[[n]][,-c(1,2)] - fitPopsCur[[n]][,-c(1,2)]
	fitnessPopsDiff <- cbind(xy,fitnessPopsDiff)
	#export
	fName <-  paste(n,"fitnessPopsDiff.csv",sep="")
	write.csv(fitnessPopsDiff,file=fName)
	fName <-  paste(n,"fitnessPopsDiff.png",sep="")
	#plot
	png(fName,1162,800)
	form <- as.formula(paste(paste(names(fitnessPopsDiff)[-c(1,2)],collapse="+"),"X+Y",sep="~"))
				print(levelplot(form,fitnessPopsDiff,colorkey=list(col=rev(rich.colors(100))),col.regions=rev(rich.colors(100)),cuts=99,par.strip.text=list(cex=0.9),panel = function(...) {
			   	panel.fill(col = "#D4D4D4")
			    	panel.levelplot(...)
		}))
	dev.off()
	})#eo lapply

	######################################################### FULL DISPERSAL	cat("################ FULL DISPERSAL\n")
	#########################################

	# chossing a population in each cell (1000 times)
	whichPopsFut <- lapply(simNames,function(x){
	cat("#### Scen: ",x)
		
	res <- mclapply(1:nRes,function(r){
		cat(".")
		curves <- lapply(1:5,function(pop){colnames(scenList[[x]])[scenList[[x]][pop,]==1]})
					
		if (x=="scen.1") {
			fitMat <- sapply(curves,function(c)fitnessThreshFut[,c] )
		} else {
			fitMat <- sapply(lapply(curves,function(c)fitnessThreshFut[,c] ),function(m) apply(m,1,max))
		}
		apply(fitMat,1,function(y){
			y <- unlist(y)
			if(sum(y)==0) { 0 } else { which(rmultinom(1,1,y)==1) }
		})
	},mc.cores=10)
	res <- do.call(cbind,res)
	cat("\n")
	res
	})
	whichPopsFut <- lapply(whichPopsFut,function(x){data.frame(xy,x)})
	names(whichPopsFut) <- paste("scen.",1:5,sep="")
	popStatsFut <- mclapply(names(whichPopsFut),function(x){
	tab <- matrix(NA,1000,6)
			
	for (i in 1:1000) for (j in 0:5) tab[i,j+1] <- sum(whichPopsFut[[x]][,i+2]==j)
	colnames(tab) <- paste("pop",0:5,sep="_")
	tab <- data.frame(tab)
	names(tab) <- paste("pop",0:5,sep="_")
	averages <- sapply(tab,mean)
	sds <- sapply(tab,sd)
	list(tab=tab,stats=data.frame(averages,sds))
	},mc.cores=10)
	names(popStatsFut) <- paste("scen.",1:5,sep="")
	#export
	lapply(names(whichPopsFut),function(x){
	t <- popStatsFut[[x]]$stats
	fName <- paste(x,"statsPopsFuture_full.csv",sep="_")
	write.csv(t,file=fName)
	})
	#plots
	png("boxplot_areas_future_full.png",2000,400)
	par(mfrow=c(1,5))
	par(mar=c(3.1,2.1,2.1,1))
	lapply(names(whichPopsFut),function(x){
	boxplot(popStatsFut[[x]]$tab[,-1],main=x,col="#C3C3C3",border=c("blue","green","yellow","orange","red"),ylim=c(0,22000))#
	})
	dev.off()
	#couting the populations
	countPopsFut <- mclapply(names(whichPopsFut),function(x){
	cat(x,"\n")
	res <- mclapply(rownames(whichPopsFut[[x]]),function(row){
		#cat(".")
		r <- table(unlist(whichPopsFut[[x]][row,-c(1,2)]))
		r1 <- rep(0,5)
		names(r1) <- seq(1,5)
		if(length(names(r))!=1) r1[names(r)] <- r
		if(length(names(r))==1 & names(r)[1]!="0") r1[names(r)] <- r
		r1
	},mc.cores=10)
	#cat("\n")
	do.call(rbind,res)
	})
	names(countPopsFut) <- names(whichPopsFut)

	#export
	lapply(names(countPopsFut), function(n){
	fName <- paste(n,"countPopsFuture_full.csv",sep="_")
	write.csv(countPopsFut[[n]],file=fName)
	})#eo lapply

	#for the plot
	whichPopsFut1Res <- data.frame(whichPopsFut[[1]][,c(1,2)],apply(countPopsFut[[1]],1,function(x){
	if(sum(x)==0){ 0 }else{ which.max(x) }
	}))
	for (i in 2:5) whichPopsFut1Res <- cbind(whichPopsFut1Res,apply(countPopsFut[[i]],1,function(x){
	if(sum(x)==0){ 0 }else{ which.max(x) }
	}))
	names(whichPopsFut1Res) <- c("X","Y",names(whichPopsFut))
	write.csv(whichPopsFut1Res,file="popsFutureToPlot_full.csv")

	#plots
	png("futurePops_full.png",1162,800)
	form <- as.formula(paste(paste(names(whichPopsFut1Res)[-c(1,2)],collapse="+"),"X+Y",sep="~"))
	print(levelplot(form,whichPopsFut1Res,colorkey=list(col=c("white","blue","green","yellow","orange","red"),at=seq(0.5,6.5),labels=list(labels=as.character(seq(0,5)))),col.regions=c("white","blue","green","yellow","orange","red"),cuts=5,par.strip.text=list(cex=0.9),panel = function(...) {
		   	panel.fill(col = "#6EB4D2")
		    	panel.levelplot(...)
		    	
	}))
	dev.off()

	##########################################	################ NO DISPERSAL cat("################ NO DISPERSAL\n") #########################################

	#checking which pops are present in each grid cell
	whichPopsFut_nd <- mclapply(names(whichPopsCur),function(x){
		cat(x,"\n")
		curves <- lapply(1:5,function(pop){colnames(scenList[[x]])[scenList[[x]][pop,]==1]})
		if (x=="scen.1") {
		  fitMat <- sapply(curves,function(c)fitnessThreshFut[,c] )
		} else {
		  fitMat <- sapply(lapply(curves,function(c)fitnessThreshFut[,c] ),function(m) apply(m,1,max))
		}
		r <- whichPopsCur[[x]][,-c(1,2)] 
		res <- mclapply(1:length(r),function(c){
			l <- r[,c]
		    
			sapply(1:length(l),function(k){
				if(l[k]==0) return(0)
				val <- fitMat[k,l[k]]
				if(val==0) {
				return(0)
				}else{
				return(l[k])
				}
			})
		},mc.cores=2)#eo mclapply
		res <- do.call(cbind,res)
	},mc.cores=5)
	whichPopsFut_nd <- lapply(whichPopsFut_nd,function(x){data.frame(xy,x)})
	names(whichPopsFut_nd) <- paste("scen.",1:5,sep="")

	#statistics
	popStatsFut_nd <- mclapply(names(whichPopsFut_nd),function(x){
		tab <- matrix(NA,1000,6)
			
		for (i in 1:1000) for (j in 0:5) tab[i,j+1] <- sum(whichPopsFut_nd[[x]][,i+2]==j)
		colnames(tab) <- paste("pop",0:5,sep="_")
		tab <- data.frame(tab)
		names(tab) <- paste("pop",0:5,sep="_")
		averages <- sapply(tab,mean)
		sds <- sapply(tab,sd)
		list(tab=tab,stats=data.frame(averages,sds))
	},mc.cores=10)
	names(popStatsFut_nd) <- paste("scen.",1:5,sep="")
	#export
	lapply(names(whichPopsFut_nd),function(x){
		t <- popStatsFut_nd[[x]]$stats
		fName <- paste(x,"statsPopsFuture_nd.csv",sep="_")
		write.csv(t,file=fName)
	})
	png("boxplot_areas_future_nd.png",2000,400)
	par(mfrow=c(1,5))
	par(mar=c(3.1,2.1,2.1,1))
	lapply(names(whichPopsFut_nd),function(x){
		boxplot(popStatsFut_nd[[x]]$tab[,-1],main=x,col="#C3C3C3",border=c("blue","green","yellow","orange","red"),ylim=c(0,15000))
	})
	dev.off()
	countPopsFut_nd <- mclapply(names(whichPopsFut_nd),function(x){
	cat(x,"\n")
	res <- mclapply(rownames(whichPopsFut_nd[[x]]),function(row){
		r <- table(unlist(whichPopsFut_nd[[x]][row,-c(1,2)]))
		r1 <- rep(0,6)
		names(r1) <- seq(0,5)
		r1[names(r)] <- r
		r1
	  
	},mc.cores=3)

	do.call(rbind,res)
	},mc.cores=3)
	countPopsFut_nd <- lapply(countPopsFut_nd,function(x){data.frame(xy,x)})
	names(countPopsFut_nd) <- names(whichPopsFut_nd)
	lapply(names(countPopsFut_nd), function(n){
	fName <- paste(n,"countPopsFuture_nd.csv",sep="_")
	write.csv(countPopsFut_nd[[n]],file=fName)
	})#eo lapply
	#for the plot
	#for the plot
	whichPopsFut1Res_nd <- whichPopsFut_nd[[1]][,c(1,2,3)]
	for (i in 2:5) whichPopsFut1Res_nd <- cbind(whichPopsFut1Res_nd,whichPopsFut_nd[[i]][,3])
	names(whichPopsFut1Res_nd) <- c("X","Y",names(whichPopsFut_nd))
	write.csv(whichPopsFut1Res_nd,file="popsFutureToPlot_nd.csv")
	png("futurePops_nd.png",1162,800)
	form <- as.formula(paste(paste(names(whichPopsFut1Res_nd)[-c(1,2)],collapse="+"),"X+Y",sep="~"))
	print(levelplot(form,whichPopsFut1Res_nd,colorkey=list(col=c("white","blue","green","yellow","orange","red"),at=seq(0.5,6.5),labels=list(labels=as.character(seq(0,5)))),col.regions=c("white","blue","green","yellow","orange","red"),cuts=5,par.strip.text=list(cex=0.9),panel = function(...) {
		   	panel.fill(col = "#6EB4D2")
		    	panel.levelplot(...)
		    	
	}))
	dev.off()

	####################### deltas for all simulationscat("####################### deltas for all simulations\n")
	#delta current - future full dispersal
	png("boxplot_delta_full.png",2000,400)
	par(mfrow=c(1,5))
	par(mar=c(3.1,2.1,2.1,1))
	deltaFull <- lapply(names(whichPopsCur),function(x){
		tabCur <- matrix(NA,1000,6)
			
		for (i in 1:1000) for (j in 0:5) tabCur[i,j+1] <- sum(whichPopsCur[[x]][,i+2]==j)
		colnames(tabCur) <- paste("pop",0:5,sep="_")
		tabFut <- matrix(NA,1000,6)
			
		for (i in 1:1000) for (j in 0:5) tabFut[i,j+1] <- sum(whichPopsFut[[x]][,i+2]==j)
		colnames(tabFut) <- paste("pop",0:5,sep="_")
		tab <- tabFut - tabCur
		colnames(tab) <- paste("pop",0:5,sep="_")
		tab <- data.frame(tab)
		averages <- sapply(tab,mean)
		sds <- sapply(tab,sd)
		boxplot(tab,main=x,col="#C3C3C3",border=c("blue","green","yellow","orange","red")) 
		abline(h=0,lty=2)
		data.frame(averages,sds)
	})
	dev.off()
	names(deltaFull) <- names(whichPopsFut_nd)
	lapply(names(deltaFull),function(x){
		fName <- paste(x,"_delta_full.csv",sep="")
		write.csv(deltaFull[[x]],file=fName)
	})#eo lapply
	png("boxplot_delta_nd.png",2000,400)
	par(mfrow=c(1,5))
	par(mar=c(3.1,2.1,2.1,1))
	deltaFull_nd <- lapply(names(whichPopsCur),function(x){
		tabCur <- matrix(NA,1000,6)
			
		for (i in 1:1000) for (j in 0:5) tabCur[i,j+1] <- sum(whichPopsCur[[x]][,i+2]==j)
		colnames(tabCur) <- paste("pop",0:5,sep="_")
		tabCur <- tabCur[,-1]
		tabFut <- matrix(NA,1000,6)
		for (i in 1:1000) for (j in 0:5) tabFut[i,j+1] <- sum(whichPopsFut_nd[[x]][,i+2]==j)

		colnames(tabFut) <- paste("pop",0:5,sep="_")
		tabFut <- tabFut[,-1]
		tab <- tabFut - tabCur
		names(tab) <- paste("pop",1:5,sep="_")
		tab <- data.frame(tab)
		averages <- sapply(tab,mean)
		sds <- sapply(tab,sd)
		boxplot(tab,main=x,col="#C3C3C3",border=c("blue","green","yellow","orange","red")) 
		abline(h=0,lty=2)
		data.frame(averages,sds)
	})
	dev.off()
	names(deltaFull_nd) <- names(whichPopsFut_nd)
	lapply(names(deltaFull_nd),function(x){
		fName <- paste(x,"_delta_nd.csv",sep="")
		write.csv(deltaFull_nd[[x]],file=fName)
	})#eo lapply

	####################### proportions for all simulations
	cat("####################### proportions for all simulations\n")
	#proportion current - future full dispersal
	png("boxplot_proportions_full.png",2000,400)
	par(mfrow=c(1,5))
	par(mar=c(3.1,2.1,2.1,1))
	propFull <- lapply(names(whichPopsCur),function(x){
		tabCur <- matrix(NA,1000,6)
			
		for (i in 1:1000) for (j in 0:5) tabCur[i,j+1] <- sum(whichPopsCur[[x]][,i+2]==j)
		colnames(tabCur) <- paste("pop",0:5,sep="_")
		tabCur <- tabCur[,-1]
		tabFut <- matrix(NA,1000,6)
			
		for (i in 1:1000) for (j in 0:5) tabFut[i,j+1] <- sum(whichPopsFut[[x]][,i+2]==j)
		colnames(tabFut) <- paste("pop",0:5,sep="_")
		tabFut <- tabFut[,-1]
		tab <- tabFut / tabCur
		names(tab) <- paste("pop",1:5,sep="_")
		tab <- data.frame(tab)
		averages <- sapply(tab,mean)
		cat(x," : means ",averages,"\n")
		sds <- sapply(tab,sd)
		boxplot(tab,main=x,col="#C3C3C3",border=c("blue","green","yellow","orange","red"),ylim=c(0,2))
		data.frame(averages,sds)
	})
	dev.off()
	names(propFull) <- names(whichPopsFut_nd)
	lapply(names(propFull),function(x){
		fName <- paste(x,"_prop_full.csv",sep="")
		write.csv(propFull[[x]],file=fName)
	})#eo lapply
	png("boxplot_prop_nd.png",2000,400)
	par(mfrow=c(1,5))
	par(mar=c(3.1,2.1,2.1,1))
	propNd <- lapply(names(whichPopsCur),function(x){
		tabCur <- matrix(NA,1000,6)
			
		for (i in 1:1000) for (j in 0:5) tabCur[i,j+1] <- sum(whichPopsCur[[x]][,i+2]==j)
		colnames(tabCur) <- paste("pop",0:5,sep="_")
		tabCur <- tabCur[,-1]
		tabFut <- matrix(NA,1000,6)
			
		for (i in 1:1000) for (j in 0:5) tabFut[i,j+1] <- sum(whichPopsFut_nd[[x]][,i+2]==j)
		colnames(tabFut) <- paste("pop",0:5,sep="_")
		tabFut <- tabFut[,-1]
		tab <- tabFut / tabCur
		names(tab) <- paste("pop",1:5,sep="_")
		tab <- data.frame(tab)
		averages <- sapply(tab,mean)
		cat(x," : means ",averages,"\n")
		sds <- sapply(tab,sd)
		boxplot(tab,main=x,col="#C3C3C3",border=c("blue","green","yellow","orange","red"),ylim=c(0,1))
		data.frame(averages,sds)
	})
	dev.off()
	names(propNd) <- names(whichPopsFut_nd)
	lapply(names(propNd),function(x){
		fName <- paste(x,"_prop_nd.csv",sep="")
		write.csv(propNd[[x]],file=fName)
	})#eo lapply

	cat("########## COMPRESSING THE RESULTS...\n")
	com <- paste("tar cvzf resPops_thresh",substr(as.character(thresh),2,nchar(as.character(thresh))),".tgz *.*",sep="")
	system(com)
	setwd(wd.init)
	cat("################################ DONE !\n")
	cat("##################################### !\n")

}#eo doThresh
