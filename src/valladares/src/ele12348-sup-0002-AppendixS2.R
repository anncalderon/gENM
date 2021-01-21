### SUPLEMENTARY MATERIAL  
## R code for generating the different intraspecific scenarios used in Fig. 1 (conceptual model)

#Intraspecific scenario 1, No differentiation

w <- seq(10,25,by=0.1) #specification of the climatic gradient
plot(w, (1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.5, sd=2.3), type="l", col="black", xlab="Temperature", ylab="Fitness",
	lty = c(1),lwd=2,cex.lab=1.8, cex.axis=1.3) #definition of the overall niche shape 



##Intraspecific scenario 2, Local adaptation, equal plasticity

w <- seq(10,25,by=0.1)
plot(w, (1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.5, sd=2.3), type="l", col="black", xlab="Temperature", ylab="Fitness",
	lty = c(1),lwd=3,cex.lab=1.8, cex.axis=1.3)


#definition of each subpopulation, varying its fitness and temperature optimum and the assignment to a population/color

lines(w,0.392*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.5, sd=0.9), type="l", col="yellow") 
lines(w,0.388*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.27, sd=0.9), type="l", col="yellow")
lines(w,0.388*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.73, sd=0.9), type="l", col="yellow")
lines(w,0.383*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.04, sd=0.9), type="l", col="yellow")
lines(w,0.383*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.96, sd=0.9), type="l", col="yellow")
lines(w,0.374*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.81, sd=0.9), type="l", col="yellow")
lines(w,0.374*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.19, sd=0.9), type="l", col="yellow")
lines(w,0.359*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.58, sd=0.9), type="l", col="yellow")
lines(w,0.359*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.42, sd=0.9), type="l", col="yellow")

lines(w,0.34*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.35, sd=0.9), type="l", col="green")
lines(w,0.32*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.12, sd=0.9), type="l", col="green")
lines(w,0.29*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.89, sd=0.9), type="l", col="green")
lines(w,0.27*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.66, sd=0.9), type="l", col="green")
lines(w,0.24*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.43, sd=0.9), type="l", col="green")
lines(w,0.22*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.2, sd=0.9), type="l", col="green")
lines(w,0.19*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.97, sd=0.9), type="l", col="green")
lines(w,0.17*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.74, sd=0.9), type="l", col="green")
lines(w,0.145*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.51, sd=0.9), type="l", col="green")

lines(w,0.125*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.28, sd=0.9), type="l", col="blue")
lines(w,0.105*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.05, sd=0.9), type="l", col="blue")
lines(w,0.09*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.82, sd=0.9), type="l", col="blue")
lines(w,0.07*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.59, sd=0.9), type="l", col="blue")
lines(w,0.06*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.36, sd=0.9), type="l", col="blue")
lines(w,0.05*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.13, sd=0.9), type="l", col="blue")
lines(w,0.04*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=12.9, sd=0.9), type="l", col="blue")
lines(w,0.03*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=12.67, sd=0.9), type="l", col="blue")
lines(w,0.02*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=12.44, sd=0.9), type="l", col="blue")

lines(w,0.34*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.65, sd=0.9), type="l", col="orange")
lines(w,0.32*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.88, sd=0.9), type="l", col="orange")
lines(w,0.29*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.11, sd=0.9), type="l", col="orange")
lines(w,0.27*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.34, sd=0.9), type="l", col="orange")
lines(w,0.24*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.57, sd=0.9), type="l", col="orange")
lines(w,0.22*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.8, sd=0.9), type="l", col="orange")
lines(w,0.19*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.03, sd=0.9), type="l", col="orange")
lines(w,0.17*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.26, sd=0.9), type="l", col="orange")
lines(w,0.145*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.49, sd=0.9), type="l", col="orange")

lines(w,0.125*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.72, sd=0.9), type="l", col="red")
lines(w,0.105*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.95, sd=0.9), type="l", col="red")
lines(w,0.09*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.18, sd=0.9), type="l", col="red")
lines(w,0.07*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.41, sd=0.9), type="l", col="red")
lines(w,0.06*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.64, sd=0.9), type="l", col="red")
lines(w,0.05*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.87, sd=0.9), type="l", col="red")
lines(w,0.04*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=22.1, sd=0.9), type="l", col="red")
lines(w,0.03*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=22.33, sd=0.9), type="l", col="red")
lines(w,0.02*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=22.56, sd=0.9), type="l", col="red")

#Intraspecific scenario 3, High margin plasticity

w <- seq(10,25,by=0.1)
plot(w, (1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.5, sd=2.3), type="l", col="black", xlab="Temperature", ylab="Fitness",
	lty = c(1),lwd=3,cex.lab=1.8, cex.axis=1.3)

#definition of each subpopulation, varying its fitness and temperature optimum and the assignment to a population/color

lines(w,0.392*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.5, sd=0.9), type="l", col="yellow") 
lines(w,0.388*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.27, sd=0.9), type="l", col="yellow")
lines(w,0.388*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.73, sd=0.9), type="l", col="yellow")
lines(w,0.383*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.04, sd=0.9), type="l", col="yellow")
lines(w,0.383*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.96, sd=0.9), type="l", col="yellow")

lines(w,0.374*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.81, sd=0.9), type="l", col="green")
lines(w,0.359*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.58, sd=0.9), type="l", col="green")
lines(w,0.34*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.35, sd=0.9), type="l", col="green")
lines(w,0.32*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.12, sd=0.9), type="l", col="green")
lines(w,0.29*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.89, sd=0.9), type="l", col="green")
lines(w,0.27*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.66, sd=0.9), type="l", col="green")
lines(w,0.24*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.43, sd=0.9), type="l", col="green")

lines(w,0.374*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.19, sd=0.9), type="l", col="orange")
lines(w,0.359*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.42, sd=0.9), type="l", col="orange")
lines(w,0.34*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.65, sd=0.9), type="l", col="orange")
lines(w,0.32*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.88, sd=0.9), type="l", col="orange")
lines(w,0.29*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.11, sd=0.9), type="l", col="orange")
lines(w,0.27*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.34, sd=0.9), type="l", col="orange")
lines(w,0.24*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.57, sd=0.9), type="l", col="orange")

lines(w,0.22*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.2, sd=0.9), type="l", col="blue")
lines(w,0.19*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.97, sd=0.9), type="l", col="blue")
lines(w,0.17*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.74, sd=0.9), type="l", col="blue")
lines(w,0.145*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.51, sd=0.9), type="l", col="blue")
lines(w,0.125*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.28, sd=0.9), type="l", col="blue")
lines(w,0.105*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.05, sd=0.9), type="l", col="blue")
lines(w,0.09*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.82, sd=0.9), type="l", col="blue")
lines(w,0.07*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.59, sd=0.9), type="l", col="blue")
lines(w,0.06*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.36, sd=0.9), type="l", col="blue")
lines(w,0.05*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.13, sd=0.9), type="l", col="blue")
lines(w,0.04*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=12.9, sd=0.9), type="l", col="blue")
lines(w,0.03*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=12.67, sd=0.9), type="l", col="blue")
lines(w,0.02*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=12.44, sd=0.9), type="l", col="blue")

lines(w,0.22*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.8, sd=0.9), type="l", col="red")
lines(w,0.19*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.03, sd=0.9), type="l", col="red")
lines(w,0.17*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.26, sd=0.9), type="l", col="red")
lines(w,0.145*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.49, sd=0.9), type="l", col="red")
lines(w,0.125*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.72, sd=0.9), type="l", col="red")
lines(w,0.105*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.95, sd=0.9), type="l", col="red")
lines(w,0.09*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.18, sd=0.9), type="l", col="red")
lines(w,0.07*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.41, sd=0.9), type="l", col="red")
lines(w,0.06*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.64, sd=0.9), type="l", col="red")
lines(w,0.05*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.87, sd=0.9), type="l", col="red")
lines(w,0.04*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=22.1, sd=0.9), type="l", col="red")
lines(w,0.03*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=22.33, sd=0.9), type="l", col="red")
lines(w,0.02*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=22.56, sd=0.9), type="l", col="red")


#Intraspecific scenario 4, High central plasticity


w <- seq(10,25,by=0.1)
plot(w, (1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.5, sd=2.3), type="l", col="black", xlab="Temperature", ylab="Fitness",
	lty = c(1),lwd=3,cex.lab=1.8, cex.axis=1.3)

#definition of each subpopulation, varying its fitness and temperature optimum and the assignment to a population/color

lines(w,0.392*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.5, sd=0.9), type="l", col="yellow") 
lines(w,0.388*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.27, sd=0.9), type="l", col="yellow")
lines(w,0.388*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.73, sd=0.9), type="l", col="yellow")
lines(w,0.383*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.04, sd=0.9), type="l", col="yellow")
lines(w,0.383*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.96, sd=0.9), type="l", col="yellow")
lines(w,0.374*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.81, sd=0.9), type="l", col="yellow")
lines(w,0.374*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.19, sd=0.9), type="l", col="yellow")
lines(w,0.359*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.58, sd=0.9), type="l", col="yellow")
lines(w,0.359*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.42, sd=0.9), type="l", col="yellow")
lines(w,0.34*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.35, sd=0.9), type="l", col="yellow")
lines(w,0.34*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.65, sd=0.9), type="l", col="yellow")
lines(w,0.32*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.12, sd=0.9), type="l", col="yellow")
lines(w,0.32*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.88, sd=0.9), type="l", col="yellow")
lines(w,0.29*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.89, sd=0.9), type="l", col="yellow")
lines(w,0.29*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.11, sd=0.9), type="l", col="yellow")
lines(w,0.27*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.34, sd=0.9), type="l", col="yellow")
lines(w,0.27*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.66, sd=0.9), type="l", col="yellow")
lines(w,0.24*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.43, sd=0.9), type="l", col="yellow")
lines(w,0.24*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.57, sd=0.9), type="l", col="yellow")

lines(w,0.22*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.2, sd=0.9), type="l", col="green")
lines(w,0.19*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.97, sd=0.9), type="l", col="green")
lines(w,0.17*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.74, sd=0.9), type="l", col="green")
lines(w,0.145*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.51, sd=0.9), type="l", col="green")
lines(w,0.125*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.28, sd=0.9), type="l", col="green")
lines(w,0.105*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.05, sd=0.9), type="l", col="green")
lines(w,0.09*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.82, sd=0.9), type="l", col="green")
lines(w,0.07*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.59, sd=0.9), type="l", col="green")
lines(w,0.06*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.36, sd=0.9), type="l", col="green")
lines(w,0.05*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.13, sd=0.9), type="l", col="blue")
lines(w,0.04*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=12.9, sd=0.9), type="l", col="blue")
lines(w,0.03*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=12.67, sd=0.9), type="l", col="blue")
lines(w,0.02*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=12.44, sd=0.9), type="l", col="blue")

lines(w,0.22*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.8, sd=0.9), type="l", col="orange")
lines(w,0.19*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.03, sd=0.9), type="l", col="orange")
lines(w,0.17*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.26, sd=0.9), type="l", col="orange")
lines(w,0.145*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.49, sd=0.9), type="l", col="orange")
lines(w,0.125*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.72, sd=0.9), type="l", col="orange")
lines(w,0.105*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.95, sd=0.9), type="l", col="orange")
lines(w,0.09*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.18, sd=0.9), type="l", col="orange")
lines(w,0.07*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.41, sd=0.9), type="l", col="orange")
lines(w,0.06*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.64, sd=0.9), type="l", col="orange")

lines(w,0.05*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.87, sd=0.9), type="l", col="red")
lines(w,0.04*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=22.1, sd=0.9), type="l", col="red")
lines(w,0.03*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=22.33, sd=0.9), type="l", col="red")
lines(w,0.02*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=22.56, sd=0.9), type="l", col="red")

#Intraspecific scenario 5, High leadind edge plasticity


w <- seq(10,25,by=0.1)
plot(w, (1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.5, sd=2.3), type="l", col="black", xlab="Temperature", ylab="Fitness",
	lty = c(1),lwd=3,cex.lab=1.8, cex.axis=1.3)


#definition of each subpopulation, varying its fitness and temperature optimum and the assignment to a population/color

lines(w,0.392*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.5, sd=0.9), type="l", col="green") 
lines(w,0.388*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.27, sd=0.9), type="l", col="green")
lines(w,0.388*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.73, sd=0.9), type="l", col="green")
lines(w,0.383*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.04, sd=0.9), type="l", col="green")
lines(w,0.383*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=17.96, sd=0.9), type="l", col="green")
lines(w,0.374*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.81, sd=0.9), type="l", col="green")
lines(w,0.374*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.19, sd=0.9), type="l", col="green")
lines(w,0.359*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.58, sd=0.9), type="l", col="green")
lines(w,0.359*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.42, sd=0.9), type="l", col="green")
lines(w,0.34*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.35, sd=0.9), type="l", col="green")
lines(w,0.32*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=16.12, sd=0.9), type="l", col="green")
lines(w,0.29*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.89, sd=0.9), type="l", col="green")

lines(w,0.27*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.66, sd=0.9), type="l", col="blue")
lines(w,0.24*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.43, sd=0.9), type="l", col="blue")
lines(w,0.22*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=15.2, sd=0.9), type="l", col="blue")
lines(w,0.19*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.97, sd=0.9), type="l", col="blue")
lines(w,0.17*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.74, sd=0.9), type="l", col="blue")
lines(w,0.145*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.51, sd=0.9), type="l", col="blue")
lines(w,0.125*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.28, sd=0.9), type="l", col="blue")
lines(w,0.105*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=14.05, sd=0.9), type="l", col="blue")
lines(w,0.09*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.82, sd=0.9), type="l", col="blue")
lines(w,0.07*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.59, sd=0.9), type="l", col="blue")
lines(w,0.06*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.36, sd=0.9), type="l", col="blue")
lines(w,0.05*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=13.13, sd=0.9), type="l", col="blue")
lines(w,0.04*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=12.9, sd=0.9), type="l", col="blue")
lines(w,0.03*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=12.67, sd=0.9), type="l", col="blue")
lines(w,0.02*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=12.44, sd=0.9), type="l", col="blue")

lines(w,0.34*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.65, sd=0.9), type="l", col="yellow")
lines(w,0.32*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=18.88, sd=0.9), type="l", col="yellow")
lines(w,0.29*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.11, sd=0.9), type="l", col="yellow")
lines(w,0.27*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.34, sd=0.9), type="l", col="yellow")
lines(w,0.24*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.57, sd=0.9), type="l", col="yellow")
lines(w,0.22*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=19.8, sd=0.9), type="l", col="yellow")
lines(w,0.19*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.03, sd=0.9), type="l", col="yellow")
lines(w,0.17*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.26, sd=0.9), type="l", col="yellow")
lines(w,0.145*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.49, sd=0.9), type="l", col="yellow")
lines(w,0.125*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.72, sd=0.9), type="l", col="yellow")

lines(w,0.105*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=20.95, sd=0.9), type="l", col="orange")
lines(w,0.09*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.18, sd=0.9), type="l", col="orange")
lines(w,0.07*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.41, sd=0.9), type="l", col="orange")
lines(w,0.06*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.64, sd=0.9), type="l", col="orange")
lines(w,0.05*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=21.87, sd=0.9), type="l", col="orange")

lines(w,0.04*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=22.1, sd=0.9), type="l", col="red")
lines(w,0.03*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=22.33, sd=0.9), type="l", col="red")
lines(w,0.02*(1/dnorm(17.5, mean=17.5, sd=2.3))*dnorm(w, mean=22.56, sd=0.9), type="l", col="red")
 
