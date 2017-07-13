# Source all functions
source("Functions.R")

# Set parameters
num.itrs  <- 50 #nr of iterations
n         <- 10 #nr of species
J         <- 100 #community size; fixed
COM.0     <- rep(ceiling(J/n),n)
num.steps <- 500
freq.meta <- runif(n, 1, 100) #frequencies in the meta-community
freq.meta <- freq.meta/sum(freq.meta)
prob.m    <- 0.05 #immigration probability
fit.min   <- 1
fit.max   <- 2
fitnesses <- runif(n, fit.min, fit.max) 

#Run
Test <- IterateTimeDynamics(num.itrs=num.itrs,COM.0=COM.0, 
                            num.steps=num.steps, freq.meta=freq.meta, 
                            fitnesses=fitnesses, prob.m=prob.m)



#Initialize plot
plot(0,0, xlab = "Time", col="transparent",
     ylab = "Frequency of species 1", 
     ylim = c(0, 1), xlim=c(1,num.years))