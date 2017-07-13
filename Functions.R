# Implement niche overlap (check Vellend)

#Function to simulate 1 death-birth/immigration event
DeathBirthImm <- function(COM, #community composition, length=nr of species, integers (nr of ind per species)
                     freq.meta, #frequencies of the n species in the meta-community
                     fitnesses, #fitnesses of the different species
                     prob.m) #immigration probability
{
  #Measure nr of species
  n <- length(COM)
  #Measure frequencies
  freq <- COM/sum(COM)
  #Kill 1 individual: randomly sample 1 species for which death will occur.
  kill <- sample(c(1:n), 1, prob=freq)
  COM[kill] <- COM[kill] - 1
  #Define species from meta-community that will provide immigrant IF immigration occurs
  immigrant.species <- sample(c(1:n), 1, prob=freq.meta)
  immigrant <- COM*0
  immigrant[immigrant.species] <- 1
  #Calculate probabilities to reproduce IF reproduction occurs
  prob.repro  <- fitnesses*COM/sum(fitnesses*COM)
  #Define species that will produce the newborn IF reproduction occurs
  newborn.species <- sample(c(1:n), 1, prob=prob.repro)
  newborn <- COM*0
  newborn[newborn.species] <- 1
  #Decide if immigration (1) OR local reproduction (2)
  immigration.event <- sample(c(1,2), 1, prob=c(prob.m, 1-prob.m))
  COM <- COM + immigrant*(immigration.event==1) + newborn*(immigration.event==2)
  return(COM)
}

#Function to simulate death-birth/immigration events through time
TimeDynamics <- function(COM.0, #initial community composition
                         num.steps, #nr of time steps 
                         freq.meta, fitnesses, prob.m)
{
  COM <- COM.0
  for (t in (1:num.steps))
  {
    COM <- DeathBirthImm(COM, freq.meta, fitnesses, prob.m)
  }
  return(COM)
}

#Function to execute TimeDynamics multiple times
IterateTimeDynamics <- function(num.itrs, #nr of itarions
                                COM.0, num.steps, freq.meta, fitnesses, prob.m)
{
  All.COM <- NULL
  for (i in c(1:num.itrs))
  {
    COM.i <- TimeDynamics(COM.0, num.steps, freq.meta, fitnesses, prob.m)
    All.COM <- rbind(All.COM, COM.i)
  }
  return(All.COM)
}
