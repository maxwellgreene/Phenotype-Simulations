#This file contains four functions:
#   runSimSoft - main simulation with manual param input
#   runSimSoftRep - repeatedly run runSimSoft and average output
#   runSimSoftAllEnv - repeatedly run runSimSoftRep with variety of colonies 
#                      to test for best colony for given environment
#   runSimSoftAllCol - repeatedly run runSimSoftRep with variety of environments 
#                      to test for best environment for given colony


#runSimSoft:
#params
#   enviro - length 4 vector of quantities with scalars for seasonal 
#            mortality rate functions and a scaling number

runSimSoft <- function(enviro,colony,nDayCycle=100,nReprod=1)
{
  #Starting number of workers and energy stores
  Day <- 0; nWorker <- 0; kStore <- 0;
  #Number of trips that workers and reproductives can make each day
  nTripReprod <- 2; nTripWorker <- 2.5;
  #Set amount of energy per trip that workers and reproductives make
  kTripWorker <- 2; kTripReprod <- 2;
  #Set amount of energy needed to create a worker and reproductive
  kCreateWorker <- 5; kCreateReprod <- 5.5;
  
  #Create dataframe to store data.
  data <- data.frame(timestep=0,
                     nReprod=nReprod,
                     nWorker=nWorker,
                     kStore=kStore)
  
  for (i in 1:nDayCycle)
  {
    Day <- i;
    
    #Forage as much as you can/need with workers
    nWorker <- ceiling(nWorker*fMortRateSoft(enviro,Day,nDayCycle));
    kStore <- kStore + nWorker * nTripWorker;
    
    #Are the reproductives foraging? 
    if(isReprodForageSoft(nWorker=nWorker))
    {
      nReprod <- ceiling(nReprod*fMortRateSoft(enviro,Day,nDayCycle));
      kStore <- kStore + nReprod * nTripReprod;
    }
    
    while (kStore > kCreateReprod | kStore>kCreateWorker)
    {
      if(kStore>kCreateReprod & birthReprodSoft(colony,Day,nDayCycle))
      {kStore <- kStore - kCreateReprod;  nReprod <- nReprod + 1;}
      
      if(kStore>kCreateWorker & !birthReprodSoft(colony,Day,nDayCycle))
      {kStore <- kStore - kCreateWorker;  nWorker <- nWorker + 1;}
    }
  }
  return(nReprod)
}

#Runs simlation nRun times with given parameters and 
#averages number of reproductives at end of season for nRun colonies
runSimSoftRep <- function(nRun,environment,colony)
{
  x <- 0;
  for(i in 1:nRun)
  {
    x[i] <- runSimSoft(environment,colony);
    if(i%%10==0){print(i);}
  }
  x <<-x;
  return(mean(x))
}

#Find the most successful colony for a given environment
runSimSoftAllEnv <- function(environment,nRun=5,nEach=3)
{
  colony <- expand.grid(early=seq(0.3,.7,length.out = nEach),
                        middle=seq(0.3,.7,length.out = nEach),
                        late=seq(0.3,.7,length.out = nEach),
                        scale=seq(0.3,.7,length.out = nEach));
  colony["nReprod"]<-NA
  for(i in 1:nrow(colony))
  {
    if(i%%10==0){print(i);}
    colony[i,"nReprod"] <- runSimSoftRep(nRun,environment,as.numeric(colony[i,1:4]))
  }
  return(colony)
}

#Find the most successful colony for a given environment and evolve populations
runSimSoftAllEnvGen <- function(environment,nRun=2,nEach=2,nGen=3)
{
  colony <- expand.grid(early=seq(0.3,.7,length.out = nEach),
                        middle=seq(0.3,.7,length.out = nEach),
                        late=seq(0.3,.7,length.out = nEach),
                        scale=seq(0.3,.7,length.out = nEach));
  colony["nReprod"]<-NA
  #First Generation
  for(i in 1:nrow(colony))
  {
    if(i%%10==0){print(i);}
    colony[i,"nReprod"] <- runSimSoftRep(nRun,environment,as.numeric(colony[i,1:4]))
  }
  
  #Next generations
  for(i in 2:nGen)
  {
    iBest <- colony[head(rev(order(colony$nReprod)),nrow(colony)/nEach),]
    for(i in 1:nrow(iBest))
    {iBestEvol <- iBest[i,]*rnorm(4,1,.1)}
    colony[head(order(colony$nReprod),nrow(colony)/nEach),] <- iBestEvol
    for(i in 1:nrow(colony))
    {
      if(i%%10==0){print(i);}
      colony[i,"nReprod"] <- runSimSoftRep(nRun,environment,as.numeric(colony[i,1:4]))
    }
    
  }
  return(colony)
}

#Find the best environment for a given colony
runSimSoftAllCol <- function(nRun,nEach,colony)
{
  environment <- expand.grid(early=seq(0.3,.7,length.out = nEach),
                        middle=seq(0.3,.7,length.out = nEach),
                        late=seq(0.3,.7,length.out = nEach),
                        scale=seq(0.3,.7,length.out = nEach));
  environment["nReprod"]<-NA
  for(i in 1:nrow(environment))
  {
    if(i%%10==0){print(i);}
    environment[i,"nReprod"] <- runSimSoftRep(nRun,as.numeric(environment[i,1:4]),colony)
  }
  return(environment)
}


