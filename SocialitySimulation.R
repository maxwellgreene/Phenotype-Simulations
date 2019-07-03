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
runSimSoftAllEnv <- function(nRun,nEach,environment)
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


