runSimHard <- function()
{
  #Set number of Days in a Cycle
  Day <- 0; nDayCycle <- 100;
  #Starting number of reproductives, workers and energy stores
  nReprod <- 1; nWorker <- 0; kStore <- 0;
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
    
    nWorker <- ceiling(nWorker*fMortRateHard(Day,nDayCycle));
    kStore <- kStore + nWorker * nTripWorker;
    
    #Are the reproductives foraging? 
    if(isReprodForageHard(nWorker=nWorker))
    {
      nReprod <- ceiling(nReprod*fMortRateHard(Day,nDayCycle));
      kStore <- kStore + nReprod * nTripReprod;
    }
    
    while (kStore > kCreateReprod | kStore>kCreateWorker)
    {
      if(kStore>kCreateReprod & birthReprodHard(Day,nDayCycle))
      {kStore <- kStore - kCreateReprod;  nReprod <- nReprod + 1;}
      
      if(kStore>kCreateWorker & !birthReprodHard(Day,nDayCycle))
      {kStore <- kStore - kCreateWorker;  nWorker <- nWorker + 1;}
    }
    
    if(i%%5==0){print(paste("Day: ",i,"  kStore: ",kStore,"  nReprod: ",nReprod,"  nWorker: ",nWorker));}
    data <- rbind(data,list(i,nReprod,nWorker,kStore))
  }
  
  ggplot(data,aes(x=timestep)) + 
    geom_line(aes(y=kStore,color="kStore")) + 
    geom_line(aes(y=nReprod,color="nReprod")) + 
    geom_line(aes(y=nWorker,color="nWorker")) + 
    geom_line(aes(y=max(c(nWorker,nReprod))*fMortRateHard(timestep,nDayCycle),color="fMortRate"),linetype="dashed") +
    geom_line(aes(y=max(c(nWorker,nReprod))*birthReprodHard(timestep,nDayCycle,type="numeric"),color="birthReprod")) +
    scale_colour_manual("",breaks = c("kStore", "nReprod", "nWorker","fMortRate","birthReprod"),
                        values = c("kStore"="green","nReprod"="red","nWorker"="blue","fMortRate"="black","birthReprod"="black"))
}

################################
################################
################################
