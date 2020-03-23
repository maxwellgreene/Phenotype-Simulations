# Title: Sociality Simulation
# Author: Maxwell Greene
# Project: Thesis Project, Magis Honors Program

# This file contains the following functions:
#   runSim - main simulation with manual param input

newrunSim <- function(enviro, colony, params = c(0,100,1,1,5,1,2.5,2,2,5,5.5))
{
  #Set number of Days in a Cycle
  Day <- params[1];       nDayCycle <- params[2];
  #Starting number of reproductives, workers and energy stores
  nReprod <- params[3];   nWorker <- params[4]; kStore <- params[5];
  #Set amount of energy per day that workers and reproductives make
  kWorker <- params[6];   kReprod <- params[7];
  #Set amount of energy needed to create a worker and reproductive
  cWorker <- params[8];   cReprod <- params[9];
  
  
  #Create dataframe to store data.
  data <- data.frame(timestep=1:nDayCycle,
                     mortality=seasonality(env,(1:nDayCycle)/nDayCycle),
                     ratio = seasonality(col,(1:nDayCycle)/nDayCycle),
                     nReprod=c(nReprod,rep(NA,nDayCycle-1)),
                     nWorker=c(nWorker,rep(NA,nDayCycle-1)),
                     kStore =c(kStore ,rep(NA,nDayCycle-1)))
  
  for(i in 2:nDayCycle)
  {
    data$nWorker[i] <- data$nWorker[i-1] * data$mortality[i-1]
    data$kStore[i]  <- data$nWorker[i]*kWorker# + data$kStore[i-1]
    
    data$nWorker[i] <- data$nWorker[i]   + data$kStore[i]*(1-data$ratio[i])/cWorker
    data$nReprod[i] <- data$nReprod[i-1] + data$kStore[i]*data$ratio[i]/cReprod
  }
  
  return(data)
}

seasonality <- function(x, Days)
{
  return(x[4]*(x[1]*((Days-1)^2)+x[2]*(1-4*(Days-.5)^2)+x[3]*(Days^2)))
}








newVisCol <- function(data,env,col)
{
  ggplot(data,aes(x=timestep)) + 
    geom_line(aes(y=kStore,color="Energy Store")) + 
    geom_line(aes(y=nReprod,color="Reproductives")) + 
    geom_line(aes(y=nWorker,color="Workers")) + 
    geom_line(aes(y=max(c(nWorker,nReprod))*fMortRate(env,timestep,nDayCycle,type="numeric"),color="Survival Rate"),linetype="dashed") +
    geom_line(aes(y=max(c(nWorker,nReprod))*birthReprod(col,timestep,nDayCycle,type="numeric"),color="Reprod:Worker")) +
    scale_colour_manual("Legend",breaks = c("Energy Store", "Reproductives", "Workers","Survival Rate","Reprod:Worker"),
                        values = c("Energy Store"="green","Reproductives"="red","Workers"="blue","Survival Rate"="black","Reprod:Worker"="black")) +
    #ggtitle(paste("Environment: (",toString(enviro),") \nColony:          (",toString(colony),")",sep="")) + 
    labs(x="Timestep (Day)",y="Quantity")
}









