# This file contains the following functions:
#   runSim - main simulation with manual param input
#   runSimRep - repeatedly run runSimSoft and average output
#   runSimAllEnv - repeatedly run runSimSoftRep with variety of colonies 
#                      to test for best colony for given environment
#   runSimAllEnvGen - repeatedly run runSimSoftRep with variety of colonies 
#                      to test for best colony for given environment. Evolve 
#                      the best half for an umber of generations 
#
#   runSimAllCol - repeatedly run runSimSoftRep with variety of environments 
#                      to test for best environment for given colony

################################################################
###========================= TO DO  =========================###
################################################################

# 1. figure out apply function in runSimRep
# 2. find other inefficiencies and add to TODO
# 3. comment 'n' shit
# 4. change names to be more particular

# See Desmos graphs here https://www.desmos.com/calculator/j5nisf3tcp 

################################################################
###========================  runSim  ========================###
################################################################

# runSim:
# params
#   enviro - length 4 vector of quantities with scalars for seasonal
#            mortality rate functions and a scaling number

runSim <- function(enviro,colony,nDayCycle=100,nReprod=1)
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
    nWorker <- ceiling(nWorker * fMortRate(enviro,Day,nDayCycle));
    kStore <- kStore + nWorker * nTripWorker;
    
    #Are the reproductives foraging? 
    if(isReprodForage(nWorker=nWorker))
    {
      nReprod <- ceiling(nReprod*fMortRate(enviro,Day,nDayCycle));
      kStore <- kStore + nReprod * nTripReprod;
    }
    
    while (kStore > kCreateReprod | kStore > kCreateWorker)
    {
      if(kStore>kCreateReprod & birthReprod(colony,Day,nDayCycle))
      {kStore <- kStore - kCreateReprod;  nReprod <- nReprod + 1;}
      
      if(kStore>kCreateWorker & !birthReprod(colony,Day,nDayCycle))
      {kStore <- kStore - kCreateWorker;  nWorker <- nWorker + 1;}
    }
  }
  return(nReprod)
}

################################################################
###======================  runSimMan  =======================###
################################################################

runSimMan <- function(enviro, colony, colParams = c(0,100,1,0,0,2,2.5,2,2,5,5.5))
{
  #Default command:
  #colParams <- c(0,100,1,0,0,2,2.5,2,2,5,5.5)
  
  #Set number of Days in a Cycle
  Day <- colParams[1]; nDayCycle <- colParams[2];
  #Starting number of reproductives, workers and energy stores
  nReprod <- colParams[3]; nWorker <- colParams[4]; kStore <- colParams[5];
  #Number of trips that workers and reproductives can make each day
  nTripReprod <- colParams[6]; nTripWorker <- colParams[7];
  #Set amount of energy per trip that workers and reproductives make
  kTripWorker <- colParams[8]; kTripReprod <- colParams[9];
  #Set amount of energy needed to create a worker and reproductive
  kCreateWorker <- colParams[10]; kCreateReprod <- colParams[11];
  
  #Create dataframe to store data.
  data <- data.frame(timestep=0,
                     nReprod=nReprod,
                     nWorker=nWorker,
                     kStore=kStore)
  
  for (i in 1:nDayCycle)
  {
    Day <- i;
    
    #Forage as much as you can/need with workers
    nWorker <- ceiling(nWorker*fMortRate(enviro,Day,nDayCycle));
    kStore <- kStore + nWorker * nTripWorker;
    
    #Are the reproductives foraging?
    if(isReprodForage(nWorker=nWorker))
    {
      #If so, send them out for some kStore
      nReprod <- ceiling(nReprod * fMortRate(enviro,Day,nDayCycle));
      kStore <- kStore + nReprod * nTripReprod;
    }
    while (kStore > kCreateReprod | kStore>kCreateWorker)
    {
      if(kStore>kCreateReprod & birthReprod(colony,Day,nDayCycle))
      {kStore <- kStore - kCreateReprod;  nReprod <- nReprod + 1;}
      
      if(kStore>kCreateWorker & !birthReprod(colony,Day,nDayCycle))
      {kStore <- kStore - kCreateWorker;  nWorker <- nWorker + 1;}
    }
    
    if(i%%10==0){print(paste("Day: ",i,"  kStore: ",kStore,"  nReprod: ",nReprod,"  nWorker: ",nWorker));}
    data <- rbind(data,list(i,nReprod,nWorker,kStore))
  }
  
  return(data)
}

################################################################
###======================  runSimRep  =======================###
################################################################

#Runs simlation nRun times with given parameters and 
#averages number of reproductives at end of season for nRun colonies
runSimRep <- function(nRun,environment,colony)
{
  #TODO: figure out apply function
  #x<-matrix(1:nRun,nrow=nRun,ncol=1);
  #x<-apply(x,1,runSim,environment,colony);
  
  x <- rep(0,nRun)
  for(i in 1:nRun)
  {
    x[i] <- runSim(environment,colony);
  }
  return(mean(x))
}

################################################################
###====================  runSimRepMan  ======================###
################################################################

#Runs simlation nRun times with given parameters and 
#averages number of reproductives at end of season for nRun colonies
runSimRepMan <- function(nRun,environment,colony)
{
  x <- rep(0,nRun);
  #PB = txtProgressBar(min = 0, max = nRun, initial = 0);
  for(i in 1:nRun)
  {
    #setTxtProgressBar(PB,i);
    x[i] <- runSim(environment,colony);
  }
  return(x)
  close(PB)
}

################################################################
###=====================  runSimAllEnv  =====================###
################################################################

#Find the most successful colony for a given environment
runSimAllEnv <- function(environment,nRun=5,nEach=3)
{
  colony <- expand.grid(early =seq(0.3,.7,length.out = nEach),
                        middle=seq(0.3,.7,length.out = nEach),
                        late  =seq(0.3,.7,length.out = nEach),
                        scale =seq(0.3,.7,length.out = nEach));
  colony["nReprod"]<-NA
  
  for(i in 1:nrow(colony))
  {
    colony[i,"nReprod"] <- runSimRep(nRun,environment,as.numeric(colony[i,1:4]))
  }
  return(colony)
}

################################################################
###===================  runSimAllEnvGen  ====================###
################################################################

#Find the most successful colony for a given environment and evolve populations
runSimAllEnvGen <- function(environment,nRun=2,nEach=2,nGen=3)
{
  colony <- expand.grid(early=seq(0.2,0.8,length.out = nEach),
                        middle=seq(0.2,0.8,length.out = nEach),
                        late=seq(0.2,0.8,length.out = nEach),
                        scale=seq(0.2,0.8,length.out = nEach));
  #Initialize nReprod to NA for all individuals
  colony["nReprod"]<-NA
  #Initialize identities for all individuals
  colony["identity"]<-1:nrow(colony);idCount<-nrow(colony);
  
  onegenPB = txtProgressBar(min = 0, max = nrow(colony), initial = 0)
  
  #First Generation
  print("Performing INITIAL generation")
  
  #Assess Fitness
  for(i in 1:nrow(colony))
  {
    setTxtProgressBar(onegenPB,i)
    colony[i,"nReprod"] <- runSimRep(nRun,environment,as.numeric(colony[i,1:4]))
  }
  close(onegenPB)
  
  #Next generations
  print("Performing SUBSEQUENT generations")
  
  subgenPB = txtProgressBar(min = 0, max = nrow(colony)*(nGen)/2, initial = 0)
  
  for(i in 1:nGen)
  {
    #Identify the best half of individuals
    nBestEvol <- colony[head(rev(order(colony$nReprod)),nrow(colony)/2),]
    
    #Process these individuals
    for(j in 1:nrow(nBestEvol))
    {
      #Evolve these inviduals
      nBestEvol[j,] <- nBestEvol[j,]*c(rnorm(4,1,.1),1,NA)
      #Assign them a new identity
      nBestEvol[j,"identity"]<-idCount;idCount<-idCount+1;
    }
    
    #Assess fitness of nBestEvol before attaching to colony
    for(k in 1:nrow(nBestEvol))
    {
      #Update progress bar for subsequent generations
      setTxtProgressBar(subgenPB,(i-1)*nrow(nBestEvol)+k)
      nBestEvol[k,"nReprod"] <- runSimRep(nRun,environment,as.numeric(nBestEvol[k,1:4]))
    }
    
    #Replace the worst half with evolved best half
    colony[head(order(colony$nReprod),nrow(colony)/2),] <- nBestEvol
  }
  
  close(subgenPB)
  colonyOrdered<-colony[order(colony$nReprod,decreasing=TRUE),]
  return(colonyOrdered)
}

################################################################
###==================  runSimAllEnvGenLin  ==================###
################################################################
# Find the most successful colony for a given environment, evolve populations
# and track lineages
runSimAllEnvGenLin <- function(environment,nRun=2,nEach=2,nGen=3)
{
  colony <- expand.grid(early=seq(0.2,0.8,length.out = nEach),
                        middle=seq(0.2,0.8,length.out = nEach),
                        late=seq(0.2,0.8,length.out = nEach),
                        scale=seq(0.2,0.8,length.out = nEach));
  
  #Initialize nReprod to NA for all individuals
  colony["nReprod"]<-NA
  #Initialize identities for all individuals
  colony["child"]<-1:nrow(colony);idCount<-nrow(colony);
  #Initialize lineages for all individuals
  colony["parent"]<-0;
  #Initialize generation number for all individuals
  colony["generation"]<-rep(0,length(colony[,1]))
  
  onegenPB = txtProgressBar(min = 0, max = nrow(colony), initial = 0)
  
  #First Generation
  print("Performing INITIAL generation")
  
  #Assess Fitness
  for(i in 1:nrow(colony))
  {
    setTxtProgressBar(onegenPB,i)
    colony[i,"nReprod"] <- runSimRep(nRun,environment,as.numeric(colony[i,1:4]))
  }
  close(onegenPB)
  
  #Next generations
  print("Performing SUBSEQUENT generations")
  subgenPB = txtProgressBar(min = 0, max = nrow(colony)*(nGen)/2, initial = 0)
  
  fullCol <- colony
  
  for(i in 1:nGen)
  {
    #Identify the best half of individuals
    nBestEvol <- colony[head(rev(order(colony$nReprod)),nrow(colony)/2),]
    
    #Process these individuals
    for(j in 1:nrow(nBestEvol))
    {
      #Update lineage to these individuals
        #nBestEvol$lineage[j] <-list(append(unlist(nBestEvol[j,"parent"]),nBestEvol[j,"child"]))
      
      #Add generation number to new individuals
      nBestEvol$generation[j] <- i
      #Add parent under "lineage" to individuals
      nBestEvol$parent[j] <- nBestEvol[j,"child"]
      #Assign them a new identity
      idCount <- idCount+1
      nBestEvol$child[j]<-idCount
      #Evolve these inviduals
      nBestEvol[j,1:4] <- nBestEvol[j,1:4]*c(rnorm(4,1,.1))
    }
    
    #Assess fitness of nBestEvol before attaching to colony
    for(k in 1:nrow(nBestEvol))
    {
      #Update progress bar for subsequent generations
      setTxtProgressBar(subgenPB,(i-1)*nrow(nBestEvol)+k)
      nBestEvol[k,"nReprod"] <- runSimRep(nRun,environment,as.numeric(nBestEvol[k,1:4]))
    }
    
    #Replace the worst half with evolved best half
    colony[head(order(colony$nReprod),nrow(colony)/2),] <- nBestEvol
    fullCol <- rbind(fullCol,nBestEvol)
    
    #Update lineages on survived individuals
      #for(l in 1:nrow(colony))
      #{colony$lineage[l] <-list(append(unlist(colony[l,"lineage"]),colony[l,"identity"]))}
  }
  
  close(subgenPB)
  colonyOrdered<-colony[order(colony$nReprod,decreasing=TRUE),]
  #return(colonyOrdered)
  return(fullCol)
}

################################################################
###=====================  runSimAllCol  =====================###
################################################################

#Find the best environment for a given colony
runSimAllCol <- function(nRun,nEach,colony)
{
  environment <- expand.grid(early=seq(0.3,.7,length.out = nEach),
                             middle=seq(0.3,.7,length.out = nEach),
                             late=seq(0.3,.7,length.out = nEach),
                             scale=seq(0.8,.9,length.out = nEach));
  environment["nReprod"]<-NA
  for(i in 1:nrow(environment))
  {
    environment[i,"nReprod"] <- runSimSoftRep(nRun,as.numeric(environment[i,1:4]),colony)
  }
  return(environment)
}

################################################################
###=====================  runSimAllDen  =====================###
################################################################
#Create a dataset with density functions for all combinations of data
runSimAllDen <- function(nRun,nEach,range,env=c(.6,.7,.8,.75),col=c(.5,.5,.75,.75), which = rep(1,8), return = "vector")
{
  envB <- env - range; envA <- env + range;
  colB <- col - range; colA <- col + range;
    
  data <- expand.grid(envEarly  = seq(envA[1],envB[1],length.out = ifelse(which[1],nEach,1)),
                      envMiddle = seq(envA[2],envB[2],length.out = ifelse(which[2],nEach,1)),
                      envLate   = seq(envA[3],envB[3],length.out = ifelse(which[3],nEach,1)),
                      envScale  = seq(envA[4],envB[4],length.out = ifelse(which[4],nEach,1)),
                      colEarly  = seq(colA[1],colB[1],length.out = ifelse(which[5],nEach,1)),
                      colMiddle = seq(colA[2],colB[2],length.out = ifelse(which[6],nEach,1)),
                      colLate   = seq(colA[3],colB[3],length.out = ifelse(which[7],nEach,1)),
                      colScale  = seq(colA[4],colB[4],length.out = ifelse(which[8],nEach,1)));

  results <- list()
  
  progress = txtProgressBar(min = 0, max = nrow(data), initial = 0)
  
  for(i in 1:nrow(data))
  {
    setTxtProgressBar(progress,i)
    result <- runSimRepMan(nRun,as.numeric(data[i,1:4]),as.numeric(data[i,5:8]))
    if(return == "vector")  { results[[i]] <- result }
    if(return == "density") { results[[i]] <- density(result) }
  }
  close(progress)
  
  return(list(data,results))
}









