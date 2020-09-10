# Title: Sociality Simulation
# Author: Maxwell Greene
# Project: Thesis Project, Magis Honors Program

newRunSim <- function(enviro, colony, params = c(0,100,1,0,0,2,2.5,5,5.5),ftype="quad")
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
                     mortal = seasonality(x = enviro, Days = (1:nDayCycle)/nDayCycle, type = ftype),
                     bRatio = seasonality(x = colony, Days = (1:nDayCycle)/nDayCycle, type = ftype),
                     nReprod= c(nReprod,rep(NA,nDayCycle-1)),
                     nWorker= c(nWorker,rep(NA,nDayCycle-1)),
                     kStore = c(kStore ,rep(NA,nDayCycle-1)))
  
  for(i in 2:nDayCycle)
  {
    #Kill off workers
    data$nWorker[i] <- data$nWorker[i-1] * (1-data$mortal[i-1])
    #Add energy from workers
    data$kStore[i]  <- data$nWorker[i] * rnorm(1,kWorker,.5) + data$kStore[i-1]
    
    #Add energy from reproductives
    if(data$nWorker[i-1] < 2)#isReprodForage(nWorker = data$nWorker[i-1]))
    {data$kStore[i]  <- data$nReprod[i-1] * rnorm(1,kReprod,.5)}
    
    #Create Workers
    data$nWorker[i] <- data$nWorker[i] + data$kStore[i]*(1-data$bRatio[i])/cWorker
    #Create Reproductives
    data$nReprod[i] <- data$nReprod[i-1]*.995 + data$kStore[i]*data$bRatio[i]/cReprod
  }
  return(data)
}

################################################################
###====================  newRunSimRep  ======================###
################################################################

#Runs simlation nRun times with given parameters and 
#averages number of reproductives at end of season for nRun colonies
newRunSimRep <- function(nRun,env,col,ftype="quad", params = c(0,100,1,0,0,2,2.5,5,5.5), PB = TRUE)
{
  x <- rep(NA,nRun);
  if(PB){pb = txtProgressBar(min = 0, max = nRun, initial = 0);}
  for(i in 1:nRun)
  {
    if(PB){setTxtProgressBar(pb,i);}
    result <- newRunSim(enviro=env,colony=col,ftype = ftype,params = params);
    x[i] <- tail(result$nReprod,n=1)
  }
  return(x)
  close(pb)
}

timerun <- function()
{ ptm <- proc.time();  runSimRepMan(1000,env,col);  print(proc.time()-ptm)}

timenewrun <- function()
{ ptm <- proc.time();  newRunSimRep(1000,env,col);  print(proc.time()-ptm)}

################################################################
###=====================  newRunSimEnv  =====================###
################################################################

#Find the most successful colony for a given environment
newRunSimEnv <- function(env,type,nRun=5,nEach=3, params = c(0,100,1,0,0,2,2.5,5,5.5))
{
  colony <- makepop(nEach=nEach,type=type)
  PB = txtProgressBar(min = 0, max = nrow(colony), initial = 0);
  colony["nReprod"]<-NA
  
  for(i in 1:nrow(colony))
  {
    tempcol <- as.numeric(colony[i,1:length(env)])
    colony[i,"nReprod"] <- mean(newRunSimRep(nRun = nRun,env = env,
                                        col = tempcol, params = params, PB = FALSE))
    setTxtProgressBar(PB,i);
  }
  return(colony)
}

################################################################
###=====================  newRunSimCol  =====================###
################################################################

#Find the most successful colony for a given environment
newRunSimCol <- function(col,type,nRun=5,nEach=3, params = c(0,100,1,0,0,2,2.5,5,5.5))
{
  colony <- makepop(nEach=nEach,type=type)
  PB = txtProgressBar(min = 0, max = nrow(colony), initial = 0);
  colony["nReprod"]<-NA
  
  for(i in 1:nrow(colony))
  {
    tempenv <- as.numeric(colony[i,1:length(col)])
    colony[i,"nReprod"] <- mean(newRunSimRep(nRun = nRun,env = tempenv,
                                             col = col, params = params, PB = FALSE))
    setTxtProgressBar(PB,i);
  }
  return(colony)
}

################################################################
###===================  newRunSimEnvGen  ====================###
################################################################

#Find the most successful colony for a given environment and evolve populations
newRunSimEnvGen <- function(env,type,nRun=1,nEach=2,nGen=3,range = c(0.2,0.8))
{
  #CC <- 50
  pop <- makepop(nEach,type)
  if(type=="quad"){typelen<-4};
  if(type=="beta"){typelen<-2};
  if(type=="simple"){typelen<-1}
  
  #Initialize nReprod to NA for all individuals
  pop["nReprod"]<-NA
  #Initialize identities for all individuals
  pop["Identity"]<-1:nrow(pop);idCount<-nrow(pop);
  
  onegenPB = txtProgressBar(min = 0, max = nrow(pop), initial = 0)
  
  #First Generation
  print("Performing INITIAL generation")
  
  #Assess Fitness
  for(i in 1:nrow(pop))
  {
    setTxtProgressBar(onegenPB,i)
    pop[i,"nReprod"] <- mean(newRunSimRep(nRun=nRun,env=env,col=as.numeric(pop[i,1:typelen]),
                                     ftype=ftype))
  }
  close(onegenPB)
  
  #print(pop)
  
  #Next generations
  print("Performing SUBSEQUENT generations")
  subgenPB = txtProgressBar(min = 0, max = nrow(pop)*nGen/2, initial = 0)
  
  for(i in 1:nGen)
  {
    #Identify the best half of individuals
    nBestEvol <- pop[head(rev(order(pop$nReprod)),nrow(pop)/2),]
    
    #Assess fitness of nBestEvol before attaching to colony
    for(k in 1:nrow(nBestEvol))
    {
      setTxtProgressBar(subgenPB,(i-1)*nrow(pop)/2+k)
      
      #Evolve these inviduals
      nBestEvol[k,] <- nBestEvol[k,]*c(rnorm(4,1,.1),1,NA)
      #Assign them a new identity
      nBestEvol[k,"Identity"]<-idCount;idCount<-idCount+1;
      
     
      tempcol <- as.numeric(nBestEvol[k,1:4])
      nBestEvol[k,"nReprod"] <- mean(newRunSimRep(nRun = nRun,env=env,col=tempcol,
                                          ftype=ftype))
    }
    
    #Replace the worst half with evolved best half
    #pop[head(order(pop$nReprod),nrow(pop)/2),] <- nBestEvol
    pop <- rbind(pop,nBestEvol)
  }
  
  close(subgenPB)
  popOrdered <- pop[order(pop$nReprod,decreasing=TRUE),]
  return(popOrdered)
}

################################################################
###==================  newRunSimEnvGenLin  ==================###
################################################################
# Find the most successful colony for a given environment, evolve populations
# and track lineages
newRunSimEnvGenLin <- function(env,nRun=2,nEach=2,nGen=3,ftype="quad")
{
  colony <- makepop(nEach,ftype)
  if(ftype=="quad"){typelen<-4};
  if(ftype=="beta"){typelen<-2};
  if(ftype=="simple"){typelen<-1}
  
  #Initialize nReprod to NA for all individuals
  colony["nReprod"]<-NA
  #Initialize identities for all individuals
  colony["child"]<-1:nrow(colony)
  idCount<-nrow(colony)
  #Initialize lineages for all individuals
  colony["parent"]<-0
  #Initialize generation number for all individuals
  colony["generation"]<-rep(0,length(colony[,1]))
  
  onegenPB = txtProgressBar(min = 0, max = nrow(colony), initial = 0)
  
  #First Generation
  print("Performing INITIAL generation")
  
  #Assess Fitness
  for(i in 1:nrow(colony))
  {
    setTxtProgressBar(onegenPB,i)
    colony[i,"nReprod"] <- mean(newRunSimRep(nRun,env,as.numeric(colony[i,1:4]),ftype = ftype))
  }
  close(onegenPB)
  
  #Next generations
  print("Performing SUBSEQUENT generations")
  subgenPB = txtProgressBar(min = 0, max = nGen, initial = 0)
  
  fullCol <- colony
  
  for(i in 1:nGen)
  {
    setTxtProgressBar(subgenPB,i)
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
      
      nBestEvol[k,"nReprod"] <- mean(newRunSimRep(nRun,env,as.numeric(nBestEvol[k,1:4]),ftype = ftype))
    }
    
    #Replace the worst half with evolved best half
    colony[head(order(colony$nReprod),nrow(colony)/2),] <- nBestEvol
    fullCol <- rbind(fullCol,nBestEvol)
    
    #Update lineages on survived individuals
    #for(l in 1:nrow(colony))
    #{colony$lineage[l] <-list(append(unlist(colony[l,"lineage"]),colony[l,"identity"]))}
  }
  
  close(subgenPB)
  #colonyOrdered<-colony[order(colony$nReprod,decreasing=TRUE),]
  #return(colonyOrdered)
  fullColOrdered <- fullCol[order(fullCol$nReprod,decreasing=TRUE),]
  return(fullCol)
}

################################################################
###=====================  Parameters  =======================###
################################################################

seasonality <- function(x, Days, type)
{
  if(type == "quad")
  {
    if(length(x) == 4)
    {
      return(x[4]*(x[1]*((Days-1)^2)+x[2]*(1-4*(Days-.5)^2)+x[3]*(Days^2)))
    }
    else{stop("Quadratic function requires 4 arguments.")}
  }
  
  if(type == "beta")
  {
    if(length(x) == 2)
    {
      tempbeta <- dbeta(Days,x[1],x[2])
      tempbeta[which(tempbeta == Inf)] <- rep(max(tempbeta[which(tempbeta < Inf)]),length(which(tempbeta == Inf)))
      return(tempbeta/max(tempbeta[which(tempbeta < Inf)]))
    }
    else{stop("Beta function requires 2 arguments.")}
  }
  
  if(type == "simple")
  {
    if(length(x) == 1)
    {
      return(rep(x,length(Days)))
    }
    else{stop("Constant function requires 1 argument.")}
  }
}

makepop <- function(nEach,type)
{
  if(type == "quad")
  {
    pop <- expand.grid(early = seq(0.7,0.9,length.out = nEach),
                       middle= seq(0.7,0.9,length.out = nEach),
                       late  = seq(0.7,0.9,length.out = nEach),
                       scale = seq(0.8,0.8,length.out = 1))
  }
  
  if(type == "beta")
  {
    pop <- expand.grid(first = seq(0.5,2.0,length.out = nEach),
                       second= seq(0.5,2.0,length.out = nEach))
  }
  
  if(type == "simple")
  {
    pop <- expand.grid(first = seq(0.0,1.0,length.out = nEach))
  }
  
  return(pop)
}


################################################################
###====================  Visualization  =====================###
################################################################
newVisCol <- function(data,env,col,type="quad")
{
  ggplot(data,aes(x=timestep)) + 
    geom_line(aes(y=kStore,color="Energy Store")) + 
    geom_line(aes(y=nReprod,color="Reproductives")) + 
    geom_line(aes(y=nWorker,color="Workers")) + 
    geom_line(aes(y=max(c(nWorker,nReprod))*seasonality(env,(1:100)/100,type=type),color="Survival Rate"),linetype="dashed") +
    geom_line(aes(y=max(c(nWorker,nReprod))*seasonality(col,(1:100)/100,type=type),color="Reprod:Worker")) +
    scale_colour_manual("Legend",breaks = c("Energy Store", "Reproductives", "Workers","Survival Rate","Reprod:Worker"),
                        values = c("Energy Store"="green","Reproductives"="red","Workers"="blue","Survival Rate"="black","Reprod:Worker"="black")) +
    #ggtitle(paste("Environment: (",toString(enviro),") \nColony:          (",toString(colony),")",sep="")) + 
    labs(x="Timestep (Day)",y="Quantity")
}

newVisDensity <- function(nReprods)
{
  plot1 <- ggplot(data.frame(nReprods), aes(x = nReprods)) + geom_density(color="darkblue", fill="lightblue") + 
    ggtitle("Density of nReprods")
  
  plot2 <- ggplot(data.frame(nReprods), aes(x = log(nReprods))) + geom_density(color="darkblue", fill="lightblue") + 
    ggtitle("Density of LOG(nReprods)")
  
  return(list(plot1,plot2))
}

################################################################
###=================  newVisPopParamsPlotly  ================###
################################################################

# Plot the results of a single-generation or evolved population
# in an interactive plotly plot
newVisPopParamsPlotly <- function(population)
{
  plot_ly(x=population$early, y=population$middle, z=population$late, 
          type="scatter3d", mode="markers", color=log(population$nReprod)) %>% 
    layout(title = "Number of Reproductives as a Function of Colony",
           scene = list(
             xaxis = list(title = "Early Season"),
             yaxis = list(title = "Middle Season"),
             zaxis = list(title = "Late Season")))
}

################################################################
###======================  ggConvert  =======================###
################################################################

#converts population into standard ggenealogy data.frame
ggConvert <- function(popDF,varname)
{
  #colnames(popDF) <- c("early","middle","late","scale","nReprod","child","parent","generation")
  popDF <- popDF[c("child","parent",varname)]
  popDF$child <- as.character(popDF$child)
  popDF$parent <- as.character(popDF$parent)
  tempdf <- data.frame(child="0",parent="NA",tempname=0)
  names(tempdf)[names(tempdf) == "tempname"] <- varname
  popDF <- rbind(popDF,tempdf)
  return(popDF)
}

################################################################
###======================  VisLinPath  ======================###
################################################################

#Plot a visualpath between individuals in a population
newVisLinPath <- function(popDF,to,from,x,y)
{
  colnames(popDF) <- c("early","middle","late","scale","nReprod","child","parent","generation")
  popDF <- popDF[c("child","parent",x,y)]
  popDF$child <- as.character(popDF$child);
  popDF$parent <- as.character(popDF$parent);
  tempdf <- data.frame(child="0",parent="NA",tempname=-1,tempname2=-1)
  names(tempdf)[names(tempdf) == "tempname"] <- x
  names(tempdf)[names(tempdf) == "tempname2"] <- y
  
  popDF <- rbind(popDF,tempdf)
  
  popIG <- dfToIG(popDF)
  path <- getPath(to,from,popIG,popDF,x)
  #plotPathOnAll(path,popDF,popIG,x,pathEdgeCol = "orange") + 
  #  ggplot2::xlab(x) + ggplot2::ylab(y)
  plot1 <- plotPathOnAll(path,popDF,popIG,x)
  plot2 <- plotPath(path,popDF,x)
  return(list(plot1,plot2))
}

################################################################
###======================  VisAncTree  ======================###
################################################################

#Plot a n ancestral tree of all individuals in a population
newVisAncTree<- function(popDF,indiv,mAnc=0,mDes=20)
{
  popDF <- ggConvert(popDF,"nReprod")
  plotAncDes(indiv, popDF, mAnc = mAnc, mDes = mDes)
}





