#Maxwell Greene
#Genetic Evolution


geneticPopulation <- function(fitfunc,species,gens,indivs,creation = "new", plot = "maxVals")
{
  #========== CREATE DATAFRAME ==========
  specData <- data.frame(speciesNum=0,timestep=1,avgFit=0,maxFit=0,minFit=0,var1=0,var2=0)
  
  #========== MAIN LOOP ==========
  for(i in 1:species)
  {
    #========== GENERATION DATA ==========
    temp <- geneticSpecies(fitfunc = fitfunc,indivs = indivs,gens = gens,creation = creation,return=T)
    specData <- rbind(specData,
                      dplyr::mutate(speciesNum = rep(i,nrow(temp)),temp))
  }
  specData <<- specData
  
  #========== PLOT avgFIT BY TIME ==========
  if(plot == "time")
  {
    ggplot(specData) + 
    geom_line(mapping = aes(
      x = timestep, y = avgFit,
      color = as.character(speciesNum))) + 
      theme(legend.position = "none")
  }else 
  #========== PLOT maxFIT VALUES ==========
  if(plot == "maxVals")
  {
    X <- seq(-4,4,0.1); Y <- seq(-4,4,0.1);
    
    ggplot(specData) + 
      geom_point(mapping = aes(x = var1,y = var2, color = as.character(speciesNum))) + 
      #geom_contour(data=grid,mapping = aes(x=Var1,y=Var2,z=z),bins = 20)
      geom_contour(data=expand.grid(X,Y),
                   mapping = aes(x=Var1,y=Var2,z=fitfunc(Var1,Var2)),bins = 20)
  }
}



geneticSpecies <- function(fitfunc,indivs,gens,creation = "new",return = F)
{
  #========== CREATE AGENTS ==========
    #Assign each agent a value for var1 and var2 according to rnorm
    agents <- data.frame(var1 = rnorm(indivs,0,1),
                         var2 = rnorm(indivs,0,1))
    #Assign each agent a score according to fitfunc(var1,var2)
    agents<- mutate(agents, fitness = fitfunc(var1,var2))
  
  #========== CREATE DATA ==========
    #Craete dataframe with descriptive values on var1 and var2 of agents population
    agentsData <- data.frame(timestep = 0,
                             avgFit = mean(agents$fitness),
                             minFit = agents$fitness[which.min(agents$fitness)],
                             maxFit = agents$fitness[which.max(agents$fitness)],
                             var1   = agents$var1,
                             var2   = agents$var2)
  
  #===============================
  #========== MAIN LOOP ==========
  #===============================
    for(i in 1:gens)
    {
      #========== CREATE NEW AGENT ==========
        if(creation == "new")
        {
          #Creates a new agent, exactly as the population was created
          newAgent <- data.frame(var1 = rnorm(1,0,1),
                                 var2 = rnorm(1,0,1))
          newAgent <- mutate(newAgent, fitness = fitfunc(newAgent$var1,newAgent$var2))
        }else if(creation == "child")
        {
          #randomly select a parent from current population
          parent <- sample(1:NROW(agents),1)
          #modify parent by rnorm weight and re-assign fitness
          newAgent <- data.frame(var1 = rnorm(1,1,.03)*agents$var1[parent],
                                 var2 = rnorm(1,1,.03)*agents$var2[parent],
                                 fitness = agents$fitness[parent])
          newAgent <- mutate(newAgent,fitness = fitfunc(newAgent$var1,newAgent$var2))
        }
      
      #========== DETERMINE REPLACEMENT ==========
        replace <- which.min(agents$fitness)#sample(1:NROW(agents),1)
      
      #========== CHECK FITNESS ==========
        if(newAgent$fitness[1]>agents$fitness[replace])
        {
          agents[replace,] <- newAgent[1,]
        }
      
      #========== ADD TIMESTEP TO "agentsData" ==========
        agentsData <- rbind(agentsData,data.frame(timestep = i, 
                                                  avgFit = mean(agents$fitness),
                                                  maxFit = agents$fitness[which.max(agents$fitness)],
                                                  minFit = agents$fitness[which.min(agents$fitness)],
                                                  var1 = agents$var1,
                                                  var2 = agents$var2))
    }
  #==============================
  #========== END LOOP ==========
  #==============================
  
  agents <<- agents; agentsData <<- agentsData
  
  ggplot(agentsData) + 
    geom_line(mapping = aes(x = timestep,y = avgFit)) + 
    geom_line(mapping = aes(x = timestep,y = minFit), color = "red") + 
    geom_line(mapping = aes(x = timestep,y = maxFit), color = "green")
    
  if(return){return(agentsData)}
}