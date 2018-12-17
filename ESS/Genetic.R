#Maxwell Greene
#Genetic Evolution

geneticSpecies <- function(fitfunc,species,gens,indivs,creation = "new", plot = "maxVals")
{
  #========== CREATE DATAFRAME ==========
  specData <- data.frame(speciesNum=0,timestep=1,avgFit=0,maxFit=0,minFit=0,var1=0,var2=0)
  
  #========== MAIN LOOP ==========
  for(i in 1:species)
  {
    #========== GENERATION DATA ==========
    specData <- rbind(specData,
                      dplyr::mutate(geneticGen(fitfunc = fitfunc,num = indivs,gens = gens,creation = creation,return=T),
                             speciesNum = i))
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
  X <- seq(-3,3,0.1); Y <- seq(-3,3,0.1); grid <- expand.grid(X,Y);
  grid <- mutate(as.data.frame(grid), z = fitfunc(grid$Var1,grid$Var2))
  
  ggplot(specData) + 
    geom_point(data = grid, mapping = aes(x = grid$Var1,y = grid$Var2,color = z), alpha = .6) +
    #geom_point(mapping = aes(x = ,y = Y, color = as.character(speciesNum))) + 
    geom_contour(data = grid, mapping = aes(grid$Var1,grid$Var2, z = grid$z),bins = 20)
  }
}










geneticGen <- function(fitfunc,num,gens,creation = "new",return = F)
{
  #========== CREATE AGENTS ==========
  agents <- data.frame(var1 = rnorm(num,0,1),
                       var2 = rnorm(num,0,1))
  agents<- mutate(agents, fitness = fitfunc(var1,var2))
  
  #========== CREATE DATA ==========
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
      newAgent <- data.frame(height = rnorm(1,0,1),
                         weight = rnorm(1,0,1))
      newAgent <- mutate(newAgent, fitness = fitfunc(newAgent$height,newAgent$weight))
    }else if(creation == "child")
    {
      parent <- sample(1:NROW(agents),1)
      newAgent <- data.frame(height = rnorm(1,1,.03)*agents$height[parent],
                         weight = rnorm(1,1,.03)*agents$weight[parent],
                         fitness = agents$fitness[parent])
      newAgent <- mutate(newAgent,fitness = fitfunc(newAgent$height,newAgent$weight))
    }
    
    #========== DETERMINE REPLACEMENT ==========
    replace <- which.min(agents$fitness)#sample(1:NROW(agents),1)
    
    #========== CHECK FITNESS ==========
    if(newAgent$fitness[1]>agents$fitness[replace])
    {
      agents[replace,] <- newAgent[1,]
    }
    
    #========== ADD TIMESTEP TO "agentsDATA" ==========
    agentsData <- rbind(agentsData,data.frame(timestep = i, 
                                              avgFit = mean(agents$fitness),
                                              maxFit = agents$fitness[which.max(agents$fitness)],
                                              minFit = agents$fitness[which.min(agents$fitness)]))
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