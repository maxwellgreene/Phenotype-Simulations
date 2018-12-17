#Maxwell Greene
#Colony-level evolution simulation with multiple time-step fitness consideration
createATFM <- function(cols = 5, groups = 3, tasks = 3)
{  
  normRowMat <- function(unused)
  {
    data <- matrix(NA,nrow=groups,ncol=tasks)
    for (i in 1:groups)
    {temp <- runif(tasks);      data[i,] <- temp/sum(temp)}
    return(data)
  }
  colonies <- list(0);length(colonies)<-cols
  colonies <- lapply(colonies,normRowMat)
  populations <- sample(1:groups,groups)
  combined <- list("cols" = colonies, "pops" = populations)
  return(combined)
}

fitnessEval <- function(colonies, matrix, fitness, time = 10)
{
  for(i in 1:length(colonies))
  {
    for(j in 1:time)
    {
      #do something to mutate, change by timestep?
      colonies[i] <- colonies[i] * (matrix)
    }
  }
  return(fitness(colony))
}

fitfunc <- function(colonies)
{
  fitness <- rep(NA,length(colonies))
  for(i in 1:length(colonies))
  {
    fitness[i] <- sum(colonies[[i]])
  }
  return(fitness)
}









