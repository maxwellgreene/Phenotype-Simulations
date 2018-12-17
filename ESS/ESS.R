#Maxwell Greene
#Evolutionary Stable Strategy Simulation

makeAgents <- function(.num)
{
  agents <<- data.frame(
      agent = 1:.num,
      opponent = sample(1:.num,.num,replace=F),
      hunt = runif(.num),
      random = runif(.num,0,1),
      hasfish = rep(0,.num))
  
  agents <<- mutate(agents, steal = 1-hunt)
  return(agents)
}

doAgents <- function(data,gens)
{
  for (i in gens) 
  {
    data$hasfish <- ifelse(data$hunt > data$random,data$hasfish+1,0)
    data$hasfish <- ifelse(
      data$steal[data$agent] > data$steal[data$opponent] & 
      data$hasfish[data$opponent] ,data$hasfish+1,0)
    
    data$opponent <- sample(1:nrow(data),nrow(data),replace=F)
    data$random <- runif(nrow(data),0,1)
  }
  agents <<- data
}

doAgain <- function()
{
  makeAgents(2)
  doAgents(agents,seq(1,2))
  head(agents)
}




