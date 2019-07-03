birthReprodSoft <- function(colony,Day,nDayCycle)
{
  #Set main variable as the progress through season
  x <- (Day/nDayCycle);
  #Make a function of x, which will be the probability of producing a reproductive
  y <- function(x)
  {
    x1 <- (1-4*(x-.5)^2); #Middle of season
    x2 <- (x-1)^2;        #Early season
    x3 <- x^2;            #Late season
    return(colony[2]*x1+colony[1]*x2+colony[3]*x3)
  }
  #Normalize/scale the y function so that it's always in [0,1]
  ynorm <- colony[4]*y(x)/optimize(y,interval=c(0,1),maximum = TRUE)$objective;
  
  return(runif(length(Day),0,1) < ynorm)
}

fMortRateSoft <- function(enviro,Day,nDayCycle)
{
  #Set main variable as the progress through season
  x <- (Day/nDayCycle); 
  
  #Make a function of x, which will be the probability of producing a reproductive
  y <- function(x)
  {
    x1 <- (1-4*(x-.5)^2); #Middle season
    x2 <- (x-1)^2;        #Early season
    x3 <- x^2;            #Late season
    return(enviro[2]*x1+enviro[1]*x2+enviro[3]*x3)
  }
  
  #Normalize/scale the y function so that it's always in [0,1]
  ynorm <- enviro[4]*y(x)/optimize(y,interval=c(0,1),maximum = TRUE)$objective;
  return(ynorm);
}

#Set parameters for when reproductives stop foraging
isReprodForageSoft <- function(nWorker)
{
  return(ifelse(nWorker<2,TRUE,FALSE))
}