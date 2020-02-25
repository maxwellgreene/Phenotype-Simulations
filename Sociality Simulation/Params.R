################################################################
###=====================  birthReprod  ======================###
################################################################

birthReprod <- function(colony,Day,nDayCycle,type = "logical")
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
  if(type=="numeric"){return(ynorm)}
  if(type=="logical"){return(runif(length(Day),0,1) < ynorm)}
}

################################################################
###======================  fMortRate  =======================###
################################################################

fMortRate <- function(enviro,Day,nDayCycle,type = "numeric")
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
  if(type=="numeric"){return(ynorm)}
  if(type=="logical"){return(runif(length(Day),0,1) < ynorm)}
}


################################################################
###====================  isReprodForage  ====================###
################################################################

#Set parameters for when reproductives stop foraging
isReprodForage <- function(nWorker,type="l2")
{
  if(type=="l2")
  {
    return(ifelse(nWorker<1,TRUE,FALSE))
  }
    
}