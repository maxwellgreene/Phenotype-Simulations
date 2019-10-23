#Whether to birth a reproductive or a worker
#TRUE for reproductive, FALSE for worker
#Specify you want a numerical probability, default is TRUE/FALSE for reproductive
birthReprodHard <- function(Day,nDayCycle,type="logical")
{
  a <- .7;  #Middle season reproductive
  b <- .3;  #Early season reproductive
  c <- .7;  #Late season reproductive
  ratio <- .6;
  
  #Set main variable as the progress through season
  x <- (Day/nDayCycle);
  
  #Make a function of x, which will be the probability of producing a reproductive
  y <- function(x)
  {
    x1 <- (1-4*(x-.5)^2); #Middle of season
    x2 <- (x-1)^2;        #Early season
    x3 <- x^2;            #Late season
    return(a*x1+b*x2+c*x3)
  }
  
  #Normalize/scale the y function so that it's always in [0,1]
  ynorm <- ratio*y(x)/optimize(y,interval=c(0,1),maximum = TRUE)$objective;
  
  prob <- runif(length(Day),0,1) < ynorm;
  if(type=="logical")
  {return(prob);}
  else{return(ynorm)}
}

#Set foraging mortality rate given day in cycle
fMortRateHard <- function(Day,nDayCycle)
{
  #temp <- .4+2*(Day/nDayCycle-.5)^2;
  #return(temp);
  
  a <- .4;   #Middle season MR
  b <- .75;  #Early season MR
  c <- .55;  #Late season MR
  scale <- 0.850;
  
  #Set main variable as the progress through season
  x <- (Day/nDayCycle); 
  
  #Make a function of x, which will be the probability of producing a reproductive
  y <- function(x)
  {
    x1 <- (1-4*(x-.5)^2); #Middle season
    x2 <- (x-1)^2;        #Early season
    x3 <- x^2;            #Late season
    return((a*x1+b*x2+c*x3))
  }
  
  #Normalize/scale the y function so that it's always in [0,1]
  ynorm <- scale*y(x)/optimize(y,interval=c(0,1),maximum = TRUE)$objective;
  return(ynorm);
}

#Set parameters for when reproductives stop foraging
isReprodForageHard <- function(nWorker)
{
  return(ifelse(nWorker<2,TRUE,FALSE))
}