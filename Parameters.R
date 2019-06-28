#Whether to birth a reproductive or a worker
#TRUE for reproductive, FALSE for worker
birthReprod <- function(nReprod,nWorker,Day,nDayCycle)
{
  temp <- 1-(Day/nDayCycle);
  temp2 <- runif(1,0,1) < .15*temp
  return(temp2);
}

#Set foraging mortality rate given day in cycle
fMortRate <- function(Day,nDayCycle)
{
  temp <- .45+(Day/nDayCycle-.5)^2;
  return(temp);
}

#Set parameters for when reproductives stop foraging
isReprodForage <- function(nWorker)
{
  return(ifelse(nWorker<2,TRUE,FALSE))
}