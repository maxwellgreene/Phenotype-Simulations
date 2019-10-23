
logger <- function(temp)
{
  if(!exists("logT")){logT <<- c(1,1,1,0,0,0)}
  
  logT <<- ifelse(temp>0,temp,logT)
  
  cat("\n Day: ",logT[1],"/",logT[4],
      "Rep: ",logT[2],"/",logT[5],
      "Gen: ",logT[3],"/",logT[6])
}