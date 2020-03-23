temp <- function()
{
  env <- c(.7,.7,.7,.75)
  col <- c(.1,.1,.7,.3)
  data46 <<- runSimAllDen(2500,10,0.01,env,col,which = c(0,0,0,1,0,0,0,0))
  VisDensity(data46,1,64)
}