#This file is utilities for data analysis collected from simulation


################################################################
###======================  VisRepHist  ======================###
################################################################
VisRepHist <- function(nRun,environment,colony)
{
  nReprods <- runSimRepMan(nRun,environment,colony)
  hist(nReprods,breaks=20,main=toString(c("Environment:",environment,"Colony:",colony)))
}

################################################################
###========================  VisCol  ========================###
################################################################
VisCol <- function(colony)
{
  ggplot(data,aes(x=timestep)) + 
    geom_line(aes(y=kStore,color="kStore")) + 
    geom_line(aes(y=nReprod,color="nReprod")) + 
    geom_line(aes(y=nWorker,color="nWorker")) + 
    geom_line(aes(y=max(c(nWorker,nReprod))*fMortRate(enviro,timestep,nDayCycle,type="numeric"),color="fMortRate"),linetype="dashed") +
    geom_line(aes(y=max(c(nWorker,nReprod))*birthReprod(colony,timestep,nDayCycle,type="numeric"),color="birthReprod")) +
    scale_colour_manual("",breaks = c("kStore", "nReprod", "nWorker","fMortRate","birthReprod"),
                        values = c("kStore"="green","nReprod"="red","nWorker"="blue","fMortRate"="black","birthReprod"="black"))
}

################################################################
###=====================  VisPopParams  =====================###
################################################################

VisPopParams <- function(population)
{
  plot_ly(x=colony$early, y=colony$middle, z=colony$late, type="scatter3d", mode="markers", color=colony$nReprod) %>% 
    layout(title = "Number of Reproductives as a Function of Colony",
           scene = list(
             xaxis = list(title = "Early Season"),
             yaxis = list(title = "Middle Season"),
             zaxis = list(title = "Late Season")))
}