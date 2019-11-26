#This file is utilities for data analysis collected from simulation

################################################################
###========================  VisCol  ========================###
################################################################

#Visualize single colony dynamcs throughout the season
VisCol <- function(colonyData,enviro,colony,colParams = c(0,100,1,0,0,2,2.5,2,2,5,5.5))
{
  #Set number of Days in a Cycle
  Day <- colParams[1]; nDayCycle <- colParams[2];
  #Starting number of reproductives, workers and energy stores
  nReprod <- colParams[3]; nWorker <- colParams[4]; kStore <- colParams[5];
  #Number of trips that workers and reproductives can make each day
  nTripReprod <- colParams[6]; nTripWorker <- colParams[7];
  #Set amount of energy per trip that workers and reproductives make
  kTripWorker <- colParams[8]; kTripReprod <- colParams[9];
  #Set amount of energy needer and reproductive
  kCreateWorker <- colParams[10]; kCreateReprod <- colParams[11];
  
  #Requires running runSimMan first
  ggplot(data=colonyData,aes(x=timestep)) + 
    geom_line(aes(y=kStore,color="kStore")) + 
    geom_line(aes(y=nReprod,color="nReprod")) + 
    geom_line(aes(y=nWorker,color="nWorker")) + 
    geom_line(aes(y=max(c(nWorker,nReprod))*fMortRate(enviro,timestep,nDayCycle,type="numeric"),color="fMortRate"),linetype="dashed") +
    geom_line(aes(y=max(c(nWorker,nReprod))*birthReprod(colony,timestep,nDayCycle,type="numeric"),color="birthReprod")) +
    scale_colour_manual("",breaks = c("kStore", "nReprod", "nWorker","fMortRate","birthReprod"),
                        values = c("kStore"="green","nReprod"="red","nWorker"="blue","fMortRate"="black","birthReprod"="black")) +
    ggtitle(paste("Environment: (",toString(enviro),") \nColony:          (",toString(colony),")",sep=""))
}

################################################################
###======================  VisRepHist  ======================###
################################################################

VisRepHist <- function(nReprods,enviro,colony)
{
  #nReprods <- runSimRepMan(nRun,enviro,colony)
  hist(nReprods,breaks=20,
       main=paste("Environment: (",toString(enviro),") \nColony: (",toString(colony),")",sep=""))
}

################################################################
###==================  VisPopParamsPlotly  ==================###
################################################################

# Plot the results of a single-generation or evolved population
# in an interactive plotly plot
VisPopParamsPlotly <- function(population)
{
  plot_ly(x=population$early, y=population$middle, z=population$late, type="scatter3d", mode="markers", color=population$nReprod) %>% 
    layout(title = "Number of Reproductives as a Function of Colony",
           scene = list(
             xaxis = list(title = "Early Season"),
             yaxis = list(title = "Middle Season"),
             zaxis = list(title = "Late Season")))
}

################################################################
###====================  VisPopParams3d  ====================###
################################################################

# Plot the results of a single-generation or evolved population
# in a static scatterplot3d plot (for notebooks)
VisPopParams3d <- function(population)
{
  #plot <- scatterplot3d(x=population$early,
  #              y=population$middle,
  #              z=population$late,
  #              xlab = "Early", ylab = "Middle", zlab = "Late")
  scatter3D(population$early,population$middle,population$late,colvar = population$nReprod)
  #return(plot3D::scatter3D(population$early,population$middle,population$late,colvar = population$nReprod))
}





