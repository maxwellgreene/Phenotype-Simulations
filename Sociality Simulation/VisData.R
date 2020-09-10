#This file is utilities for data analysis collected from simulation

################################################################
###========================  VisCol  ========================###
################################################################

#Visualize single colony dynamcs throughout the season
VisCol <- function(colonyData,enviro,colony,colParams = c(0,100,1,0,0,2,2,5,5.5))
{
  #Set number of Days in a Cycle
  Day <- colParams[1]; nDayCycle <- colParams[2];
  #Starting number of reproductives, workers and energy stores
  nReprod <- colParams[3]; nWorker <- colParams[4]; kStore <- colParams[5];
  #Set amount of energy per trip that workers and reproductives make
  kTripWorker <- colParams[6]; kTripReprod <- colParams[7];
  #Set amount of energy needed to create worker and reproductive
  kCreateWorker <- colParams[8]; kCreateReprod <- colParams[9];
  
  #Requires running runSimMan first
  ggplot(data=colonyData,aes(x=timestep)) + 
    geom_line(aes(y=kStore,color="Energy Store")) + 
    geom_line(aes(y=nReprod,color="Reproductives")) + 
    geom_line(aes(y=nWorker,color="Workers")) + 
    geom_line(aes(y=max(c(nWorker,nReprod))*fMortRate(enviro,timestep,nDayCycle,type="numeric"),color="Survival Rate"),linetype="dashed") +
    geom_line(aes(y=max(c(nWorker,nReprod))*birthReprod(colony,timestep,nDayCycle,type="numeric"),color="Reprod:Worker")) +
    scale_colour_manual("Legend",breaks = c("Energy Store", "Reproductives", "Workers","Survival Rate","Reprod:Worker"),
                        values = c("Energy Store"="green","Reproductives"="red","Workers"="blue","Survival Rate"="black","Reprod:Worker"="black")) +
    ggtitle(paste("Environment: (",toString(enviro),") \nColony:          (",toString(colony),")",sep="")) + 
    labs(x="Timestep (Day)",y="Quantity")
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
###======================  VisDenComp  ======================###
################################################################
VisDenVect <- function(vect1,vect2,vect3,vect4)
{
  dat <- data.frame(vects = c(vect1,vect2,vect3,vect4), divs = rep(c("a", "b","c","d"), each = length(vect1)))
  ggplot(dat, aes(x = vects, fill = divs)) + geom_density(alpha = 0.5)
}

################################################################
###======================  VisDensity  ======================###
################################################################
VisDensity <- function(temp,id,numplot,legend = FALSE)
{
  params <- temp[[1]]
  results <- temp[[2]]
  vects <- NULL
  
  for(i in 1:numplot)
  {
    vects <- c(vects,as.vector(results[[id+i-1]]))
  }
  
  df <- data.frame(vects = vects, 
                   divs  = as.character(rep(1:numplot,each = length(results[[1]]))))
  
  plot <- ggplot(df, aes(x = vects, fill = divs)) + 
    geom_density(alpha = 0.15) + xlab("Number of Reproductives") + ylab("Density") + 
    ggtitle("Reproductives at End of Season")
  
  if(!legend)
  {plot <- plot + theme(legend.title = element_blank(), legend.position = "none")}
  
  plot
}

################################################################
###===================  VisDensityParams  ===================###
################################################################

VisDensityParams <- function(temp)
{
  params <- temp[[1]]
  
  plotdata <- data.frame(time=double(),type=character(),value=double())
  t <- seq(from=0,to=1,length.out = 1001)
  
  for(i in rows(params))
  {
    p <- params[i,]
    print(p)
    
    Dw <- p$envScale*(p$envEarly*(t-1)^2 + p$envMiddle*(1-4*(t-.5)^2) + p$envLate*t^2)
    Br <- p$colScale*(p$colEarly*(t-1)^2 + p$colMiddle*(1-4*(t-.5)^2) + p$colLate*t^2)
    #Dw <- Dw / max(Dw); Br <- Br / max(Br)
    tempdata <- data.frame(time = rep(t,2),
                     value= c(Dw,Br),
                     type = c(rep("Survival Rate",length(t)),rep("Birth Rate",length(t))))
    plotdata <- rbind(plotdata,tempdata)
  }
  ggplot(plotdata) + geom_line(aes(x=time,y=value,linetype=type),lwd=.5) + xlab("Seasonal Progress (Percent)") + 
    ylab("Value") + theme(legend.title = element_blank()) + ggtitle("Parameter Values") + ylim(c(0,1))
}

################################################################
###==================  VisPopParamsPlotly  ==================###
################################################################

# Plot the results of a single-generation or evolved population
# in an interactive plotly plot
VisPopParamsPlotly <- function(population)
{
  plot_ly(x=population$early, y=population$middle, z=population$late, 
          type="scatter3d", mode="markers", color=population$nReprod) %>% 
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
VisPopParams3d <- function(pop)
{
  #plot <- scatterplot3d(x=population$early,
  #              y=population$middle,
  #              z=population$late,
  #              xlab = "Early", ylab = "Middle", zlab = "Late")
  scatter3D(pop$early,pop$middle,pop$late,colvar = pop$nReprod)
  #return(plot3D::scatter3D(population$early,population$middle,population$late,colvar = population$nReprod))
}

################################################################
###======================  ggConvert  =======================###
################################################################

#converts population into standard ggenealogy data.frame
ggConvert <- function(popDF,varname)
{
  #colnames(popDF) <- c("early","middle","late","scale","nReprod","child","parent","generation")
  popDF <- popDF[c("child","parent",varname)]
  popDF$child <- as.character(popDF$child)
  popDF$parent <- as.character(popDF$parent)
  tempdf <- data.frame(child="0",parent="NA",tempname=0)
  names(tempdf)[names(tempdf) == "tempname"] <- varname
  popDF <- rbind(popDF,tempdf)
  return(popDF)
}

################################################################
###======================  VisLinPath  ======================###
################################################################

#Plot a visualpath between individuals in a population
VisLinPath <- function(popDF,to,from,varname,varname2)
{
  #colnames(popDF) <- c("early","middle","late","scale","nReprod","child","parent","generation")
  popDF <- popDF[c("child","parent",varname,varname2)]
  popDF$child <- as.character(popDF$child);
  popDF$parent <- as.character(popDF$parent);
  tempdf <- data.frame(child="0",parent="NA",tempname=-1,tempname2=-1)
  names(tempdf)[names(tempdf) == "tempname"] <- varname
  names(tempdf)[names(tempdf) == "tempname2"] <- varname2
  
  popDF <- rbind(popDF,tempdf)
  
  popIG <- dfToIG(popDF)
  path <- getPath(to,from,popIG,popDF,varname)
  plotPathOnAll(path,popDF,popIG,varname,varname2,pathEdgeCol = "orange") + 
    ggplot2::xlab(varname) +
    ggplot2::ylab(varname2)
  #plotPathOnAll(path,popDF,popIG,varname)
  #plotPath(path,popDF,varname)
}

################################################################
###======================  VisAncTree  ======================###
################################################################

#Plot a n ancestral tree of all individuals in a population
VisAncTree<- function(popDF,indiv,mAnc=0,mDes=20)
{
  popDF <- ggConvert(popDF,"nReprod")
  plotAncDes(indiv, popDF, mAnc = mAnc, mDes = mDes)
}







