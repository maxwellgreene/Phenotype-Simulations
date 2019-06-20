#Maxwell Greene
#Genetic Evolution

#' Evolve a Population
#'
#' @param fitfunc A function; used to evaluate the fitness of each individual each generation.
#' @param species A scalar; the number of species to create and evolve.
#' @param gens A scalar; the number of generations each species should be eveolved over.
#' @param indivs A scalar; the number of individuals per species to create and evolve.
#' @param creation A character; the
#'
#'

geneticPopulation <- function(fitfunc,species,gens,indivs,creation = "new", plot = "maxVals")
{
  #========== CREATE DATAFRAME ==========
  specData <- data.frame(speciesNum=0,timestep=1,avgFit=0,maxFit=0,minFit=0,var1=0,var2=0)

  #========== MAIN LOOP ==========
  for(i in 1:species)
  {
    #========== GENERATION DATA ==========
    temp <- geneticSpecies(fitfunc = fitfunc,indivs = indivs,gens = gens,creation = creation,return=T)
    specData <- rbind(specData,
                      dplyr::mutate(speciesNum = rep(i,nrow(temp)),temp))
  }
  specData <<- specData

  #========== PLOT avgFIT BY TIME ==========
  if(plot == "time")
  {
    ggplot(specData) +
    geom_line(mapping = aes(
              x = timestep, y = avgFit,
              color = as.character(speciesNum))) +
      geom_line(mapping = aes(
        x = timestep, y = maxFit,
        color = as.character(speciesNum))) +
              theme(legend.position = "none")
  }else
  #========== PLOT maxFIT VALUES ==========
  if(plot == "maxVals")
  {
    X <- seq(-4,4,0.1); Y <- seq(-4,4,0.1);

    ggplot(specData) +
      geom_point(mapping = aes(x = var1,y = var2, color = as.character(speciesNum))) +
      geom_contour(data=expand.grid(X,Y),
                   mapping = aes(x=Var1,y=Var2,z=fitfunc(Var1,Var2)),bins = 20)+
      theme(legend.position = "none")
  }
}
