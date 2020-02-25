saveData <- function()
{
  source('/cloud/project/Sociality Simulation/Params.R')
  source('/cloud/project/Sociality Simulation/VisData.R')
  source('/cloud/project/Sociality Simulation/SocialitySimulation.R')
  env <- c(.6,.7,.8,.75)
  colony <- c(.5,1,.5,.5)
  
  v_viscol <- runSimMan(env,colony);
  v_visrephist <- runSimRepMan(1000,env,colony);
  v_vispopparams <- runSimAllEnvGen(env);
  v_vislinpath <- runSimAllEnvGenLin(env)
}

vis1 <- function()
{
  VisCol(v_viscol,env,colony)
  #VisCol(runSimMan(env,colony),env,colony)
}

vis2 <- function()
{
  VisRepHist(v_visrephist,env,colony)
  #VisRepHist(runSimRepMan(1000,env,colony),env,colony)
}

vis3 <- function()
{
  VisPopParamsPlotly(v_vispopparams)
  #VisPopParamsPlotly(runSimAllEnvGen(env))
}

vis4 <- function()
{
  VisAncTree(v_visanctree,0)
  #VisAncTree(runSimAllEnvGenLin(env),0)
}


