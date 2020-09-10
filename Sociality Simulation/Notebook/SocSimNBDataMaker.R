
saveData <- function()
{
  env1 <- c(0.5,1.0,0.5,0.5)
  env2 <- c(0.6,0.7,0.8,0.75)
  env3 <- c(0.8,0.2,0.6,0.9)
  env4 <- c(0.8,0.2,0.6,0.9)
  
  col1 <- c(0.5,1.0,0.5,0.5)
  col2 <- c(0.6,0.7,0.8,0.75)
  col3 <- c(0.3,0.3,1,0.15)
  col4 <- c(0.0,0.0,1,0.2)
  
  
  colony1 <- runSimMan(c(0.8,0.2,0.6,0.9),c(0.3,0.3,1,0.15))
  colony2 <- runSimMan(c(0.8,0.2,0.6,0.9),c(0.0,0.0,1,0.2))
  colony3 <- runSimMan(env1,col1)
  colony4 <- runSimMan(env2,col2)
  
  nReprods1 <- runSimAllDen(100,env=c(0.8,0.2,0.6,0.9),col=c(0.3,0.3,1,0.15),range = 0,nEach = 1,which=rep(0,8))
  
  popden1_1 <- runSimAllDen(1000,nEach=1,which=rep(0,8),range=0,env=c(0.8,0.2,0.6,0.9),col=c(0.3,0.3,1,0.15))
  popden1_2 <- runSimAllDen(1000,nEach=1,which=rep(0,8),range=0,env=c(0.8,0.2,0.6,0.9),col=c(0.0,0.0,1,0.2))
  popden1_3 <- runSimAllDen(1000,nEach=1,which=rep(0,8),range=0,env=env1,col=col1)
  popden1_4 <- runSimAllDen(1000,nEach=1,which=rep(0,8),range=0,env=env2,col=col2)
  
  save.image(file="SocSimNBWS.RData")
}


