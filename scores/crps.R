library(Emcdf)
library(R2Cuba)
source("ScoreScripts/intCRPS.R")

crps <- function(f,y){
  
  # Bounds and dimensions
  f <- as.matrix(f)
  y <- c(y)
  l <- apply(f,2,min)
  u <- apply(f,2,max)
  n <- dim(f)[1]
  d <- dim(f)[2]
  
  # Lower integrand
  igdL <- function(x){
    emcdf(f,x)^2
  }
  
  # Upper integrand
  igdU <- function(x){
    (emcdf(f,x)-1)^2
  }
  
  # Lower integral
  intL <- intCrps(d,igdL,l,y)
  
  # Upper integral
  intU <- intCrps(d,igdU,y,u)
  
  # CRPS result
  if((intL$ifail== 0) && (intU$ifail==0)){
    return(intL$value + intU$value)
  }else{
    return(NA)
  }

}
