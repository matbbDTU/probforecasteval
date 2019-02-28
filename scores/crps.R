library(Emcdf)
library(R2Cuba)
source("ScoreScripts/intCRPS.R")

crps <- function(x,y){
  
  # Bounds and dimensions
  x <- as.matrix(x)
  y <- c(y)
  l <- apply(x,2,min)
  u <- apply(x,2,max)
  m <- dim(x)[1]
  k <- dim(x)[2]
  
  # Lower integrand
  igdL <- function(u){
    emcdf(x,u)^2
  }
  
  # Upper integrand
  igdU <- function(u){
    (emcdf(x,u)-1)^2
  }
  
  # Lower integral
  intL <- intCrps(k,igdL,l,y)
  
  # Upper integral
  intU <- intCrps(k,igdU,y,u)
  
  # CRPS result
  if((intL$ifail== 0) && (intU$ifail==0)){
    return(intL$value + intU$value)
  }else{
    return(NA)
  }

}
