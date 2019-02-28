source("ScoreScripts/defineECDF.R")
source("ScoreScripts/intCRPS.R")
require(R2Cuba)

crps <- function(f,y){
  
  # Bounds and dimensions
  f <- as.matrix(f)
  l <- apply(f,2,min)
  u <- apply(f,2,max)
  n <- dim(f)[1]
  d <- dim(f)[2]
  
  # Estimate CDF
  cdf <- defineECDF(f,n,d)
  
  # Lower integrand
  igdL <- function(x){
    cdf(x)^2
  }
  
  # Upper integrand
  igdU <- function(x){
    (cdf(x)-1)^2
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
