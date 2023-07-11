# Numerical d-dimensional integration

intCrps <- function(d,igd,l,u){
  
  converged <- F
  reltol <- 0.01
  
  if(sum(l<u)==d){
    while(converged == F){
      hide <- capture.output(int <- vegas(igd,lowerLimit=l,upperLimit=u,relTol=reltol))
      if(converged == F){
        reltol <- reltol + 0.01
      }
      else{
        converged <- T
      }
      
      if(reltol > 0.1){
        converged <- T
      }
    }
  }else{
    int<-NULL
    int$integral <- 0
    int$returnCode <- 1
  }
  
  return(int)
  
}
