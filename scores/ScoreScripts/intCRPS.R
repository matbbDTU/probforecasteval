# Numerical d-dimensional integration

intCrps <- function(d,igd,l,u){
  
  converged <- F
  reltol <- 0.01
  
  if(sum(l<u)==d){
    while(converged == F){
      hide <- capture.output(int <- vegas(d,1,igd,lower=l,upper=u,rel.tol=reltol))
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
    int$value <- 0
    int$ifail <- 0
  }
  
  return(int)
  
}
