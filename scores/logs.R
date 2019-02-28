library(ks)

logs <- function(x,y){

  # Estimate kernel density and evaluate fhat(y)
  fhat_y <- kde(x,eval.points = y)$estimate
  
  # Log score
  -log(fhat_y)
  
}
