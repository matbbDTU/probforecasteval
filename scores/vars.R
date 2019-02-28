vars <- function(x,y,p=0.5){

  m <- dim(x)[1] # Size of ensemble
  k <- dim(x)[2] # Maximal forecast horizon
  
  # Iterate through all pairs
  score <- 0
  for(i in 1:(k-1)){
    for(j in (i+1):k){
      
      Ediff <- 1/m*sum(abs(x[,i]-x[,j])^p)
      score <- score + (abs(y[i]-y[j])^p - Ediff)^2
      
    }
  }
  
  # Variogram score
  return(score)
  
}
