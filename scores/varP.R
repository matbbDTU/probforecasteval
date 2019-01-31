varP <- function(f,y,p){
  
  n <- dim(f)[1] # Size of ensemble
  d <- dim(f)[2] # Dimension
  
  # Iterate through all pairs
  score <- 0
  for(i in 1:(d-1)){
    for(j in (i+1):d){
      
      Ediff <- 1/n*sum(abs(f[,i]-f[,j])^p)
      score <- score + (abs(y[i]-y[j])^p - Ediff)^2
      
    }
  }
  
  # Result
  return(score)
}