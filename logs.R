require(ks)

logs <- function(f,y){

  -log(kde(f,eval.points = as.numeric(y))$estimate)
  
}