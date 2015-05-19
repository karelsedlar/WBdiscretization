data.norm <- function(X){
  n <- nrow(X)
  m <- ncol(X)
  for(i in 1:n){
    X[i,] <- X[i,]/max(X[i,])
  }
  return(X)
}