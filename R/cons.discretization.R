cons.discretization <- function(X, method="mean", p=0.5)
{
  n <- nrow(X)
  m <- ncol(X)
  D <- abs.discretization(X, method, p)[[1]]
  K <- data.frame(matrix(0,ncol=m, nrow=1))
  names(K) <- names(X)
  for(i in 1:m){
    freq <- table(D[,i])
    K[1,i] <- as.numeric(names(freq)[which.max(freq)])
  }
  return(K)
}