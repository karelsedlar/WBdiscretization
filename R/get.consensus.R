get.consensus <- function(D)
{
  n <- nrow(D)
  m <- ncol(D)
  K <- data.frame(matrix(0,ncol=m, nrow=1))
  names(K) <- names(D)
  for(i in 1:m){
    freq <- table(D[,i])
    K[1,i] <- as.numeric(names(freq)[which.max(freq)])
  }
  return(K)
}