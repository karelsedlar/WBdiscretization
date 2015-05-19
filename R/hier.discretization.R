hier.discretization <- function(X, l=2)
{
  n <- nrow(X)
  m <- ncol(X)
  d <- dist(t(X), method = "euclidean")
  tree <- hclust(d, method = "average")
  temp <-cutree(tree, k = l)
  D <- rep(0,m)
  iod_mean <- rep(0,l)
  for(i in 1:l){
    iod_mean[i] <- mean(as.matrix(frs2[temp==i]))
  }
  iod_mean_sorted <- sort(iod_mean)
  level <- 1:l
  for(i in 1:l){
    nbin <- level[iod_mean==iod_mean_sorted[i]]
    D[temp==nbin] <- i
  }
  names(D) <- names(X)
  
  return(D)
}