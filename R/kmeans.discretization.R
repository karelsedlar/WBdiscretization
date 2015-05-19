kmeans.discretization <- function(X, l=2, nstart=100, iter.max=250)
{
  n <- nrow(X)
  m <- ncol(X)
  temp <- kmeans(t(X), centers=l, iter.max = iter.max, nstart = nstart)
  temp_data <- temp[[1]]
  iod <- temp[[2]]
  iod_mean <- rep(0,l)
  D <- rep(0,m)
  for(i in 1:l){
    iod_mean[i] <- mean(iod[i,])
  }
  iod_mean_sorted <- sort(iod_mean)
  level <- 1:l
  for(i in 1:l){
    nbin <- level[iod_mean==iod_mean_sorted[i]]
    D[temp_data==nbin] <- i
  }
  names(D) <- names(X)

  return(D)
}