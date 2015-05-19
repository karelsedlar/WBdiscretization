abs.discretization <- function(X, method="mean", p=0.5)
{
  n <- nrow(X)
  m <- ncol(X)
  D <- as.data.frame(matrix(0,ncol=m, nrow=n))
  threshold <- matrix(0,ncol=1, nrow=n)
  names(D) <- names(X)
  row.names(D) <- row.names(X)
  row.names(threshold) <- row.names(X)
  if (method=="mean"){
    for (i in 1:n){
      threshold[i,1]=mean(as.numeric(X[i,]))
      D[i,which(as.numeric(X[i,])>threshold[i,1])]=1
    }    
  }
  else if(method=="median"){
    for (i in 1:n){
      threshold[i,1]=median(as.numeric(X[i,]))
      D[i,which(as.numeric(X[i,])>threshold[i,1])]=1
    }
  }
  else if(method=="max"){
    for (i in 1:n){
      threshold[i,1]=max(as.numeric(X[i,]))*p
      D[i,which(as.numeric(X[i,])>threshold[i,1])]=1
    }    
  }
  else if(method=="percentile"){
    for (i in 1:n){
      threshold[i,1]=as.numeric(quantile(as.numeric(X[i,]),p))
      D[i,which(as.numeric(X[i,])>threshold[i,1])]=1
    }
  }
  
  return(list(D, threshold))
}