stat.discretization <- function(X,D, states=c(0,1))
{
  nX<- nrow(X)
  mX<- ncol(X)
  nD <- nrow(D)
  mD <- ncol(D)
  if(mX!=mD | nX!=nD){
    return("error")
  }
  temp <- matrix(0, nrow=nX, ncol=mX)
  temp[X==states[2] & D==states[2]] <- 1
  TP <- sum(temp)
  
  temp <- matrix(0, nrow=nX, ncol=mX)
  temp[X==states[2] & D==states[1]] <- 1
  FP <- sum(temp)
  
  temp <- matrix(0, nrow=nX, ncol=mX)
  temp[X==states[1] & D==states[1]] <- 1
  TN <- sum(temp)
  
  temp <- matrix(0, nrow=nX, ncol=mX)
  temp[X==states[1] & D==states[2]] <- 1
  FN <- sum(temp)
  
  precision <- TP/(TP+FP)
  recall <- TP/(TP+FN)
  accuracy <- (TP+TN)/(TP+TN+FP+FN)
  
  return(list(TP, FP, TN, FN, precision, recall, accuracy))
}