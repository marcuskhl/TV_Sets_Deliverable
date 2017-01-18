makeMatrix <- function(x,rnames=countries,yrs=1990:2020){
  xmat <- data.matrix(x[,2:ncol(x)])
  colnames(xmat) <- yrs
  rownames(xmat) <- rnames
  xmat
}