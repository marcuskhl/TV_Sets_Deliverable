displayalloc <- function(x,y,tlist=techlist){
  for(i in 1:length(tlist)){
    y[,,,i] <- x
  }
  y
}