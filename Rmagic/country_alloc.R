country_alloc <- function(x,template,clist=countries){
  y<-x
  for(i in clist){
    y[i,,] <- template
  }
  y
}

# This function allows for the replication of a matrix across a list of countries to produce a 3D array.
# It is used to replicate the year difference inputs.

# Because 2008-2005 is the same no matter what country you are in. :)