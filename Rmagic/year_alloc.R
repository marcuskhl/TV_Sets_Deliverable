year_alloc <- function(x,template,yrs=years){
  y<-x
  for(i in 1:length(yrs)){
    y[,,i] <- template
  }
  y
}

# This is a handy function that allows the replication inputs across chronological years to form a 3d array.
# In our case, we have used it for mean and standard deviation of failure years. 

# These are defined per country and year of installation, however to use them as an argument for
# our 3d failure distribution we must replicate them across our chronological years.