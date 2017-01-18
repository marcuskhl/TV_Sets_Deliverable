crd<-function(x){
  vcol<-as.numeric(colnames(year_diff))
  vrow<-as.numeric(rownames(year_diff))
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      x[i,j] <- vcol[j]-vrow[i]
    }
  }
  x
}

# The crd or "Column-Row Deduct" function.

# A very handy specific function that allows for calculating the difference between the current year
#and the year of calculation. 

# Chronological years go across the columns from left to right.
# Installed base/Installation years go from top to bottom.