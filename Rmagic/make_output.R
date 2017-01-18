make_output<-function(input_array,nlist=splitlist){
  for(i in 1:length(splitlist)){
    x=paste("C:/Users/jtb44363/Documents/Integration/outputs",paste(deparse(substitute(input_array)),i,nlist[i],".csv",sep=""),sep="/")
    write.csv(input_array[,,i],x)
  }
}
