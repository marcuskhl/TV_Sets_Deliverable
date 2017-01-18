# This script reads the output

library(reshape)
library(data.table)
library(dplyr)
library(tidyr)


yr <- 2020
output_list <- dir(path="M:/Technology/DATA/TV_sets_model/Integration/inputs/imported_output/")
#for(i in 1:3){
for(i in 1:length(output_list)){
  if(i==1){
    output_extract <- fread(paste("M:/Technology/DATA/TV_sets_model/Integration/inputs/imported_output",
                                     output_list[i],sep="/"), header = T)
  }
  else{
    output_extract <- rbind(output_extract,
              fread(paste("M:/Technology/DATA/TV_sets_model/Integration/inputs/imported_output",output_list[i],sep="/"), header = T))
  }

}
output_extract <- as.data.frame(output_extract)
print("extract produced")

# n=levels(output_extract$Measure)[1:length(levels(output_extract$Measure))-1]
#
# for(i in 1:length(n)){
#   write.csv(melt(output_extract[output_extract$Measure==levels(output_extract$Measure)[i],],
#                  id.vars=colnames(output_extract[output_extract$Measure==levels(output_extract$Measure)[i],])[1:6]),
#                  paste("C:/Users/jtb44363/Documents/Integration/outputs/output_extract",i,".csv",sep=""))
# }

#output_extract<-melt(output_extract,id.vars=colnames(output_extract)[1:6])
output_extract <- output_extract %>%
  gather(variable, value,`1990`:`2020`) %>% # <-----------------------------------------------------------CHANGE
  spread(Metric, value)

#output_extract <- cast(output_extract,...~Metric)
output_extract[is.na(output_extract)] <- 0
output_extract$variable <-gsub("X", "", output_extract$variable)
output_extract$Submeasure <- as.character(output_extract$Submeasure)
output_extract$Submeasure[output_extract$Submeasure == "Unsmart"] <- "Non-Smart"
#write.csv(output_extract,"M:/Technology/DATA/TV_sets_model/Integration/output_extract.csv")


fwrite(output_extract,"M:/Technology/DATA/TV_sets_model/Integration/output_extract.csv")


# write.csv(output_extract,"C:/Users/jtb44363/Documents/Integration/output_extract.csv"

source('M:/Technology/DATA/TV_sets_model/Integration/Rmagic/MS_output.R')
# draws the MS output into pivot form.
print("Both extracts completed")
