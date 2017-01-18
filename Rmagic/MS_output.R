# Quick scrit for MS output
library(reshape)
extract <- read.csv("M:/Technology/DATA/TV_sets_model/Integration/inputs/MS_extract.csv")
extract <- melt(extract,id.vars=colnames(extract)[1:5])
extract <- cast(extract,...~Measure,fun.aggregate = sum)
extract[is.na(extract)]<- 0 
extract$variable <-gsub("X", "", extract$variable)
extract$TV.Type <- as.character(extract$TV.Type)
extract$TV.Type[extract$TV.Type=="Connected"] <- "Smart - connected"
write.csv(extract,"M:/Technology/DATA/TV_sets_model/Integration/extract.csv")