list.of.packages <- c("openxlsx", "reshape2","reshape", "dplyr","reshape2","data.table", "readxl","tidyr","pbapply","tcltk", "MASS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){ install.packages(new.packages)}
lapply(list.of.packages, library, character.only = TRUE)
cat("\014") # clear console from uninformative stuff

# TV Sets Intelligence Tracker <- the place to upload in iTools

#get historic and forecast pivot file from here https://technology.ihs.com/531520
#find a pivot, remove every filter and double click to get flat data
#paste it to the Forecast_Paste sheets in /TRAX/

Forecast_Start_Year <- 2016
Forecast_Start_Quarter <- "Q4-16"
#Historic doesnt have 2011 either
# Historic_Paste <- read.xlsx("M:/Technology/DATA/TV_sets_model/Integration/TRAX/_TV sets_Flat File for TRAX builder.xlsx",
#                                       sheet = "Historic_Paste", startRow = 1, colNames = T,
#                                       rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
#                                       rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
Forecast_Paste <- read.xlsx("M:/Technology/DATA/TV_sets_model/Integration/TRAX/_TV sets_Flat File for TRAX builder.xlsx",
                                      sheet = "Forecast_Paste", startRow = 1, colNames = T,
                                      rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                                      rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

Forecast_Paste <- Forecast_Paste[,-match("Size.Category.(10\")",colnames(Forecast_Paste))]

colnames(Forecast_Paste)[match("Size.Category.(5\")",colnames(Forecast_Paste))] <- "Size.Category"

#Flat_Paste <- rbind.data.frame(Historic_Paste, Forecast_Paste)
Flat_Paste<-Forecast_Paste
Flat_Paste <- Flat_Paste[,c(1:14,17:18,20:23,15,16,19,24)]
Flat_Paste <- melt(Flat_Paste, id.vars = colnames(Flat_Paste)[1:(length(colnames(Flat_Paste))-4)])
colnames(Flat_Paste)[length(colnames(Flat_Paste))-1] <- "Measure"

cols <- c("Measure","Technology","Region","Size","Size.Category","Brand", "Sub-Brand","Resolution", "Format","Price.Range","Quarter","Year","value")
Flat_Paste <- data.table(Flat_Paste)
Flat_TRAX <- Flat_Paste[, cols, with = FALSE]
Flat_TRAX <- unique(Flat_TRAX) #there are overlaps between Forecast and Historic
Flat_TRAX <- Flat_TRAX[,list(value = sum(value)), by =c("Measure","Technology","Region","Size","Size.Category","Brand", "Sub-Brand","Resolution", "Format","Price.Range","Year","Quarter")]
Flat_TRAX_Annual <- Flat_TRAX[,list(value = sum(value)), by =c("Measure","Technology","Region","Size","Size.Category","Brand", "Sub-Brand","Resolution", "Format","Price.Range","Year")]
Flat_TRAX_Annual <-data.frame(Flat_TRAX_Annual)
Flat_TRAX_Annual$Year[Flat_TRAX_Annual$Year>=Forecast_Start_Year] <- paste(Flat_TRAX_Annual$Year[Flat_TRAX_Annual$Year>=Forecast_Start_Year]," (F)",sep="")
Flat_TRAX_Annual$Year <- paste("Y",Flat_TRAX_Annual$Year,sep="")
Flat_TRAX_Annual <-spread(Flat_TRAX_Annual,Year,value)
Flat_TRAX <- data.frame(Flat_TRAX)
Flat_TRAX$`Quarter-Year` <- paste(Flat_TRAX$Quarter,"-",as.numeric(as.character(Flat_TRAX$Year))-2000,sep = "")
Flat_TRAX <- Flat_TRAX[,-match(c("Quarter","Year"),colnames(Flat_TRAX))]
list <- unique(Flat_TRAX$`Quarter-Year`)

Forecast_Start <-match(Forecast_Start_Quarter,list)
Flat_TRAX$`Quarter-Year`[match(Flat_TRAX$`Quarter-Year`,list)>=Forecast_Start] <- paste(Flat_TRAX$`Quarter-Year`[match(Flat_TRAX$`Quarter-Year`,list)>=Forecast_Start]," (F)",sep="")
Flat_TRAX$`Quarter-Year` <-  gsub("-",  "a", Flat_TRAX$`Quarter-Year`)
Flat_TRAX$year <- lapply(strsplit(Flat_TRAX$`Quarter-Year`, "a"), "[", 2)
Flat_TRAX$`Quarter-Year` <-  gsub("a",  "-", Flat_TRAX$`Quarter-Year`)
Flat_TRAX <- data.frame(Flat_TRAX)
Flat_TRAX <- Flat_TRAX[order(unlist(Flat_TRAX$year)),]
# Flat_TRAX$`Quarter-Year`[match(Flat_TRAX$`Quarter-Year`,list)>=match(Forecast_Start_Quarter,list)] <- 
#   paste(Flat_TRAX$`Quarter-Year`[match(Flat_TRAX$`Quarter-Year`,list)>match(Forecast_Start_Quarter,list)])
colnames(Flat_TRAX)[12] <- "Quarter-Year"
Flat_TRAX <-Flat_TRAX[,1:12]
Flat_TRAX <-spread(Flat_TRAX,`Quarter-Year`,value)
Flat_TRAX$Measure <- gsub(".", " ", Flat_TRAX$Measure, fixed = TRUE)
Flat_TRAX_Annual$Measure <- gsub(".", " ", Flat_TRAX_Annual$Measure, fixed = TRUE)
TS_Trax<- suppressMessages(inner_join(Flat_TRAX_Annual,Flat_TRAX))


colnames(TS_Trax) <- gsub(".", " ",colnames(TS_Trax), fixed = TRUE)
TS_Trax$Measure[TS_Trax$Measure=="Avg Size × Qty" ] <- "(H) Avg Size × Qty" 
TS_Trax$Measure[TS_Trax$Measure=="Area (000 m²)" ] <- "Area - Sq meters (K)"
TS_Trax$Measure[TS_Trax$Measure=="Quantity (000s)"  ] <-   "Quantity - Units (K) "
TS_Trax$Measure[TS_Trax$Measure=="Revenue ($US 000s)" ] <- "Revenue - USD (K)" 
#rm empty rows
TS_Trax <- TS_Trax[rowSums(is.na(TS_Trax[,11:length(colnames(TS_Trax))])) != ncol(TS_Trax[,11:length(colnames(TS_Trax))]),]
TS_Trax[,11:length(colnames(TS_Trax))] <- round(TS_Trax[,11:length(colnames(TS_Trax))],6)

wb <- createWorkbook()
addWorksheet(wb,sheetName ="TV Sets Intelligence TRAX")
writeData(wb, "TV Sets Intelligence TRAX", TS_Trax)
saveWorkbook(wb, "M:/Technology/DATA/TV_sets_model/Integration/TRAX/_TV sets_Flat File for TRAX.xlsx", overwrite = T)# this is for Country Model
