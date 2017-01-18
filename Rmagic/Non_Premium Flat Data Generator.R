#~~~~~~Documentation Start~~~~~~#
# Flat data generator for domestic consumption
# File to paste to is TV Sets Domestic Consumption - Country Level_flat.xlsx
# output file is in Integration folder
#~~~~~~Documentation End~~~~~~#

# list.of.packages <- c("openxlsx", "reshape2",  "tidyr","plyr","dplyr","data.table","sqldf")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)){ install.packages(new.packages)}
# lapply(list.of.packages, library, character.only = TRUE)
# cat("\014")
# Aint nobody got time to run 4 lines
devtools::install_github("marcuskhl/BasicSettings")
library(BasicSettings)


#~~~~~~Read Stuff Start~~~~~~#
Revenues <- read.xlsx("M:/Technology/DATA/TV_sets_model/Integration/inputs/imported_output.xlsm", 
                   sheet = "Revenues", startRow = 2 , colNames = TRUE,
                   rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                   rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

Shipments <- read.xlsx("M:/Technology/DATA/TV_sets_model/Integration/inputs/imported_output.xlsm", 
                      sheet = "Shipments", startRow = 2 , colNames = TRUE,
                      rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                      rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

Domestic_Consumption <- read.xlsx("M:/Technology/DATA/TV_sets_model/Integration/inputs/imported_output.xlsm", 
                      sheet = "Domestic_consumption", startRow = 2 , colNames = TRUE,
                      rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                      rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

TV_installed_base <- read.xlsx("M:/Technology/DATA/TV_sets_model/Integration/inputs/imported_output.xlsm", 
                      sheet = "TV_installed_base", startRow = 2 , colNames = TRUE,
                      rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                      rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

TV_households <- read.xlsx("M:/Technology/DATA/TV_sets_model/Integration/inputs/imported_output.xlsm", 
                               sheet = "TV_households", startRow = 2 , colNames = TRUE,
                               rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                               rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

Country_List <- read.xlsx("M:/Technology/DATA/TV_sets_model/Integration/inputs/Non-Premium Country List.xlsx", 
                      sheet = "Country List", startRow = 1 , colNames = TRUE,
                      rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                      rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
#~~~~~~Read Stuff End~~~~~~#
Revenues <- Revenues[,c(1:2,4:5,24:length(Revenues))]
Shipments <- Shipments[,c(1:2,4:5,24:length(Shipments))]
Domestic_Consumption <- Domestic_Consumption[,c(1:2,4:5,24:length(Domestic_Consumption))]
TV_installed_base <- TV_installed_base[,c(1:2,4:5,24:length(TV_installed_base))]
TV_households <- TV_households[,c(1:2,4:5,24:length(TV_households))]

#~~~~~~Cleaning Start~~~~~~#
Revenues <- Revenues[Revenues$Measure =="Display Tech",]
Shipments <- Shipments[Shipments$Measure =="Display Tech",]
Domestic_Consumption <- Domestic_Consumption[Domestic_Consumption$Measure =="Display Tech",]
TV_installed_base <- TV_installed_base[TV_installed_base$Measure =="Display Tech",]
TV_households <- TV_households[TV_households$Measure =="Display Tech",]

Revenues$Metric <- "Revenues ($000s)"
Shipments$Metric <- "Shipments (000s)"
TV_installed_base$Metric <- "Installed base (000s)"
TV_households$Metric <- "TV households (000s)"
Domestic_Consumption$Metric <- "Domestic consumption (000s)"

Country_List$Year <- as.character(Country_List$Year)

Revenues$Country[grep(" Others", Revenues$Country)] <- "Others"
Revenues$Country[grep(" Other", Revenues$Country)] <- "Others"

Shipments$Country[grep(" Others", Shipments$Country)] <- "Others"
Shipments$Country[grep(" Other", Shipments$Country)] <- "Others"

TV_installed_base$Country[grep(" Others", TV_installed_base$Country)] <- "Others"
TV_installed_base$Country[grep(" Other", TV_installed_base$Country)] <- "Others"

TV_households$Country[grep(" Others", TV_households$Country)] <- "Others"
TV_households$Country[grep(" Other", TV_households$Country)] <- "Others"

Domestic_Consumption$Country[grep(" Others", Domestic_Consumption$Country)] <- "Others"
Domestic_Consumption$Country[grep(" Other", Domestic_Consumption$Country)] <- "Others"
#~~~~~~Cleaning End~~~~~~#


#~~~~~~Melt Start~~~~~~#
Revenues <-data.table(Revenues)
Revenues <- gather( Revenues,key = Year,value = Revenue,5:length(Revenues))
Revenues$Revenue <- as.numeric(as.character(Revenues$Revenue))

Shipments <-data.table(Shipments)
Shipments <- gather( Shipments,key = Year,value = Shipments,5:length(Shipments))

Domestic_Consumption <-data.table(Domestic_Consumption)
Domestic_Consumption <- gather( Domestic_Consumption,key = Year,value = Domestic_Consumption,5:length(Domestic_Consumption))

TV_installed_base <-data.table(TV_installed_base)
TV_installed_base <- gather( TV_installed_base,key = Year,value = TV_installed_base,5:length(TV_installed_base))

TV_households <-data.table(TV_households)
TV_households <- gather( TV_households,key = Year,value = TV_households,5:length(TV_households))

TV_installed_base_prev <- TV_installed_base
TV_installed_base_prev$Year <- as.numeric(as.character(TV_installed_base_prev$Year))+1
TV_installed_base_prev <- TV_installed_base_prev[!TV_installed_base_prev$Year=="2021",]
colnames(TV_installed_base_prev)[match("TV_installed_base",colnames(TV_installed_base_prev))] <- "Previous year Installed base (000s)"
TV_installed_base_prev$Year <- as.character(TV_installed_base_prev$Year)
#~~~~~~Melt End~~~~~~#


#~~~~~~Others Aggregate Start~~~~~~#
Country_List <- data.table(Country_List)
Revenues_others <- anti_join(Revenues,Country_List)
Revenues_others$Country <- "Others"

Shipments_others <- anti_join(Shipments,Country_List)
Shipments_others$Country <- "Others"

Domestic_Consumption_others <- anti_join(Domestic_Consumption,Country_List)
Domestic_Consumption_others$Country <- "Others"

TV_installed_base_others <- anti_join(TV_installed_base,Country_List)
TV_installed_base_others$Country <- "Others"

TV_households_others <- anti_join(TV_households,Country_List)
TV_households_others$Country <- "Others"

Revenues <- rbind.data.frame(Revenues,Revenues_others)
Shipments <- rbind.data.frame(Shipments,Shipments_others)
Domestic_Consumption <- rbind.data.frame(Domestic_Consumption,Domestic_Consumption_others)
TV_installed_base <- rbind.data.frame(TV_installed_base,TV_installed_base_others)
TV_households <- rbind.data.frame(TV_households,TV_households_others)
#~~~~~~Others Aggregate End~~~~~~#


#~~~~~~Main Aggregate Start~~~~~~#
Revenues <-data.table(Revenues)
Revenues <- Revenues[, list(`Revenues ($000s)` = sum(Revenue)), by =c("Region", "Country", "Year")] 

Shipments <-data.table(Shipments)
Shipments <- Shipments[, list(`Shipments (000s)` = sum(Shipments)), by =c("Region", "Country", "Year")] 

Domestic_Consumption <-data.table(Domestic_Consumption)
Domestic_Consumption <- Domestic_Consumption[, list(`Domestic consumption (000s)` = sum(Domestic_Consumption)), by =c("Region", "Country", "Year")] 

TV_installed_base <-data.table(TV_installed_base)
TV_installed_base <- TV_installed_base[, list(`Installed base (000s)` = sum(TV_installed_base)), by =c("Region", "Country", "Year")] 

TV_households <-data.table(TV_households)
TV_households <- TV_households[, list(`TV households (000s)` = sum(TV_households)), by =c("Region", "Country", "Year")] 

TV_installed_base_prev <-data.table(TV_installed_base_prev)
TV_installed_base_prev <- TV_installed_base_prev[, list(`Previous year Installed base (000s)` = sum(`Previous year Installed base (000s)` )), by =c("Region", "Country", "Year")] 
#~~~~~~Main Aggregate End~~~~~~#

                                                         
#~~~~~~Merge Start~~~~~~#    
rm(flat_table)
flat_table <-   inner_join(Domestic_Consumption,TV_households)
flat_table <-   inner_join(flat_table,TV_installed_base) 
flat_table <-   inner_join(flat_table,Shipments)      
flat_table <-   inner_join(flat_table,Revenues)
flat_table <-   left_join(flat_table,TV_installed_base_prev)
flat_table[is.na(flat_table)] <-   0
flat_table <-   left_join(Country_List,flat_table)
flat_table <- as.data.frame(flat_table)
#flat_table[,4:length(flat_table)] <- round(unlist(flat_table[,4:length(flat_table)]),digits = 0)
flat_table[,4:length(flat_table)] <- unlist(flat_table[,4:length(flat_table)])
flat_table$Year <- as.numeric(as.character(flat_table$Year))
#~~~~~~Merge End~~~~~~#                                                         
                                      
wb <- createWorkbook()
addWorksheet(wb,sheetName ="flat_data")
writeData(wb,              "flat_data", flat_table)
saveWorkbook(wb,"M:/Technology/DATA/TV_sets_model/Integration/TV Sets Domestic Consumption - Country Level Flat Data.xlsx", overwrite = T)
