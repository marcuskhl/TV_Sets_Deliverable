list.of.packages <- c("openxlsx", "reshape2",  "plyr","dplyr","data.table","tidyr","sqldf", "zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){ install.packages(new.packages)}
lapply(list.of.packages, library, character.only = TRUE)
cat("\014") 


# .d8888b.  888                                               8888888888                                        Y88b   d88P                        
# d88P  Y88b 888                                               888                                                Y88b d88P                         
# 888    888 888                                               888                                                 Y88o88P                          
# 888        88888b.   8888b.  88888b.   .d88b.   .d88b.       8888888   888  888  .d88b.  888d888 888  888         Y888P  .d88b.   8888b.  888d888 
# 888        888 "88b     "88b 888 "88b d88P"88b d8P  Y8b      888       888  888 d8P  Y8b 888P"   888  888          888  d8P  Y8b     "88b 888P"   
# 888    888 888  888 .d888888 888  888 888  888 88888888      888       Y88  88P 88888888 888     888  888          888  88888888 .d888888 888     
# Y88b  d88P 888  888 888  888 888  888 Y88b 888 Y8b.          888        Y8bd8P  Y8b.     888     Y88b 888          888  Y8b.     888  888 888     
#  "Y8888P"  888  888 "Y888888 888  888  "Y88888  "Y8888       8888888888  Y88P    "Y8888  888      "Y88888          888   "Y8888  "Y888888 888     
#                                            888                                                        888                                         
#                                       Y8b d88P                                                   Y8b d88P                                         
#                                        "Y88P"                                                     "Y88P"                                          
#actual_year <- 2015 #next change is Q1 2017

# fk it, better work it out automatically
yq <- as.yearqtr(Sys.Date(), format = "%Y-%m-%d") 
current_year <- substr(yq, 1,4)

actual_year <- as.numeric(as.character(current_year))-1


column_header <- read.xlsx("M:/Technology/DATA/TV_sets_model/Integration/TRAX/TV Set Domestic Consumption by Feature_TRAX.xlsx", 
                          sheet = "Sheet1", startRow = 1 , colNames = TRUE,
                          rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                          rows = 1:2, cols = NULL, check.names = FALSE, namedRegion = NULL)

measure_list <- read.xlsx("M:/Technology/DATA/TV_sets_model/Integration/TRAX/TV Set Domestic Consumption by Feature_TRAX.xlsx", 
                          sheet = "Measure_list", startRow = 1 , colNames = TRUE,
                          rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                          rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

country_list <- read.xlsx("M:/Technology/DATA/TV_sets_model/Integration/TRAX/TV Set Domestic Consumption by Feature_TRAX.xlsx", 
                           sheet = "Country_list", startRow = 1 , colNames = TRUE,
                           rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                           rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

HH <- read.xlsx("M:/Technology/DATA/TV_sets_model/Integration/TRAX/TV Set Domestic Consumption by Feature_TRAX.xlsx", 
                          sheet = "HH", startRow = 1 , colNames = TRUE,
                          rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                          rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

# output of Pivot_output.R
flat_data <- read.csv2("M:/Technology/DATA/TV_sets_model/Integration/output_extract.csv", sep = ",")


#~~~~~~Cleaning Start~~~~~~#
flat_data <- flat_data[,-1] # remove row name
flat_data <- flat_data[,-2] # remove sub region

flat_data <- left_join(flat_data,country_list)

flat_data <- flat_data[,-match("Country", names(flat_data))]
names(flat_data)[match("Old.country", names(flat_data))] <- "Country"

names(flat_data)[grep("households", names(flat_data))] <- "TV.households"
names(flat_data)[grep("Installed", names(flat_data))] <- "Installed.base"
names(flat_data)[grep("Domestic", names(flat_data))] <- "Domestic.consumption"
names(flat_data)[grep("Shipments", names(flat_data))] <- "Shipments"
names(flat_data)[grep("Revenues", names(flat_data))] <- "Revenues"


flat_data$Region <- as.character(flat_data$Region)
flat_data$Country <- as.character(flat_data$Country)
flat_data$Submeasure <- as.character(flat_data$Submeasure)
flat_data$variable <- as.numeric(as.character(flat_data$variable))
flat_data$TV.households <- as.numeric(as.character(flat_data$TV.households))
flat_data$Installed.base <- as.numeric(as.character(flat_data$Installed.base))
flat_data$Domestic.consumption <- as.numeric(as.character(flat_data$Domestic.consumption))
flat_data$Shipments <- as.numeric(as.character(flat_data$Shipments))
flat_data$Revenues <- as.numeric(as.character(flat_data$Revenues))





  
flat_data <- data.table(flat_data)


flat_data <- flat_data[, list(TV.households = sum(TV.households), Installed.base = sum(Installed.base), Domestic.consumption = sum(Domestic.consumption),
                              Shipments = sum(Shipments), Revenues = sum(Revenues)),
                       by = c("Region", "Country", "Measure", "Submeasure","variable")]

                                                         
flat_data <- data.frame(flat_data)

prev_year_IB <- flat_data[,c(1:5,grep("Installed", names(flat_data)))] #sloppy way to find columns
names(prev_year_IB)[length(prev_year_IB)] <- "prev_year_IB"
prev_year_IB$variable <- prev_year_IB$variable + 1 # so that it will merge to next year when joining it back to the main table

flat_data <- left_join(flat_data, prev_year_IB)
flat_data$prev_year_IB[is.na(flat_data$prev_year_IB)] <- 0

flat_data <- flat_data[flat_data$variable>2006,]
names(flat_data)[match("variable", names(flat_data))] <- "Year"
#~~~~~~Cleaning end~~~~~~#



#~~~~~~Finding Net Addition & Replacement~~~~~~#
# Replacements & Net Add are calculated fields
# have to be calculated unless I want to read from the deliverable file
flat_data$Measure <- as.character(flat_data$Measure)

flat_data$Net_Addition <- 0
flat_data$Net_Addition[flat_data$Installed.base-flat_data$prev_year_IB!=0] <- flat_data$Installed.base[flat_data$Installed.base-flat_data$prev_year_IB!=0] -
  flat_data$prev_year_IB[flat_data$Installed.base-flat_data$prev_year_IB!=0] 

flat_data$Replacement <- 0 
flat_data$Replacement[flat_data$Net_Addition<0] <- flat_data$Domestic.consumption[flat_data$Net_Addition<0]
flat_data$Replacement[flat_data$Net_Addition>0] <- flat_data$Domestic.consumption[flat_data$Net_Addition>0] -
  flat_data$Net_Addition[flat_data$Net_Addition>0]

#~~~~~~Reshaping Start~~~~~~#
flat_data <- data.table(flat_data)
x <- melt.data.table(flat_data, id.vars = c("Region","Country","Measure","Submeasure","Year"))
names(x)[length(x)-1] <- "Measure"
#~~~~~~Reshaping Start~~~~~~#

#x <- dcast(data = x, variable+Region+Country+Measure~Submeasure+Year, value.var = "value") 

names_list <- c("Capability-3D", "Backlight","Connectable","Display Technology","Frame Rate","LED Type","Resolution format","Smart TV","Tuner")
out <- split( x , f = x$Measure)
for(i in 1:length(out)){
  temp_table <- out[[i]] 
  temp_table <-data.frame(temp_table)
  temp_table <- temp_table[,-3]
  temp_table <- dcast(data = temp_table, ...~Year, value.var = "value", fun.aggregate = sum) 
  colnames(temp_table)[3]<- paste(names(out)[i])
  temp_table <-data.table(temp_table)
  out[[i]] <- temp_table
}

y = Reduce(function(...) rbind.fill(...
                                   #,by = c("variable","Region", "Country")   
                                   ), out)


for(i in 1:length(out)){
  colnames(y)[match(names(out)[i],colnames(y))] <- names_list[i]
}

#~~~~~~ remove Tuner, LED Type & Connectable Start~~~~~~#
y <- y[!is.na(y$`Display Technology`)|!is.na(y$`Resolution format`)|!is.na(y$`Capability-3D`)|!is.na(y$Backlight)|!is.na(y$`Smart TV`)|!is.na(y$`Frame Rate`),]

y <- y[,-match(c("Tuner", "LED Type", "Connectable"),colnames(y))]

colnames(y)[match("Measure.1",colnames(y))] <- "Measure"
#~~~~~~ remove Tuner, LED Type & Connectable End~~~~~~#



#~~~~~~Big Table Cleaning Start~~~~~~#
y$Measure <- as.character(y$Measure)
y <- y[!y$Measure=="prev_year_IB",]
y$Measure[grep("Revenues",y$Measure)] <- measure_list[7,1]
# no revenue for Smart, 3D, Backlight & Frame Rate
y <- y[!(y$Measure==measure_list[7,1] & (!is.na(y$Backlight)|!is.na(y$`Capability-3D`)|!is.na(y$`Smart TV`)|!is.na(y$`Frame Rate`))),]

backlight_framerate <- y[(!is.na(y$Backlight)|!is.na(y$`Frame Rate`)),] 
y <- y[(is.na(y$Backlight)&is.na(y$`Frame Rate`)),] 

y$Measure[grep("Domestic",y$Measure)] <- measure_list[1,1]
y$Measure[grep("households",y$Measure)] <- measure_list[2,1]
y$Measure[grep("Installed",y$Measure)] <- measure_list[3,1]
y$Measure[grep("Replacement",y$Measure)] <- measure_list[4,1]
y$Measure[grep("Net",y$Measure)] <- measure_list[5,1]
y$Measure[grep("Shipments",y$Measure)] <- measure_list[6,1]



# Frame Rate and Backlight uses only LCD numbers
backlight_framerate$Measure[grep("Domestic",backlight_framerate$Measure)] <- measure_list[8,1]
backlight_framerate$Measure[grep("households",backlight_framerate$Measure)] <- measure_list[9,1]
backlight_framerate$Measure[grep("Installed",backlight_framerate$Measure)] <- measure_list[10,1]
backlight_framerate$Measure[grep("Replacement",backlight_framerate$Measure)] <- measure_list[11,1]
backlight_framerate$Measure[grep("Net",backlight_framerate$Measure)] <- measure_list[12,1]
backlight_framerate$Measure[grep("Shipments",backlight_framerate$Measure)] <- measure_list[13,1]


y <- rbind.data.frame(y, backlight_framerate)


y<- y[,c(grep("20",names(y), invert = T),grep("20",names(y)))] # shuffle column names

#easier to do in for loop
for (i in 1 : length(names(y)[grep("20",names(y))])){
  if(as.numeric(names(y)[grep("20",names(y))][i])>actual_year){
    names(y)[grep("20",names(y))][i] <- paste(names(y)[grep("20",names(y))][i],"(F)",sep=" ")
    }
}
names(y)[grep("20",names(y))] <- paste("Y",names(y)[grep("20",names(y))],sep="")
y<- y[,c(grep("Measure",names(y)),grep("Measure",names(y), invert = T))] # shuffle column names
#~~~~~~Big Table Cleaning End~~~~~~#





#~~~~~~Write Workbook Start~~~~~~#
wb <- createWorkbook()
addWorksheet(wb,sheetName ="TRAX")
writeData(wb, "TRAX", y)
saveWorkbook(wb, "M:/Technology/DATA/TV_sets_model/Integration/TRAX/Upload This File.xlsx", overwrite = T)

wb1 <- createWorkbook()

#~~~~~~Write Workbook End~~~~~~#

































# o.     O                 o          `O              o                     
# Oo     o                 O           o              O     o               
# O O    O        O        o           O              o                     
# O  o   o       oOo       O           O              o                     
# O   o  O .oOo.  o        o     o     o .oOo. `OoOo. O  o  O  'OoOo. .oOoO 
# o    O O O   o  O        O     O     O O   o  o     OoO   o   o   O o   O 
# o     Oo o   O  o        `o   O o   O' o   O  O     o  O  O   O   o O   o 
# O     `o `OoO'  `oO       `OoO' `OoO'  `OoO'  o     O   o o'  o   O `OoOo 
#                                                                         O 
#                                                                      OoO' 

# cannot get it to clean the measure column

# y$Backlight <- as.character(y$Backlight)
# y$`Frame Rate` <- as.character(y$`Frame Rate`)
# y$Measure <- as.character(y$Measure)
# y$Measure <- gsub("."," ", )
# y$Measure[!is.na(y$Backlight) & !is.na(y$`Frame Rate`)] <- gsub("TV.households..000s.", "LCD TV Households - Units (K)",y$Measure[!is.na(y$Backlight) | !is.na(y$`Frame Rate`)],fixed = T)
# y$Measure[!is.na(y$Backlight) & !is.na(y$`Frame Rate`)] <- gsub("Installed.base..000s.", "LCD TV Installed base - Units (K)",y$Measure[!is.na(y$Backlight) | !is.na(y$`Frame Rate`)],fixed = T)
# y$Measure[!is.na(y$Backlight) & !is.na(y$`Frame Rate`)] <- gsub("Domestic.consumption..000s.", "LCD TV Domestic Consumption - Units (K)",y$Measure[!is.na(y$Backlight) | !is.na(y$`Frame Rate`)],fixed = T)
# y$Measure[!is.na(y$Backlight) & !is.na(y$`Frame Rate`)] <- gsub("Shipments..000s.", "LCD TV Shipments - Units (K)",y$Measure[!is.na(y$Backlight) | !is.na(y$`Frame Rate`)],fixed = T)
# z <- y[!(!is.na(y$Backlight) & !is.na(y$`Frame Rate`)& y$Measure=="Revenues...m."),]
# 


# old code:
# 
#          888                                                                                                                                   888                    
#          888                                                                                                                                   888                    
#          888                                                                                                                                   888                    
#  .d8888b 88888b.   8888b.  88888b.   .d88b.   .d88b.        .d88b.  888  888  .d88b.  888d888 888  888       .d88888 888  888  8888b.  888d888 888888 .d88b.  888d888 
# d88P"    888 "88b     "88b 888 "88b d88P"88b d8P  Y8b      d8P  Y8b 888  888 d8P  Y8b 888P"   888  888      d88" 888 888  888     "88b 888P"   888   d8P  Y8b 888P"   
# 888      888  888 .d888888 888  888 888  888 88888888      88888888 Y88  88P 88888888 888     888  888      888  888 888  888 .d888888 888     888   88888888 888     
# Y88b.    888  888 888  888 888  888 Y88b 888 Y8b.          Y8b.      Y8bd8P  Y8b.     888     Y88b 888      Y88b 888 Y88b 888 888  888 888     Y88b. Y8b.     888     
#  "Y8888P 888  888 "Y888888 888  888  "Y88888  "Y8888        "Y8888    Y88P    "Y8888  888      "Y88888       "Y88888  "Y88888 "Y888888 888      "Y888 "Y8888  888     
#                                          888                                                       888           888                                                  
#                                     Y8b d88P                                                  Y8b d88P           888                                                  
#                                      "Y88P"                                                    "Y88P"            888       
# Change the file name
# filename = "IHS-2016-Q3- TV Sets Domestic Consumption by Feature - Country Level 19 Oct"
# filepath = paste("M:/Technology/DATA/TV_sets_model/Integration/",filename,".xlsx",sep = "")
# Display_data <- read.xlsx(filepath, 
#                           sheet = "Display_data", startRow = 1 , colNames = TRUE,
#                           rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
#                           rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
# 
# Resolution_Format_data <- read.xlsx(filepath, 
#                                     sheet = "Resolution_Format_data", startRow = 1 , colNames = TRUE,
#                                     rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
#                                     rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
# 
# Smart_data <- read.xlsx(filepath, 
#                         sheet = "Smart_data", startRow = 1 , colNames = TRUE,
#                         rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
#                         rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
# 
# # Connected_rate_data <- read.xlsx(filepath, 
# #                           sheet = "Connected_rate_data", startRow = 1 , colNames = TRUE,
# #                           rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
# #                           rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
# 
# ThreeD_data <- read.xlsx(filepath, 
#                          sheet = "3D_data", startRow = 1 , colNames = TRUE,
#                          rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
#                          rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
# 
# Backlight_data <- read.xlsx(filepath, 
#                             sheet = "Backlight_data", startRow = 1 , colNames = TRUE,
#                             rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
#                             rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
# 
# Frame_Rate_data <- read.xlsx(filepath, 
#                              sheet = "Frame_Rate_data", startRow = 1 , colNames = TRUE,
#                              rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
#                              rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
# 
# Display_data <- Display_data[,1:10]
# Resolution_Format_data <- Resolution_Format_data[,1:10]
# Smart_data <- Smart_data[,1:9] # no revenue
# ThreeD_data <- ThreeD_data[,1:9] # no revenue
# Backlight_data <- Backlight_data[,1:9] # no revenue
# Frame_Rate_data <- Frame_Rate_data[,1:9] # no revenue
# 
# names(ThreeD_data)[1] <- "ThreeD_data" # cant start variable names with a number
# 
# Display_data_flat <- Display_data %>% gather(Measure, Value,-Display.Technology,-Region, -Country,-Year)
# Resolution_Format_data_flat <- Resolution_Format_data %>% gather(Measure, Value,-Resolution.Format,-Region, -Country,-Year)
# Smart_data_flat <- Smart_data %>% gather(Measure, Value,-Smart.capability,-Region, -Country,-Year)
# ThreeD_data_flat <- ThreeD_data %>% gather(Measure, Value,-ThreeD_data,-Region, -Country,-Year)
# Backlight_data_flat <- Backlight_data %>% gather(Measure, Value,-Backlight,-Region, -Country,-Year)
# Frame_Rate_data_flat <- Frame_Rate_data %>% gather(Measure, Value,-Frame.Rate,-Region, -Country,-Year)
# #~~~~~~Cleaning End~~~~~~#
# 
# 
# 
# #~~~~~~Assigning TRAX Measure Names Start~~~~~~#
# # Frame Rate and Backlight uses only LCD numbers
# # Replacements & Net Add are calculated fields
# Display_data_flat$Measure[grep("Domestic",Display_data_flat$Measure)] <- measure_list[1,1]
# Display_data_flat$Measure[grep("households",Display_data_flat$Measure)] <- measure_list[2,1]
# Display_data_flat$Measure[grep("Installed",Display_data_flat$Measure)] <- measure_list[3,1]
# Display_data_flat$Measure[grep("Shipments",Display_data_flat$Measure)] <- measure_list[6,1]
# Display_data_flat$Measure[grep("Previous",Display_data_flat$Measure)] <- "Previous_Year_IB"
# 
# 
# Resolution_Format_data_flat$Measure[grep("Domestic",Resolution_Format_data_flat$Measure)] <- measure_list[1,1]
# Resolution_Format_data_flat$Measure[grep("households",Resolution_Format_data_flat$Measure)] <- measure_list[2,1]
# Resolution_Format_data_flat$Measure[grep("Installed",Resolution_Format_data_flat$Measure)] <- measure_list[3,1]
# Resolution_Format_data_flat$Measure[grep("Shipments",Resolution_Format_data_flat$Measure)] <- measure_list[6,1]
# Resolution_Format_data_flat$Measure[grep("Previous",Resolution_Format_data_flat$Measure)] <- "Previous_Year_IB"


#~~~~~~Assigning TRAX Measure Names End~~~~~~#
