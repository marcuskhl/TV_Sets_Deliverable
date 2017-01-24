if(is.na(match(c("devtools"),installed.packages()[,"Package"]))) install.packages(new.packages) else library(devtools)
suppressMessages(devtools::install_github("marcuskhl/BasicSettings"));suppressMessages(library(BasicSettings))

options(digits = 3)
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
flat_data <- fread("M:/Technology/DATA/TV_sets_model/Integration/output_extract.csv")
flat_data <- as.df(flat_data)

#~~~~~~Cleaning Start~~~~~~#
flat_data <- flat_data[,-2] # remove row name
#flat_data <- flat_data[,-2] # remove sub region

flat_data <- left_join(flat_data,country_list)

flat_data <- column.rm(flat_data, "Country")
flat_data <- df.name.change(flat_data, "Old.country", "Country", F)



flat_data <- df.name.change(flat_data, c("households", "Installed", "Domestic", "Shipments", "Revenues"),
                            c("TV.households", "Installed.base", "Domestic.consumption", "Shipments", "Revenues"), T)
# names(flat_data)[grep("households", names(flat_data))] <- "TV.households"
# names(flat_data)[grep("Installed", names(flat_data))] <- "Installed.base"
# names(flat_data)[grep("Domestic", names(flat_data))] <- "Domestic.consumption"
# names(flat_data)[grep("Shipments", names(flat_data))] <- "Shipments"
# names(flat_data)[grep("Revenues", names(flat_data))] <- "Revenues"

# flat_data <- as.df(flat_data)
# selected_cols <- c("Region", "Country", "Submeasure")
# flat_data[,match(selected_cols,names(flat_data))] <- lapply(flat_data[,match(selected_cols,names(flat_data))], as.character)
# 
# selected_cols <- c("variable", "TV.households", "Installed.base", "Domestic.consumption", "Shipments", "Revenues")
# flat_data[,match(selected_cols,names(flat_data))] <- lapply(flat_data[,match(selected_cols,names(flat_data))], f2n)




flat_data <- as.dt(flat_data)
flat_data <- flat_data[, list(TV.households = sum(TV.households), Installed.base = sum(Installed.base), Domestic.consumption = sum(Domestic.consumption),
                              Shipments = sum(Shipments), Revenues = sum(Revenues)),
                       by = c("Region", "Country", "Measure", "Submeasure","variable")]
flat_data <- as.df(flat_data)

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
flat_data <- as.df(flat_data)
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

y = Reduce(function(...) dplyr::bind_rows(...
                                   #,by = c("variable","Region", "Country")   
                                   ), out)


for(i in 1:length(out)){
  colnames(y)[match(names(out)[i],colnames(y))] <- names_list[i]
}
y <- as.df(y)

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
TRAX <- y[,c(grep("Measure",names(y)),grep("Measure",names(y), invert = T))] # shuffle column names
#~~~~~~Big Table Cleaning End~~~~~~#





#~~~~~~Write Workbook Start~~~~~~#
save.xlsx("M:/Technology/DATA/TV_sets_model/Integration/TRAX/Upload This File.xlsx", TRAX)
#~~~~~~Write Workbook End~~~~~~#
options(digits = 15)