if(is.na(match(c("devtools"),installed.packages()[,"Package"]))) install.packages(new.packages) else library(devtools)
suppressMessages(devtools::install_github("marcuskhl/BasicSettings"));suppressMessages(library(BasicSettings))

# If choose to output as .xlsx, need 16GB of RAM, dont bother with a 8GB laptop

#~~~~~~Function Loading Start~~~~~~#
# source("M:/Technology/DATA/Mobile_PCs/R_Codes/Common/case_insensitive_joins.R")
#~~~~~~Function Loading End~~~~~~#

#~~~~~~Read Files Start~~~~~~#
Price_data <- read.xlsx("M:/Technology/DATA/TV_sets_model/WW TV Price Tracker/Pricing_Complete.xlsx",
                        sheet = "Pricing", startRow = 1 , colNames = T,
                        rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                        rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

Model_data <- read.xlsx("M:/Technology/DATA/TV_sets_model/WW TV Price Tracker/Model_Complete.xlsx",
                        sheet = "Model", startRow = 1 , colNames = T,
                        rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                        rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
#~~~~~~Read files End~~~~~~#


#~~~~~~Data Clearning Start~~~~~~#
Price_data$Brand[Price_data$Brand=="Chang Hong" | Price_data$Brand=="CHANG HONG" | Price_data$Brand=="ChangHong" | Price_data$Brand=="CHANGHONG" ] <-"Changhong"
Model_data$Brand[Model_data$Brand=="Chang Hong" | Model_data$Brand=="CHANG HONG" | Model_data$Brand=="ChangHong" | Model_data$Brand=="CHANGHONG" ] <-"Changhong"

Model_data$Display[Model_data$Display =="QLED TV"] <- "OLED TV"
Model_data$Model <- gsub("  "," ", Model_data$Model, fixed = T)
Price_data$Model <- gsub("  "," ", Price_data$Model, fixed = T)
names(Model_data)[match("Location",names(Model_data))] <- "Region"

Price_data$`Price.($)` <- round(as.numeric(as.character(Price_data$`Price.($)`)), digits = 2)
Price_data$`Price.(Local)` <- round(as.numeric(as.character(Price_data$`Price.(Local)`)), digits = 2)
#~~~~~~Data Clearning End~~~~~~#



#~~~~~~Joining Start~~~~~~#


# Price_data$Region <- tolower(Price_data$Region)
# Price_data$Model <- tolower(Price_data$Model)
# Price_data$Brand <- tolower(Price_data$Brand)
# 
# Model_data$Region <- tolower(Model_data$Region)
# Model_data$Model <- tolower(Model_data$Model)
# Model_data$Brand <- tolower(Model_data$Brand)
Merged_data <- full_join(Price_data,Model_data, by = c("Region", "Model", "Brand"))
# Merged_data <- Merged_data[!is.na(Merged_data$`Price.($)`),]
# Merged_data <- merge(Price_data,Model_data, all.x = T,by = c("Region", "Model", "Brand"))

# intermediate_join_list <- insensitive(inner_join)(Price_data[,c(8,11,10)],Model_data[,1:4], by = list(x= c("Region", "Model", "Brand"), y=c("Region", "Model", "Brand"))) #just for checking
x <- insensitive.join(inner_join)(Price_data,Model_data, by = list(x= c("Region", "Model", "Brand"), y=c("Region", "Model", "Brand")))
# this is what the original Access DB is doing to join the two tables, Access is not case sensitive so i found this case insensitive join on the internet
# https://gist.github.com/jimhester/a060323a05b40c6ada34

x <- x[!is.na(x$`Price.($)`),]
x <- x[!is.na(x$Display),]

#~~~~~~Joining End~~~~~~#



#~~~~~~After Operation Cleaning Start~~~~~~#
y <- x[,match(c("Y", "Quarter", "Mon", "Region", "Country", "Brand", "Model", "Display", "Screen.Size", "Size.Group.(5-inch)",
                "Size.Group.(10-inch)", "Aspect.Ratio", "Brightness", "Display.Format", "Number.of.HDMI.Connectors", "CI+.module", 
                "Power.Consumption.(Watts)", "Backlight", "Refresh.Rate", "Number.of.USB", "Internet.Connectivity", "Ethernet.(LAN./.RJ-45)",
                "WiFi", "3D.Capable", "3D.Glasses", "Integrated.DVD.player", "Curved", "Platform", "OS", "Price.(Local)", "Price.($)"),names(x))]

names(y)[match("Y",names(y))] <- "Year"
names(y)[match("Mon",names(y))] <- "Month"
names(y) <- gsub(".", " ", names(y), fixed = T)
y$`Price ($)` <- format(y$`Price ($)`, scientific=F)
y$`Price (Local)` <- format(y$`Price (Local)`, scientific=F)
#~~~~~~After Operation Cleaning End~~~~~~#


# write.csv2(y, "M:/Technology/DATA/TV_sets_model/WW TV Price Tracker/Tracker Price Model Output/Pricing_Output.csv", row.names = F)
fwrite(y, "M:/Technology/DATA/TV_sets_model/WW TV Price Tracker/Tracker Price Model Output/Pricing_Output.csv", row.names = F) # experimental, only in beta version of data.table package, so not used

# wb <- createWorkbook()
# addWorksheet(wb,sheetName ="Output")
# writeData(wb, "Output", y)
# saveWorkbook(wb, "M:/Technology/DATA/TV_sets_model/WW TV Price Tracker/Pricing_Output.xlsx", overwrite = T)

