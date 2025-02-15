if(is.na(match(c("devtools"),installed.packages()[,"Package"]))) install.packages(new.packages) else library(devtools)
suppressMessages(devtools::install_github("marcuskhl/BasicSettings"));suppressMessages(library(BasicSettings))



#~~~~~~Notes~~~~~~#
# # of sheets need to produce: 
# From Output_extract.csv
# 1) HH_Pen_data: 2007 - 2020 a) Households; b) TV Households; c) UHD TV Households
# 2) Display_data: 2007-2020 a) TV HH; b) IB; c) Dom Con; d) Shipments; e) Rev; f) Prev Year IB
# 3) Resolution_Format_data: 2007 - 2020 a) TV HH; b) IB; c) Dom Con; d) Shipments; e) Rev; f) Prev Year IB
# 4) Smart_data: 2007-2020 a) TV HH; b) IB; c) Dom con; d) Shipments; e) Prev Year IB
# 5) Connected_rate_data: 2007 - 2020 a) Smart TV IB (smart-connected & smart-unconnected)
# 6) 3D_data: 2007 - 2020 a) TV HH; b) IB; c) Dom Con; d) Shipments; e) Prev Year IB
# 7) Backlight_data: 2007-2020  a) TV HH; b) IB; c) Dom Con; d) Shipments; e) Prev Year IB
# 8) Frame_Rate_data: 2007 - 2020 a) TV HH; b) IB; c) Dom Con; d) Shipments; e) Prev Year IB
#~~~~~~Notes~~~~~~#


final_year <- 2020
#~~~Read Input Start~~~#
country_list <- read.xlsx("M:/Technology/DATA/TV_sets_model/Integration/TRAX/TV Set Domestic Consumption by Feature_TRAX.xlsx", 
                          sheet = "Country_list", startRow = 1 , colNames = TRUE,
                          rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                          rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

HH <- read.xlsx("M:/Technology/DATA/TV_sets_model/Integration/TRAX/TV Set Domestic Consumption by Feature_TRAX.xlsx", 
                sheet = "HH", startRow = 1 , colNames = TRUE,
                rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

flat_data <- fread("M:/Technology/DATA/TV_sets_model/Integration/output_extract.csv") # data table

MS_flat_data <- fread("M:/Technology/DATA/TV_sets_model/Integration/extract.csv") # data table
#~~~Read Input End~~~#



#~~~Data Cleaning Start~~~#

#~~~HH Start~~~#
HH <- left_join(HH,country_list)
HH <- column.rm(HH, "Country")
names(HH)[match("Old.country", names(HH))] <- "Country"
HH <- as.dt(HH)

HH <- HH[order(Measure, Region, Country)]
HH_flat <- HH %>%
  gather(Year, Households,`2007`:`2020`)
HH_flat$Year <- f2n(HH_flat$Year)
HH_flat <- HH_flat[!(HH_flat$Region=="North America" & HH_flat$Country=="Others"),]
HH_flat <- as.dt(HH_flat)
HH_flat <- HH_flat[, list(Households = sum(Households)),
                       by = c("Measure", "Region", "Country", "Year")]
out <- split(HH_flat, HH_flat$Measure)
HH_1 <- as.df(out[[1]])
HH_2 <- as.df(out[[2]])
names(HH_1)[match("Households", names(HH_1))] <- names(out)[[1]]
names(HH_2)[match("Households", names(HH_2))] <- names(out)[[2]]
HH_1 <- HH_1[,-1]
HH_2 <- HH_2[,-1]
HH_out <- left_join(HH_1,HH_2) # this is not final, still need to attach UHD HH
HH_out$Total_HH <- HH_out$Total_HH * 1000
HH_out$TV_HH <- HH_out$TV_HH * 1000
#~~~HH End~~~#


#~~~Flat Data Start~~~#
flat.data.cleaning <- function(flat_data){
  flat_data <- flat_data[,c("V1","Sub.region"):=NULL] # remove row name
  flat_data <- as.df(flat_data)
  flat_data <- left_join(flat_data,country_list)
  flat_data <- column.rm(flat_data, "Country")
  names(flat_data)[match("Old.country", names(flat_data))] <- "Country"
  flat_data <- as.dt(flat_data)
  flat_data <- flat_data[, list(TV.households = sum(`TV households (000s)`), Installed.base = sum(`Installed base (000s)`), Domestic.consumption = sum(`Domestic consumption (000s)`),
                                Shipments = sum(`Shipments (000s)`), Revenues = sum(`Revenues ($m)`)),
                         by = c("Region", "Country", "Measure", "Submeasure","variable")]
  
  flat_data$variable <- as.numeric(as.character(flat_data$variable))
  return(flat_data)
}

flat_data <- flat.data.cleaning(flat_data)

flat_data <- as.df(flat_data)
prev_year_IB <- flat_data[,c(1:5,grep("Installed", names(flat_data)))] #sloppy way to find columns
names(prev_year_IB)[length(prev_year_IB)] <- "prev_year_IB"
prev_year_IB$variable <- prev_year_IB$variable + 1 # so that it will merge to next year when joining it back to the main table
flat_data <- as.dt(flat_data)
prev_year_IB <- as.dt(prev_year_IB)

flat_data <- left_join(flat_data, prev_year_IB)
flat_data$prev_year_IB[is.na(flat_data$prev_year_IB)] <- 0

flat_data <- flat_data[flat_data$variable>2006,]
names(flat_data)[match("variable", names(flat_data))] <- "Year"

flat_data <- flat_data[!(flat_data$Region=="North America" & flat_data$Country=="Others"),]


flat_data <- as.dt(flat_data)

#flat_data <- flat_data[order(Measure, Submeasure, Region, Country, Year)]
#~~~Flat Data End~~~#

#~~~MS Flat Data Start~~~#
MS_flat_data <- MS_flat_data[,c("V1"):=NULL] # remove row name
MS_flat_data <- as.df(MS_flat_data)
MS_flat_data <- left_join(MS_flat_data,country_list)
MS_flat_data <- column.rm(MS_flat_data, "Country")
names(MS_flat_data)[match("Old.country", names(MS_flat_data))] <- "Country"
MS_flat_data <- as.dt(MS_flat_data)
MS_flat_data <- MS_flat_data[, list(Installed.base = sum(`Installed base (000s)`), 
                              Shipments = sum(`Shipments (000s)`)),
                       by = c("TV.Type", "Region", "Country", "Vendor","variable")]
MS_flat_data <- df.name.change(MS_flat_data, "variable", "Year", F)
MS_flat_data$Year <- f2n(MS_flat_data$Year)
#~~~MS Flat Data End~~~#
#~~~Data Cleaning End~~~#



#~~~~~~Finding Net Addition & Replacement Start~~~~~~#
# Replacements & Net Add are calculated fields
# have to be calculated unless I want to read from the deliverable file
# currently done in pivot not in R code
# flat_data$Measure <- as.character(flat_data$Measure)
# 
# flat_data$Net_Addition <- 0
# flat_data$Net_Addition[flat_data$Installed.base-flat_data$prev_year_IB!=0] <- flat_data$Installed.base[flat_data$Installed.base-flat_data$prev_year_IB!=0] -
#   flat_data$prev_year_IB[flat_data$Installed.base-flat_data$prev_year_IB!=0] 
# 
# flat_data$Replacement <- 0 
# flat_data$Replacement[flat_data$Net_Addition<0] <- flat_data$Domestic.consumption[flat_data$Net_Addition<0]
# flat_data$Replacement[flat_data$Net_Addition>0] <- flat_data$Domestic.consumption[flat_data$Net_Addition>0] -
#   flat_data$Net_Addition[flat_data$Net_Addition>0]
#~~~~~~Finding Net Addition & Replacement Start End~~~~~~#



#~~~~~~Excel Deliverable Start~~~~~~#
out <- split(flat_data, flat_data$Measure)
for (i in 1: length(names(out))){
  # print(names(out)[i])
  temp_df <- out[[i]]
  temp_df <- as.df(temp_df)
  names(temp_df)[3] <- temp_df$Measure[1]
  out[[i]] <- temp_df
}

# x <- Reduce(function(...) dplyr::bind_rows(...
#                                   ), out) # for TRAX if decide to combine the two scripts tgt



TV.Sets.Production <- function(Measure_Match, df_list) { # Measure match is the name of the df in the list not exactly the colname
  df_list <- out
  temp_df <- df_list[[match(Measure_Match, names(df_list))]]
  temp_df <- temp_df[,-3]
  temp_df <- temp_df[,c(grep("Submeasure", names(temp_df)), grep("Submeasure", names(temp_df), invert = T))]
  names(temp_df)[1] <- Measure_Match
  return(temp_df)
}



#~~~(2) Display_data Start~~~#
Display_data <- TV.Sets.Production("Display Tech",out)
#~~~(2) Display_data End~~~#



#~~~(3) Resolution_format_data Start~~~#
Resolution_format_data <- TV.Sets.Production("Resolution format",out)
#~~~(3) Resolution_format_data End~~~#



#~~~(1) HH_Pen_data Start~~~#
temp_df <- Resolution_format_data[Resolution_format_data$`Resolution format`=="UHD",1:5]
names(temp_df)[length(names(temp_df))] <- "UHD TV households (000s)"
temp_df <- column.rm(temp_df, "Resolution format")
HH_pen_data <- left_join(HH_out,temp_df)
#~~~(1) HH_Pen_data End~~~#



#~~~(4) Smart_data Start~~~#Smart capability
Smart_data <- TV.Sets.Production("Smart",out)
Smart_data <- column.rm(Smart_data, "Revenues")
#~~~(4) Smart_data End~~~#



#~~~(5) Connected_rate_data Start~~~#
Connected_rate_data <- TV.Sets.Production("Connectable",out)
Connected_rate_data <- Connected_rate_data[,c(1:4,grep("Installed.base",names(Connected_rate_data)))]
Connected_rate_data$Connectable[Connected_rate_data$Connectable == "Connected"] <- "Smart - connected"
Connected_rate_data$Connectable[Connected_rate_data$Connectable == "Unconnected"] <- "Smart - unconnected"
#~~~(3) Resolution_format_data End~~~#



#~~~(6) 3D_data Start~~~#
`3D_data` <- TV.Sets.Production("3D",out)
`3D_data` <- column.rm(`3D_data`, "Revenues")
#~~~(6) 3D_data End~~~#



#~~~(7) Backlight_data Start~~~#
Backlight_data <- TV.Sets.Production("Backlight",out)
#~~~(7) Backlight_data End~~~#



#~~~backlight_xtra Start~~~#
backlight_xtra <- TV.Sets.Production("LED Type",out)
#~~~backlight_xtra End~~~#



#~~~Backlight_data Processing Start~~~#
Backlight_data <- Backlight_data[!Backlight_data$Backlight== "LED",]
backlight_xtra$`LED Type`[backlight_xtra$`LED Type`=="Direct"] <- "D-LED"
backlight_xtra$`LED Type`[backlight_xtra$`LED Type`=="Edge"] <- "E-LED"
names(backlight_xtra) <- names(Backlight_data)
Backlight_data <- rbind.data.frame(Backlight_data, backlight_xtra)
Backlight_data <- column.rm(Backlight_data, "Revenues")
#~~~Backlight_data Processing End~~~#



#~~~(8) Frame_Rate_data Start~~~#
Frame_Rate_data <- TV.Sets.Production("Frame Rate",out)
Frame_Rate_data <- column.rm(Frame_Rate_data, "Revenues")
#~~~(8) Frame_Rate_data End~~~#



#~~~(9) TV_MS_data Start~~~#
TV_MS_data <- MS_flat_data[MS_flat_data$TV.Type=="All",]
TV_MS_data <- column.rm(TV_MS_data, c("TV.Type","Installed.base"))
#~~~(9) TV_MS_data End~~~#

#~~~(10) Smart_TV_MS_data Start~~~#
Smart_TV_MS_data <- MS_flat_data[!MS_flat_data$TV.Type=="All",]
Smart_TV_MS_data <- as.df(Smart_TV_MS_data)
Smart_TV_MS_data <- Smart_TV_MS_data[,c(grep("Installed",names(Smart_TV_MS_data), invert = T),grep("Installed",names(Smart_TV_MS_data)))]
Smart_TV_MS_data <- as.dt(Smart_TV_MS_data)
#~~~(10) Smart_TV_MS_data End~~~#




#~~~Write Data Start~~~#
# could do a rodbc but that would mean more invasive changes to the deliverable file
save.xlsx("M:/Technology/DATA/TV_sets_model/Integration/TV Sets Domestic Consumption by Feature - Country Level DATA.xlsx",
          HH_pen_data, Display_data, Resolution_format_data, Smart_data, Connected_rate_data, `3D_data`, Backlight_data, Frame_Rate_data, TV_MS_data, Smart_TV_MS_data)
#~~~Write Data End~~~#