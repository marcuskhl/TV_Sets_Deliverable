#Input script
#library(xlsx)

# buy_rates <- array(0,dim=c(length(countries),length(years1),length(splitlist)),dimnames=list(countries,years1,splitlist))
# for(i in 1:length(dir("C:/Users/jtb44363/Documents/Integration/inputs/buy_rates"))){
#   buy_rates[,,i] <- makeMatrix(read.csv(paste("C:/Users/jtb44363/Documents/Integration/inputs/buy_rates",
#                                               dir("C:/Users/jtb44363/Documents/Integration/inputs/buy_rates")[i],
#                                               sep="/")),,yrs=years1)
# }

tv_households <- 
  makeMatrix(read.csv(
    paste("C:/Users/jtb44363/Documents/Integration/inputs/basics",
          dir("C:/Users/jtb44363/Documents/Integration/inputs/basics")[1],sep="/")
  ))
sets_per_hh <- 
  makeMatrix(read.csv(
    paste("C:/Users/jtb44363/Documents/Integration/inputs/basics",
          dir("C:/Users/jtb44363/Documents/Integration/inputs/basics")[2],sep="/")
  ))
mean_fail <- 
  makeMatrix(read.csv(
    paste("C:/Users/jtb44363/Documents/Integration/inputs/basics",
          dir("C:/Users/jtb44363/Documents/Integration/inputs/basics")[3],sep="/")
  ))
sd_fail <- 
  makeMatrix(read.csv(
    paste("C:/Users/jtb44363/Documents/Integration/inputs/basics",
          dir("C:/Users/jtb44363/Documents/Integration/inputs/basics")[4],sep="/")
  ))
buy_rate_mod <- 
  makeMatrix(read.csv(
    paste("C:/Users/jtb44363/Documents/Integration/inputs/basics",
          dir("C:/Users/jtb44363/Documents/Integration/inputs/basics")[5],sep="/")
  ))


full_list<-read.csv("C:/Users/jtb44363/Documents/Integration/inputs/countries_list.csv") 
# Imports full list of countries and their region assignments from this file linked to demand input model.
# Can hashtag out once full_list is part of environment to save on script load time.

# region_list<-factor(full_list[,1]) 
# # Defines a factor vector called region_list from the internal region column used in the full_list
# # Allows the scale up to be performed using this vector as an index.
# 
# regions <- levels(region_list)
# 
# region_list2 <- sapply(region_list, as.character)
# #Defines a second region_list over the 218 countries and turns it from a factor to a character vector.
# 
# region_sales <- array(0,dim=c(length(regions),length(years1),length(splitlist)),
#                        dimnames=list(regions,years1,splitlist))
# 
# for(i in 1:length(dir("C:/Users/jtb44363/Documents/Integration/inputs/region_totals"))){
#   region_sales[,,i] <- makeMatrix(read.csv(paste("C:/Users/jtb44363/Documents/Integration/inputs/region_totals",
#                                               dir("C:/Users/jtb44363/Documents/Integration/inputs/region_totals")[i],
#                                               sep="/")),rnames=regions,yrs=years1)
# }
# # Sets the region_totals across the splits using the CSVs in this folder ordered to match the splitlist.
# # Have to MAKE SURE i the makeMatrix function above that rnames=regions!
# 
# #ROR_list<-read.csv("C:/Users/jtb44363/Documents/Integration/inputs/rest_of_regions_list.csv") 
# # Does the same as above, but for "rest of regions", importing this from the Excel file. 
# 
# 
# ROR_list <-factor(full_list[,4]) 
# # Defines a factor vector called rest_of_regions_list from the relevant column used in the ROR_list
# # Allows the scale up to be performed using this vector as an index.
# 
# rest_of_regions <- levels(ROR_list)
# # Defines the rest of regions vector I need to use as a column name once importing for the relevant splits.
# 
# ROR_list2 <- sapply(ROR_list, as.character)
# # Defines a second rest of region ist over the 218 countries and turns it from a factor to a character vector.
# 
# ROR_sales <- array(0,dim=c(length(rest_of_regions),length(years1[23:24]),length(splitlist[1:5])),
#                       dimnames=list(rest_of_regions,years1[23:24],splitlist[1:5]))
# 
# for(i in 1:length(dir("C:/Users/jtb44363/Documents/Integration/inputs/rest_of_region_totals"))){
#   ROR_sales[,,i] <- makeMatrix(read.csv(paste("C:/Users/jtb44363/Documents/Integration/inputs/rest_of_region_totals",
#                                                  dir("C:/Users/jtb44363/Documents/Integration/inputs/rest_of_region_totals")[i],
#                                                  sep="/")),rnames=rest_of_regions,yrs=years1[23:24])
# }
# Sets the region_totals across the splits using the CSVs in this folder ordered to match the splitlist.
# Have to MAKE SURE i the makeMatrix function above that rnames=regions!

country_sales <- array(0,c(length(countries),length(years1),
                           length(splitlist)),dimnames=list(countries, years1,splitlist))

country_sales <- array(0,dim=c(length(countries),length(years1),length(splitlist)),dimnames=list(countries,years1,splitlist))
for(i in 1:length(dir("C:/Users/jtb44363/Documents/Integration/inputs/country_sales"))){
  country_sales[,,i] <- makeMatrix(read.csv(paste("C:/Users/jtb44363/Documents/Integration/inputs/country_sales",
                                                  dir("C:/Users/jtb44363/Documents/Integration/inputs/country_sales")[i],
                                                  sep="/")),,yrs=years1)
}
# Sets country sales across the splits using the CSVs in this folder ordered to match the splitlist.
print("country sales configured")

print("Inputs completed")
