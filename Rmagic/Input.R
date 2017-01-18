#Input script

splitlist <- c("CRT","LCD","OLED","PDP","RP","SD","HD","FHD","UHD","CCFL","LED","60Hz",
              "120Hz","240Hz","Direct","Edge","3D","2D","Smart","Unsmart","Analog","Digital")
# Defined the list of splits to be used. Very important step!

#need to increment for 2021
source('M:/Technology/DATA/TV_sets_model/Integration/Rmagic/makeMatrix.R')
#Custom function used in this script that converts a dataframe to a suitable matrix.

#need to increment for 2021
years <-1990:2020
instyears <- years
years1 <- years[2:length(years)]
instyears1 <- years1


full_list<-read.csv("M:/Technology/DATA/TV_sets_model/Integration/inputs/countries_list.csv") 

region_list<-factor(full_list[,1]) 
# Defines a factor vector called region_list from the internal region column used in the full_list
# Allows the scale up to be performed using this vector as an index.

regions <- levels(region_list)

region_list2 <- sapply(region_list, as.character)
#Defines a second region_list over the 218 countries and turns it from a factor to a character vector.

countries <- factor(full_list[,3])

countries2 <- sapply(region_list, as.character)

subregion_list<-factor(full_list[,6]) 

subregions <- levels(subregion_list)

subregion_list2 <- sapply(subregion_list, as.character)

country_sales <- array(0,c(length(countries),length(years1),
                           length(splitlist)),dimnames=list(countries, years1,splitlist))


tv_households <- 
  makeMatrix(read.csv(
    paste("M:/Technology/DATA/TV_sets_model/Integration/inputs/basics",
          dir("M:/Technology/DATA/TV_sets_model/Integration/inputs/basics")[1],sep="/")
  ))
sets_per_hh <- 
  makeMatrix(read.csv(
    paste("M:/Technology/DATA/TV_sets_model/Integration/inputs/basics",
          dir("M:/Technology/DATA/TV_sets_model/Integration/inputs/basics")[2],sep="/")
  ))
mean_fail <- 
  makeMatrix(read.csv(
    paste("M:/Technology/DATA/TV_sets_model/Integration/inputs/basics",
          dir("M:/Technology/DATA/TV_sets_model/Integration/inputs/basics")[3],sep="/")
  ))
sd_fail <- 
  makeMatrix(read.csv(
    paste("M:/Technology/DATA/TV_sets_model/Integration/inputs/basics",
          dir("M:/Technology/DATA/TV_sets_model/Integration/inputs/basics")[4],sep="/")
  ))
buy_rate_mod <- 
  makeMatrix(read.csv(
    paste("M:/Technology/DATA/TV_sets_model/Integration/inputs/basics",
          dir("M:/Technology/DATA/TV_sets_model/Integration/inputs/basics")[5],sep="/")
  ))




country_sales <- array(0,dim=c(length(countries),length(years1),length(splitlist)),dimnames=list(countries,years1,splitlist))
for(i in 1:length(dir("M:/Technology/DATA/TV_sets_model/Integration/inputs/country_sales"))){
  country_sales[,,i] <- makeMatrix(read.csv(paste("M:/Technology/DATA/TV_sets_model/Integration/inputs/country_sales",
                                                  dir("M:/Technology/DATA/TV_sets_model/Integration/inputs/country_sales")[i],
                                                  sep="/")),yrs=years1)
}
# Sets country sales across the splits using the CSVs in this folder ordered to match the splitlist.

splitlist2 <- c("FPD","HD+","FHD+","120Hz+")

print("country sales imported")

print("Inputs completed")
source('M:/Technology/DATA/TV_sets_model/Integration/Rmagic/Process.R')
source('M:/Technology/DATA/TV_sets_model/Integration/Rmagic/Output.R')
