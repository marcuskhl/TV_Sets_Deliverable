# Output script. Will output installed base, households and sales. 

#First to output households
library(gtools)

for(i in 1:length(splitlist)){
  metric <- rep("TV households (000s)",length(countries))
  splitvector <- rep(splitlist[i],length(countries))
  if(i==1){
    household_output <- data.frame(cbind(metric,region_list2,subregion_list2,countries,splitvector,data.frame(final_tv_households[,,i])))
  }
  else{
    household_output <- rbind(household_output,
                               data.frame(cbind(metric,region_list2,subregion_list2,countries,splitvector,data.frame(final_tv_households[,,i]))))
  }
  
}

print("household output ready.")

for(i in 1:length(splitlist)){
  metric <- rep("Installed base (000s)",length(countries))
  splitvector <- rep(splitlist[i],length(countries))
  if(i==1){
    installed_base_output <- data.frame(cbind(metric,region_list2,subregion_list2,countries,splitvector,data.frame(installed_base[,,i])))
  }
  else{
    installed_base_output <- rbind(installed_base_output,
                              data.frame(cbind(metric,region_list2,subregion_list2,countries,splitvector,data.frame(installed_base[,,i]))))
  }
  
}

print("Installed base output ready.")

for(i in 1:length(splitlist)){
  metric <- rep("Domestic consumption (000s)",length(countries))
  splitvector <- rep(splitlist[i],length(countries))
  if(i==1){
    country_sales_output <- data.frame(cbind(metric,region_list2,subregion_list2,countries,splitvector,data.frame(country_sales[,,i])))
  }
  else{
    country_sales_output <- rbind(country_sales_output,
                                   data.frame(cbind(metric,region_list2,subregion_list2,countries,splitvector,data.frame(country_sales[,,i]))))
  }
  
}

print("Domestic consumption output ready.")

output <- smartbind(household_output,installed_base_output,country_sales_output) 
output[is.na(output)] <- 0

write.csv(output,'M:/Technology/DATA/TV_sets_model/Integration/output.csv')

print("Output produced.")