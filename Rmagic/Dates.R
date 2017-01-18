time_periods <- read.csv("Time_periods.csv")
j <- c(1:as.numeric(length(time_periods$Time_period)))
brightness_output <- data.frame()


for (i in j) {
  
  t_quarter <- time_periods$Quarter[i]
  t_year <- time_periods$Year[i]
  t_name <- as.character(time_periods$Time_period[i])
  
  
  brightness_working <- subset(all_brands, Year == t_year & Quarter == t_quarter, select = c(Product.Type, Technology, Size, Brightness, Year, Quarter, country_ship))
  
  brightness_working <- aggregate(brightness_working$country_ship,  
                                 by=list(brightness_working$Product.Type, brightness_working$Technology, brightness_working$Size,
                                                                  brightness_working$Brightness,
                                         brightness_working$Year, brightness_working$Quarter
                                                                  ), FUN = sum)
  
  
  colnames(brightness_working) <- c("Product.Type",
                                   "Technology", "Size",
                                   "Brightness",
                                   "Year", "Quarter", "Shipments")
  brightness_working <- merge(brightness_working, core)
  
  brightness_working$period_split <- brightness_working$Shipments / brightness_working$Core.Shipments
  
 brightness_working <- subset(brightness_working, select = c(Product.Type, Technology, Size, Brightness, period_split))
  
  names(brightness_working)[5] <- t_name
  
  if (i == 1) { brightness_output <-  brightness_working}
  
  else {brightness_output <- merge(brightness_output, brightness_working, all.x = TRUE, all.y = TRUE)}
  
  
}

write.csv(brightness_output, "brightness_output.csv")
