buyrates_dtech <- array(0,dim=c(length(countries),length(years1),length(techlist)),dimnames=list(countries,years1,techlist))

for(i in 1:length(techlist)){
  temp <- read.xlsx("C:/Users/jtb44363/Documents/Integration/Rmagic/buy_rates/dislpaytech_buyrates.xlsx",sheetIndex=i,colIndex=3:32,rowIndex=2:220)
# Need to makeMatrix first!!
buyrates_dtech[,,i] <- makeMatrix(temp, years=1991:2019)
}