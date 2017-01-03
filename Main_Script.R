if(is.na(match(c("devtools"),installed.packages()[,"Package"]))) install.packages(new.packages)
library(devtools)
suppressMessages(devtools::install_github("marcuskhl/BasicSettings"))
suppressMessages(library(BasicSettings))

