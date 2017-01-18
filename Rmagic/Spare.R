# Process script. Here will loop through and do all calculations


tv_sets <- sets_per_hh*tv_households

net_additions <- tv_sets[,2:ncol(tv_sets)]-tv_sets[,1:ncol(tv_sets)-1]

year_diff <- matrix(0,nrow=length(instyears),ncol=length(years),dimnames=list(instyears,years))

year_diff <- crd(year_diff)

year_diff[year_diff<0]<-NA

year_diff_array <- array(0,dim=c(length(countries),length(instyears),length(years)),
                         dimnames=list(countries,instyears,years))

year_diff_array <- country_alloc(year_diff_array,year_diff)

mean_array <- array(0,dim=c(length(countries),length(instyears),length(years)),
                    dimnames=list(countries,instyears,years))

mean_array <- year_alloc(mean_array,mean_fail)

sd_array <- array(0,dim=c(length(countries),length(instyears),length(years)),
                  dimnames=list(countries,instyears,years))

sd_array <- year_alloc(sd_array,sd_fail)

fail_dist <- array(0,dim=c(length(countries),length(years),length(years)),
                   dimnames=list(countries,instyears,years))

fail_dist <- pnorm(year_diff_array,mean_array,sd_array)

fail_dist[is.na(fail_dist)]<-0

fail_density <- fail_dist

fail_density[,,2:length(years)]<-fail_dist[,,2:length(years)]-fail_dist[,,1:length(years1)]

fail_split <- array(0,dim=c(length(countries),length(instyears1),length(years1),length(splitlist)),
                    dimnames=list(countries,instyears1,years1,splitlist))

for(i in 1:length(splitlist)){
  fail_split[,,,i]<-fail_density[,2:length(instyears),2:length(years)]
}

net_additions_split <- array(data=0,dim=c(length(countries),length(years1),length(splitlist)),
                          dimnames=list(countries,years1,splitlist))

for(i in 1:length(splitlist)){
  net_additions_split[,,i]<-net_additions*buy_rates[,,i]
}

base_year <- 1-fail_dist[,1,]

base_year <- base_year[,2:length(years)]

base_year[,2:length(years1)]<-base_year[,1:(length(years1)-1)]

base_year[,1]<-1

base_year<-base_year/(apply(base_year,1,sum))

base_year_split <- array(0,c(length(countries),length(years1),length(splitlist)),
                        dimnames=list(countries,years1,splitlist))

for(i in 1:length(splitlist)){
  base_year_split[,,i] <- base_year
}

tv_sets_base <- tv_sets
for(i in 1:length(years)){
  tv_sets_base[,i]<-tv_sets_base[,1]
}
tv_sets_base<-tv_sets_base[,2:length(tv_sets_base[1,])]

base_fails<-base_year*tv_sets_base

deduction_dist<-array(0,c(length(countries),length(years1),length(splitlist)),
                      dimnames=list(countries,years1,splitlist))

deduction_dist[,,c(1,6,18,20)]<-1 
# Sets these initial splits out of which base models are deducted to 1. All the rest remain 0.

base_fail_deductions<-array(0,c(length(countries),length(years1),length(splitlist)),
                            dimnames=list(countries,years1,splitlist))

for(i in 1:length(splitlist)){
  base_fail_deductions[,,i] <- -1*base_fails*deduction_dist[,,i]
}


base_fail_additions<-array(0,c(length(countries),length(years1),length(splitlist)),
                           dimnames=list(countries,years1,splitlist))

for(i in 1:length(splitlist)){
  base_fail_additions[,,i]<-base_fails*buy_rates[,,i]
}

same_year<-array(0,c(length(countries),length(years1),length(splitlist)),
                 dimnames=list(countries,years1,splitlist))

for(j in 1:length(splitlist)){
  for(i in 1:length(countries)){
    same_year[i,,j]<-diag(fail_split[i,,,j])
  }
}

failed_sets <- array(0,dim=c(length(countries),length(instyears1),length(years1),length(splitlist)),
                     dimnames=list(countries,instyears1,years1,splitlist))
# Is the 4d array for failed sets, to accompany the fail_split % array.
#apply(failed_sets,c(1,3),sum) Takes total failed sets across countries over chronological years, no splits.

failed_sets_additions <- array(0,c(length(countries),length(years1),length(splitlist)),
                               dimnames=list(countries,years1,splitlist))
# configuring the array for the additions to installed base from failed sets.

new_sets_installed <- net_additions_split+base_fail_additions+ failed_sets_additions
#Configures new sets installed from the addition.

fail_split2 <- fail_split
# Defines another 4d array for failure splits, but this second one will have its same year fails set to zero.
# This will be to avoid the equivalent of circular references on the failed sets calculations.

for(j in 1:length(splitlist)){
  for(i in 1:length(countries)){
    diag(fail_split2[i,,,j])<-0
  }
} # This sets diagonal entries to zero, thereby making same year fail rates zero, so we don't iterate
#Same year fails on top of already calculated failed set shipments ad nauseum. 


for(i in 1:length(instyears1)){
  failed_sets[,,i,]<-fail_split2[,,i,]*new_sets_installed
  for(j in 1:length(splitlist)){
    failed_sets_additions[,,j] <- apply(failed_sets[,,,1:5],c(1,3),sum)*buy_rates[,,j]
  }
  new_sets_installed <- net_additions_split+base_fail_additions+failed_sets_additions
} #Recursive loop for setting failures and new sets installed.
# Core part, that replicates the replacement cycle of the old tv_set_demand_new model.
#Sum is only from 1 to 5, over the display tech split to avoid overcounting. Is fine to apply as all buy_rates are
# split percentages of total shipments, which display tech split adds up to.


same_failed <- same_year*new_sets_installed
initial_sales <- new_sets_installed+same_failed
# This COMPLETES the first phase of this script, with initial sales calculated before rebalancing.

regions <- c("Asia Pacfic","China","Eastern Europe","Japan","Latin America",
             "Middle East and Africa","North America","Western Europe")



