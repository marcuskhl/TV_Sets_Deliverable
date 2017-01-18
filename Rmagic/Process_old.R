# Process script. Here will loop through and do all calculations


tv_sets <- sets_per_hh*tv_households

net_additions <- tv_sets[,2:ncol(tv_sets)]-tv_sets[,1:ncol(tv_sets)-1]

year_diff <- matrix(0,nrow=length(instyears),ncol=length(years),dimnames=list(instyears,years))

year_diff <- crd(year_diff)
# Calculates year differences using the years and instyears rows and columns
#Used in fail rate calculations.

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

net_additions_split <- round(net_additions_split,4)
#ensures accuracy and consistency rounding here.

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

print("All basics set up.")


base_fails<-base_year*tv_sets_base

deduction_dist<-array(0,c(length(countries),length(years1),length(splitlist)),
                      dimnames=list(countries,years1,splitlist))

deduction_dist[,,c(1,6,18,20)]<-1 
# Sets these initial splits out of which base models are deducted to 1. All the rest remain 0.

# base_fail_deductions<-array(0,c(length(countries),length(years1),length(splitlist)),
#                             dimnames=list(countries,years1,splitlist))
# 
# for(i in 1:length(splitlist)){
#   base_fail_deductions[,,i] <- -1*base_fails*deduction_dist[,,i]
# }
# 
# 
# base_fail_additions<-array(0,c(length(countries),length(years1),length(splitlist)),
#                            dimnames=list(countries,years1,splitlist))
# 
# for(i in 1:length(splitlist)){
#   base_fail_additions[,,i]<-base_fails*buy_rates[,,i]
# }
# 
# print("base fails set up")


same_year<-array(0,c(length(countries),length(years1),length(splitlist)),
                 dimnames=list(countries,years1,splitlist))

for(j in 1:length(splitlist)){
  for(i in 1:length(countries)){
    same_year[i,,j]<-diag(fail_split[i,,,j])
  }
}

print("same year fails set up")

failed_sets <- array(0,dim=c(length(countries),length(instyears1),length(years1),length(splitlist)),
                     dimnames=list(countries,instyears1,years1,splitlist))
# Is the 4d array for failed sets, to accompany the fail_split % array.
#apply(failed_sets,c(1,3),sum) Takes total failed sets across countries over chronological years, no splits.

fail_split2 <- fail_split
# Defines another 4d array for failure splits, but this second one will have its same year fails set to zero.
# This will be to avoid the equivalent of circular references on the failed sets calculations.

for(j in 1:length(splitlist)){
  for(i in 1:length(countries)){
    diag(fail_split2[i,,,j])<-0
  }
} # This sets diagonal entries to zero, thereby making same year fail rates zero, so we don't iterate
#Same year fails on top of already calculated failed set shipments ad nauseum. 

print("failure distribution defined")

# failed_sets_additions <- array(0,c(length(countries),length(years1),length(splitlist)),
#                                dimnames=list(countries,years1,splitlist))
# # configuring the array for the additions to installed base from failed sets.
# 
# new_sets_installed <- array(0,c(length(countries),length(years1),length(splitlist)),
#                             dimnames=list(countries,years1,splitlist))
# # Configures new sets installed, produced in the loop below.
# 
# 
# new_sets_installed <- net_additions_split+base_fail_additions+ failed_sets_additions
# #Configures new sets installed from the addition.
# 
# pb <- txtProgressBar(min = 0, max = length(instyears1), style = 3)
# for(i in 1:length(instyears1)){
#   failed_sets[,,i,]<-fail_split2[,,i,]*new_sets_installed
#   failed_sets_additions <- sweep(buy_rates,c(1,2),apply(failed_sets[,,,1:5],c(1,3),sum),"*")
#   new_sets_installed <- net_additions_split+base_fail_additions+failed_sets_additions
# #   for(j in 1:length(splitlist)){
# #     failed_sets_additions[,,j] <- apply(failed_sets[,,,1:5],c(1,3),sum)*buy_rates[,,j]
#     #print(i*(length(instyears)-1)+j)
#     #setTxtProgressBar(pb, i*j)
# #   }  
#   #print(i)
#   setTxtProgressBar(pb, i)
# } #Recursive loop for setting failures and new sets installed.
# # Core part, that replicates the replacement cycle of the old tv_set_demand_new model.
# #Sum is only from 1 to 5, over the display tech split to avoid overcounting. Is fine to apply as all buy_rates are
# # split percentages of total shipments, which display tech split adds up to.
# 
# print("first run of failures and new installed done.")
# 
# same_failed <- same_year*new_sets_installed
# initial_sales <- new_sets_installed+same_failed
# # This COMPLETES the first phase of this script, with initial sales calculated before rebalancing.
# 
# region_initial <- array(0,c(length(regions),length(years1),length(splitlist)),dimnames=list(regions,years1,splitlist))
# #Defines the region totals initially obtained from running the model.Contrasts with region_sales gotten from
# # the input.
# 
# 
# for(i in 1:length(regions)){
#   if(length(which(region_list2==regions[i]))>1){
#     region_initial[i,,]<-apply(initial_sales[region_list2==regions[i],,],c(2,3),sum)
#   }
#   else{
#     region_initial[i,,]<-initial_sales[region_list2==regions[i],,]
#   }
# } # allocates region sales from the model!
# 
# 
# scale_up <- region_sales/region_initial 
# # helps get country sales from region sales, by taking ratio of benchmark region_sales and initial total region sales.
# scale_up[is.nan(scale_up)] <- 0
# scale_up[is.infinite(scale_up)] <- 0 
# # Clears up errors from taking the ratio and turns them to zeroes.
# 
# 
# country_sales <- array(0,c(length(countries),length(years1),
#                            length(splitlist)),dimnames=list(countries, years1,splitlist))
# 
# country_scale_up <- array(0,c(length(countries),length(years1),length(splitlist)),
#                           dimnames=list(countries, years1,splitlist)) 
# #Array defined to do scale up on the country sales.
# 
# 
# 
# # Defines by country scale up at same dimensionality to initial sales and therefore conformable.
# 
# 
# #Array defined to do scale up on the country sales.
# for(i in 1:length(regions)){
#   for(j in which(region_list2==regions[i])){
#     country_scale_up[j,,] <- scale_up[i,,]
#   }
# } # Defines by country scale up at same dimensionality to initial sales and therefore conformable.
# 
# 
# 
# for(i in 1:length(regions)){
#   if(length(which(region_list2==regions[i]))>1){
#     country_sales[which(region_list2==regions[i]),,] <- 
#       country_scale_up[which(region_list2==regions[i]),,]*initial_sales[which(region_list2==regions[i]),,]
#   }
#   else{
#     country_sales[which(region_list2==regions[i]),,] <- region_sales[i,,]
#   }
# } # Gets country sales defined that exactly match region sales input.


buy_rates_final <- array(0,c(length(countries),length(years1),
                             length(splitlist)),dimnames=list(countries, years1,splitlist))

for(i in 1:length(splitlist)){
  buy_rates_final[,,i] <- country_sales[,,i]/apply(country_sales[,,1:5],c(1,2),sum)
} # Sets final buy rates.


# ROR_initial <- array(0,c(length(rest_of_regions),length(years1[23:24]),length(splitlist[1:5])),
#                      dimnames=list(rest_of_regions,years1[23:24],splitlist[1:5]))
# #Defines the rest of region totals initially obtained from the country sales that add up to 
# 
# 
# for(i in 1:length(rest_of_regions)){
#   if(length(which(ROR_list2==rest_of_regions[i]))>1){
#     ROR_initial[i,,]<-apply(country_sales[ROR_list2==rest_of_regions[i],23:24,1:5],c(2,3),sum)
#   }
#   else{
#     ROR_initial[i,,]<-country_sales[ROR_list2==rest_of_regions[i],23:24,1:5]
#   }
# } # Completes the first part of the dodgy rebalancing to match country historics for
# #BRI +US and Canada. Gets rest of region totals as initially coming from country sales
# #that add up to region sales.
# 
# ROR_scale_up <- ROR_sales/ROR_initial 
# # helps get country sales from ROR sales, by taking ratio of benchmark ROR_sales and initial total ROR sales.
# ROR_scale_up[is.nan(ROR_scale_up)] <- 0
# ROR_scale_up[is.infinite(ROR_scale_up)] <- 0 
# # Clears up errors from taking the ratio and turns them to zeroes.
# 
# ROR_country_scale_up <- array(0,c(length(countries),length(years1[23:24]),length(splitlist[1:5])),
#                           dimnames=list(countries, years1[23:24],splitlist[1:5])) 
# #Array defined to do scale up on the country sales, for the ROR sales in display technology.
# 
# for(i in 1:length(rest_of_regions)){
#   for(j in which(ROR_list2==rest_of_regions[i])){
#     ROR_country_scale_up[j,,] <- ROR_scale_up[i,,]
#   }
# } # Defines by country scale up at same dimensionality to initial sales and therefore conformable.
# 
# country_sales2 <-country_sales
# #spare array for me to do second loop with.
# 
# for(i in 1:length(rest_of_regions)){
#   if(length(which(ROR_list2==rest_of_regions[i]))>1){
#     country_sales[which(ROR_list2==rest_of_regions[i]),23:24,1:5] <- 
#       ROR_country_scale_up[which(ROR_list2==rest_of_regions[i]),,]*
#       country_sales2[which(ROR_list2==rest_of_regions[i]),23:24,1:5]
#   }
#   else{
#     country_sales[which(ROR_list2==rest_of_regions[i]),23:24,1:5] <- ROR_sales[i,,]
#   }
# } # Gets country sales defined that exactly match rest of region sales input.
# 
# for(i in 1:length(splitlist[1:5])){
#   buy_rates_final[,,i] <- country_sales[,,i]/apply(country_sales[,,1:5],c(1,2),sum)
# } # Resets final buy rates just for display technology.
# 
# country_sales2 <- country_sales
# #Stores values in country_sales2 once again, before executing what's below 
# #(safeguards against unexpected modifications).
# 
# country_sales <- sweep(buy_rates_final,c(1,2),apply(country_sales2[,,1:5],c(1,2),sum),"*")
# #Finally rebalances all country sales, to match the now ROR balanced country totals,
# # while the displaytech modified splits match the ROR input exactly too.
# 
# print("country sales configured")

same_failed_rebalanced <- country_sales*(same_year/(1+same_year))
# Now started the rebalancing and got same year failed sets rescaled. 


new_sets_installed_rebalanced <- country_sales-same_failed_rebalanced
# Got new_sets_installed rebalanced now.


base_fail_additions_rebalanced <- array(0,c(length(countries),length(years1),
                             length(splitlist)),dimnames=list(countries, years1,splitlist))

for(i in 1:length(splitlist)){
  base_fail_additions_rebalanced[,,i]<-base_fails*buy_rates_final[,,i]
}
#base_fail_additions_rebalanced<-sweep(buy_rates_final,c(1,2),base_fails,"*")
# Rebalanced base fails now to match new buy rates exactly.

failed_sets_rebalanced <- 
  array(0,c(length(countries),length(instyears1),length(years1),
            length(splitlist)),dimnames=list(countries,instyears1,years1,splitlist)) 
# Defining rebalanced failed sets.

for(i in 1:length(instyears1)){
  failed_sets_rebalanced[,,i,]<-fail_split2[,,i,]*new_sets_installed_rebalanced
} # Resetting failed sets using rebalanced new installed numbers.

failed_sets_deductions <- array(0,c(length(countries),length(years1),length(splitlist)),
                                dimnames=list(countries,years1,splitlist))
# defining failed sets deductions. This is going to be rebalanced number from beginning to re-derive installed base.

failed_sets_deductions <- -1 * apply(failed_sets_rebalanced,c(1,3,4),sum) # Deducted failed sets.

net_additions_rebalanced <- new_sets_installed_rebalanced+failed_sets_deductions+base_fail_deductions
# Net additions rebalanced defined.

splitdist <- array(0, length(splitlist),dimnames=list(splitlist))
#Sets initial split distribution. Same for all countries in 1990. Is a 5 row 1D vector.

splitdist[c("CRT","SD","2D","Unsmart")]<-1
#Makes the above splits initially 100% for the installed base.

initial_base_dist <- matrix(splitdist,nrow=length(countries),ncol=length(splitlist),
                            byrow=TRUE,dimnames=list(countries,splitlist))
# Produces  matrix of initial splits for each country filled by row! For 1999, by row=TRUE ensures done properly.

installed_base <- 
  array(0,c(length(countries),length(years),length(splitlist)),dimnames=list(countries,years,splitlist)) 


for(i in 1:length(splitlist)){
  installed_base[,1,i]<-initial_base_dist[,i]*tv_sets[,1]
} # Sets the initial installed base in 1990 with 100% for CRT and 0% for all other splits.


for(i in 1:length(years1)){
  installed_base[,1+i,]<-installed_base[,i,]+net_additions_rebalanced[,i,]
} # Then gets installed base for all years from 1990 to 2019!  If i=0, RHS IB is at 1990 and LHS IB is 1991 one.

print("Failures and installed base rebalanced.")


single_set_households <- tv_households-(tv_sets-tv_households)/
  (sets_per_hh+(1/exp(-.5025*1))*exp(-.5025*sets_per_hh)-1)
# Translation of the single sets households formulas in R. First input for calculating TV hh pens.
# Yields single set hhs always as a portion of total tv hhs between 100% and 0% (limits from infinity and 0)
# of Total TV households. Elegant expression in this way!

installed_base_pen <- 
  array(0,c(length(countries),length(years1),length(splitlist)),dimnames=list(countries,years1,splitlist))


installed_base_pen <- sweep(installed_base,c(1,2),apply(installed_base[,,1:5],c(1,2),sum),"/")
# Defined installed base penetration. Did so with no loop!! So cool :)
# Can use this top optimise the above code, it sweeps over the 3rd dimension(splits) of installed base and divides by
# calculated 2d total array from the apply function.

single_set_pen <- installed_base_pen

total_multi_set_households <- tv_households-single_set_households

total_multi_sets <- apply(installed_base[,,1:5],c(1,2),sum)-single_set_households

single_sets <- sweep(single_set_pen,c(1,2),single_set_households,"*")

tv_sets_per_hh<-apply(installed_base[,,1:5],c(1,2),sum)/tv_households
#Final tv sets per hh, (less than 1 for now currently for Nepal and Afghanistan...)

multi_sets_per_hh <- total_multi_sets/total_multi_set_households

multi_sets <- installed_base-single_sets

multi_set_installed_base_pen <- sweep(multi_sets,c(1,2),apply(multi_sets[,,1:5],c(1,2),sum),"/")

multi_set_households <- multi_sets/(1+sweep(multi_set_installed_base_pen,c(1,2),multi_sets_per_hh-1,"*"))

multi_set_pen <- sweep(multi_set_households,c(1,2),apply(multi_set_households[,,1:5],c(1,2),sum),"/")

multi_set_households <- sweep(multi_set_pen,c(1,2),total_multi_set_households,"*")

tv_set_pen <- sweep(single_sets+multi_set_households,c(1,2),tv_households,"/")



