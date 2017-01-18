# Process script. Here will loop through and do all calculations

# First we import all the custom functions I created here.
source('M:/Technology/DATA/TV_sets_model/Integration/Rmagic/crd.R')
source('M:/Technology/DATA/TV_sets_model/Integration/Rmagic/year_alloc.R')
source('M:/Technology/DATA/TV_sets_model/Integration/Rmagic/country_alloc.R')



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

year_diff_array[is.na(year_diff_array)]<-0
fail_density[,,2:length(years)]<-fail_dist[,,2:length(years)]-fail_dist[,,1:length(years1)]
fail_density[year_diff_array==1]<- fail_dist[year_diff_array==1]
# This is a bit tricky. Originally the below formula was here but it is not strictly correct. 
# Strictly speaking the failures in the year after the same year fail should not be differences to the
# same year fail percentage as they are acting on new installed TVs. The failure in the first year should
# hence be cumulative, with the succeeding ones being differences for the sum of %s to add to 1.
# See also the file called "Replacement_cycle_note_AM.xlsx" in the Integration folder.

# fail_density[,,2:length(years)]<-fail_dist[,,2:length(years)]-fail_dist[,,1:length(years1)]


fail_split <- array(0,dim=c(length(countries),length(instyears1),length(years1),length(splitlist)),
                    dimnames=list(countries,instyears1,years1,splitlist))

for(i in 1:length(splitlist)){
  fail_split[,,,i]<-fail_density[,2:length(instyears),2:length(years)]
}

# net_additions_split <- array(data=0,dim=c(length(countries),length(years1),length(splitlist)),
#                           dimnames=list(countries,years1,splitlist))
# 
# for(i in 1:length(splitlist)){
#   net_additions_split[,,i]<-net_additions*buy_rates[,,i]
# }
# 
# net_additions_split <- round(net_additions_split,4)

#This whole bit of code is now redundant. Net additions are worked out in "rebalanced" form from country sales lower down.


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
#Sets these initial splits out of which base models are deducted to 1. All the rest remain 0.

base_fail_deductions<-array(0,c(length(countries),length(years1),length(splitlist)),
                            dimnames=list(countries,years1,splitlist))

for(i in 1:length(splitlist)){
  base_fail_deductions[,,i] <- -1*base_fails*deduction_dist[,,i]
}
print("base fail deductions set up")


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

buy_rates_final <- array(0,c(length(countries),length(years1),
                             length(splitlist)),dimnames=list(countries, years1,splitlist))

for(i in 1:length(splitlist)){
  buy_rates_final[,,i] <- country_sales[,,i]/apply(country_sales[,,1:5],c(1,2),sum)
} # Sets final buy rates.


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


installed_base2 <- 
  array(0,c(length(countries),length(years),length(splitlist2)),dimnames=list(countries,years,splitlist2))
#Defining second installed base for aggregate splits. necessary for accurate hh pens below.

installed_base2[,,"FPD"] <- installed_base[,,"LCD"]+installed_base[,,"OLED"]+installed_base[,,"PDP"]
installed_base2[,,"HD+"] <- installed_base[,,"HD"]+installed_base[,,"FHD"]+installed_base[,,"UHD"]
installed_base2[,,"FHD+"] <- installed_base2[,,"HD+"]-installed_base[,,"HD"]
installed_base2[,,"120Hz+"] <- installed_base[,,"120Hz"]+installed_base[,,"240Hz"]

print("Failures and installed base rebalanced.")

tv_sets <- apply(installed_base[,,1:5],c(1,2),sum)
#Resets total tv_sets once the above installed base has been figured out.

tv_sets_per_hh <- tv_sets/tv_households
#Final tv sets per hh, (less than 1 for now currently for Nepal and Afghanistan...)

tv_sets_per_hh[is.na(tv_sets_per_hh)]<-1
#Sets the inevitable NAs from the division to 1.

tv_sets_per_hh[tv_sets_per_hh<1] <- 1
# Sets the sets per hh to 1 if less than 1. Will allow me to make a quick fix, recalculating tv households below with
#it that will allow me to maintain consistency with the calculation.

tv_households <- tv_sets/tv_sets_per_hh
#Lowers households numbers and balances to installed base, preventing household numbers from exceeding IB.

single_set_households <- tv_households-(tv_sets-tv_households)/
  (tv_sets_per_hh+(1/exp(-.5025*1))*exp(-.5025*tv_sets_per_hh)-1)
# Translation of the single sets households formulas in R. First input for calculating TV hh pens.
# Yields single set hhs always as a portion of total tv hhs between 100% and 0% (limits from infinity and 0)
# of Total TV households. Elegant expression in this way!

installed_base_pen <- 
  array(0,c(length(countries),length(years1),length(splitlist)),dimnames=list(countries,years1,splitlist))
installed_base_pen2 <- 
  array(0,c(length(countries),length(years1),length(splitlist2)),dimnames=list(countries,years1,splitlist2))


installed_base_pen <- sweep(installed_base,c(1,2),apply(installed_base[,,1:5],c(1,2),sum),"/")
installed_base_pen2 <- sweep(installed_base2,c(1,2),apply(installed_base[,,1:5],c(1,2),sum),"/")
# Defined installed base penetration. Did so with no loop!! So cool :)
# Can use this top optimise the above code, it sweeps over the 3rd dimension(splits) of installed base and divides by
# calculated 2d total array from the apply function.
#Did the same for the aggregate splits too, getting installed_base_pen2.

single_set_pen <- installed_base_pen
single_set_pen2 <- installed_base_pen2

total_multi_set_households <- tv_households-single_set_households

total_multi_sets <- apply(installed_base[,,1:5],c(1,2),sum)-single_set_households

single_sets <- sweep(single_set_pen,c(1,2),single_set_households,"*")
single_sets2 <- sweep(single_set_pen2,c(1,2),single_set_households,"*")

multi_sets_per_hh <- total_multi_sets/total_multi_set_households
multi_sets_per_hh[is.na(multi_sets_per_hh)] <-0
#resovles the 0/0 situation from making sets per hh exactly equal to 1 when was less than 1.

multi_sets <- installed_base-single_sets
multi_sets2 <- installed_base2-single_sets2

multi_sets[is.na(multi_sets)] <-0

multi_set_installed_base_pen <- sweep(multi_sets,c(1,2),total_multi_sets,"/")
#multi_set_installed_base_pen <- sweep(multi_sets,c(1,2),apply(multi_sets[,,1:5],c(1,2),sum),"/")
multi_set_installed_base_pen2 <- sweep(multi_sets2,c(1,2),total_multi_sets,"/")

multi_set_installed_base_pen[is.na(multi_set_installed_base_pen)] <-0
multi_set_installed_base_pen2[is.na(multi_set_installed_base_pen2)] <-0

multi_set_households <- multi_sets/(1+sweep(multi_set_installed_base_pen,c(1,2),multi_sets_per_hh-1,"*"))
multi_set_households2 <- multi_sets2/(1+sweep(multi_set_installed_base_pen2,c(1,2),multi_sets_per_hh-1,"*"))
#These don't add up to total multi_set_households and were actually never supposed to (even in the old model).
#I am however adding the aggregate multi_set_households2 fields which I'll use at the end to arrive at sensible
# hhe penetrations through subtraction. All should be well!

multi_set_pen <- 
  array(0,c(length(countries),length(years),length(splitlist)),dimnames=list(countries,years,splitlist)) 

multi_set_pen<-sweep(multi_set_households,c(1,2),total_multi_set_households,"/")
multi_set_pen2<-sweep(multi_set_households2,c(1,2),total_multi_set_households,"/")

# multi_set_pen[,,1:5]<-sweep(multi_set_households[,,1:5],c(1,2),apply(multi_set_households[,,1:5],c(1,2),sum),"/")
# multi_set_pen[,,6:9]<-sweep(multi_set_households[,,6:9],c(1,2),apply(multi_set_households[,,6:9],c(1,2),sum),"/")
# multi_set_pen[,,10:11]<-
#   sweep(sweep(multi_set_households[,,10:11],c(1,2),apply(multi_set_households[,,10:11],c(1,2),sum),"/"),c(1,2),
#         multi_set_pen[,,2],"*")
# multi_set_pen[,,12:14]<-
#   sweep(sweep(multi_set_households[,,12:14],c(1,2),apply(multi_set_households[,,12:14],c(1,2),sum),"/"),c(1,2),
#         multi_set_pen[,,2],"*")
# multi_set_pen[,,15:16]<-
#   sweep(sweep(multi_set_households[,,15:16],c(1,2),apply(multi_set_households[,,15:16],c(1,2),sum),"/"),c(1,2),
#       multi_set_pen[,,11],"*")
# multi_set_pen[,,17:18]<-
#   sweep(multi_set_households[,,17:18],c(1,2),apply(multi_set_households[,,17:18],c(1,2),sum),"/")
# multi_set_pen[,,19:20]<-
#   sweep(multi_set_households[,,19:20],c(1,2),apply(multi_set_households[,,19:20],c(1,2),sum),"/")
# This is done to adjust the above multi_set_pens to make them normalise since the above multi_set_households don't
#initially add up to the total they are fed.


multi_set_pen[is.na(multi_set_pen)]<-0
multi_set_pen2[is.na(multi_set_pen2)]<-0
#resolves the NAs from above calculation when sets per hh is 1.

multi_set_households <- sweep(multi_set_pen,c(1,2),total_multi_set_households,"*")
multi_set_households2 <- sweep(multi_set_pen2,c(1,2),total_multi_set_households,"*")

tv_set_pen <- sweep(single_sets+multi_set_households,c(1,2),tv_households,"/")
tv_set_pen2 <- sweep(single_sets2+multi_set_households2,c(1,2),tv_households,"/")

split_tv_households <- sweep(tv_set_pen,c(1,2),tv_households,"*")
split_tv_households2 <- sweep(tv_set_pen2,c(1,2),tv_households,"*")

split_tv_households[is.na(split_tv_households)] <- 0
split_tv_households2[is.na(split_tv_households2)] <- 0


final_tv_households <- 
  array(0,c(length(countries),length(years),length(splitlist)),dimnames=list(countries,years,splitlist)) 
final_tv_households2 <- 
  array(0,c(length(countries),length(years),length(splitlist2)),dimnames=list(countries,years,splitlist2)) 


final_tv_households[,,"OLED"] <- split_tv_households[,,"OLED"]

final_tv_households[,,"RP"] <- split_tv_households[,,"RP"]

final_tv_households[,,"PDP"] <- split_tv_households[,,"PDP"]

# final_tv_households2[,,"FPD"][split_tv_households2[,,"FPD"]+final_tv_households[,,"RP"]>0] <- 
#   tv_households - final_tv_households[,,"RP"]

final_tv_households2[,,"FPD"] <- split_tv_households2[,,"FPD"]

final_tv_households[,,"LCD"] <- final_tv_households2[,,"FPD"] - 
  final_tv_households[,,"OLED"] - final_tv_households[,,"PDP"]

final_tv_households[,,"CRT"] <- tv_households - final_tv_households[,,"LCD"] - 
  final_tv_households[,,"OLED"] - final_tv_households[,,"PDP"]

final_tv_households[,,"UHD"] <- split_tv_households[,,"UHD"]

final_tv_households[,,"FHD"] <- split_tv_households2[,,"FHD+"] - final_tv_households[,,"UHD"]

final_tv_households[,,"HD"] <- split_tv_households2[,,"HD+"] - final_tv_households[,,"FHD"] -
  final_tv_households[,,"UHD"]

final_tv_households[,,"SD"] <- tv_households - final_tv_households[,,"HD"] - final_tv_households[,,"FHD"] -
  final_tv_households[,,"UHD"]

final_tv_households[,,"LED"] <- final_tv_households[,,"LCD"]*
  (split_tv_households[,,"LED"]/(split_tv_households[,,"LED"]+split_tv_households[,,"CCFL"]))

final_tv_households[,,"CCFL"] <- final_tv_households[,,"LCD"] - final_tv_households[,,"LED"]

final_tv_households[,,"Direct"] <- final_tv_households[,,"LED"]*
  (split_tv_households[,,"Direct"]/(split_tv_households[,,"Direct"]+split_tv_households[,,"Edge"]))

final_tv_households[,,"Edge"] <- final_tv_households[,,"LED"]-final_tv_households[,,"Direct"]

final_tv_households[,,"240Hz"] <- split_tv_households[,,"240Hz"]

final_tv_households[,,"120Hz"] <- split_tv_households2[,,"120Hz+"] - split_tv_households[,,"240Hz"]

final_tv_households[,,"60Hz"] <- final_tv_households[,,"LCD"] - 
  final_tv_households[,,"120Hz"]-final_tv_households[,,"240Hz"]

final_tv_households[,,"3D"] <- split_tv_households[,,"3D"]

final_tv_households[,,"2D"] <- tv_households - final_tv_households[,,"3D"]

final_tv_households[,,"Smart"] <- split_tv_households[,,"Smart"]

final_tv_households[,,"Unsmart"] <- tv_households - final_tv_households[,,"Smart"]

final_tv_households[,,"Digital"] <- split_tv_households[,,"Digital"]

final_tv_households[,,"Analog"] <- tv_households - final_tv_households[,,"Digital"]

# Reproduces the set of calculations originally done in Excel in the old demand model tor produce final TV household
# numbers that balance to match total TV households numbers but are also reflective of adoption.

final_tv_households[is.na(final_tv_households)] <- 0
final_tv_households[final_tv_households<0] <- 0
# Removes NAs and negatives and zeroes them priot to normalisation below. No negatives at the moment, but they
#may eventually appear given the calculation above for CRT and SD in developed countries..(Actually this aint really a 
# solution if installed base remains positive at that point...)


# NOW households are properly configured for the splits. I will now do some final normalisations on the above,
# Revised tv penetrations and then remultiply these to the total households numbers.

tv_set_pen[,,1:5]<-sweep(final_tv_households[,,1:5],c(1,2),colSums(aperm(final_tv_households[,,1:5],c(3,1,2))),"/")
tv_set_pen[,,6:9]<-sweep(final_tv_households[,,6:9],c(1,2),colSums(aperm(final_tv_households[,,6:9],c(3,1,2))),"/")
tv_set_pen[,,10:11]<-
  sweep(sweep(final_tv_households[,,10:11],c(1,2),colSums(aperm(final_tv_households[,,10:11],c(3,1,2))),"/"),c(1,2),
        tv_set_pen[,,"LCD"],"*")
tv_set_pen[,,12:14]<-
  sweep(sweep(final_tv_households[,,12:14],c(1,2),colSums(aperm(final_tv_households[,,12:14],c(3,1,2))),"/"),c(1,2),
                           tv_set_pen[,,"LCD"],"*")
tv_set_pen[,,15:16]<-
  sweep(sweep(final_tv_households[,,15:16],c(1,2),colSums(aperm(final_tv_households[,,15:16],c(3,1,2))),"/"),c(1,2),
                           tv_set_pen[,,"LED"],"*")
tv_set_pen[,,17:18]<-sweep(final_tv_households[,,17:18],c(1,2),
                           colSums(aperm(final_tv_households[,,17:18],c(3,1,2))),"/")
tv_set_pen[,,19:20]<-sweep(final_tv_households[,,19:20],c(1,2),
                           colSums(aperm(final_tv_households[,,19:20],c(3,1,2))),"/")
tv_set_pen[,,21:22]<-sweep(final_tv_households[,,21:22],c(1,2),
                           colSums(aperm(final_tv_households[,,19:20],c(3,1,2))),"/")
# Recalculated tv penetrations now for normalised final tv households numbers. Note ColSums after using aperm is
# more efficient than using the apply function as it's supposed to be directly vectorised, while the former
# actually just executes a for loop.


final_tv_households <- sweep(tv_set_pen,c(1,2),tv_households,"*")
final_tv_households[is.na(final_tv_households)] <- 0
# Got final households and removed NAs again given divisions above and 0/0s when model types weren't released.


print("households defined and split")

# Setting up outputs. Will clear Nas and make them zeroes first. Only need to do for households.







