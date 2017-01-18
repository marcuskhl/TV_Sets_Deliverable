pre_2006_PDP_fails<-
  rbind(colSums(failed_sets_rebalanced["UK",1:15,15:29,4],dims=1),
        colSums(failed_sets_rebalanced["France",1:15,15:29,4],dims=1),
        colSums(failed_sets_rebalanced["Germany",1:15,15:29,4],dims=1))

write.csv(pre_2006_PDP_fails,"pre_2006_PDP_fails.csv")

PDP_2005_installed_base<-
  rbind(installed_base["UK",15:20,4],
        installed_base["France",15:20,4],
        installed_base["Germany",15:20,4])

write.csv(PDP_2005_installed_base,"PDP_2005_installed_base.csv")