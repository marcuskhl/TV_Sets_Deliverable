markdown <- read.csv("markdown.csv")
markdown <- melt(markdown,id.vars=colnames(markdown[,1:4]))
markdown$month <- substr(markdown$variable,1,3)
markdown$year <- substr(markdown$variable,4,6)
markdown$quarter <- sapply(markdown$month,monthquarter)
markdown$ln_markdown <- log(markdown$value)
#markdown$quarter[markdown$quarter!="Q4"]="Q1"
markdown2<- markdown[!is.na(markdown$value),][markdown$value[!is.na(markdown$value)]>.25,] #removes extreme outliers.
markdown2<- markdown2[!is.na(markdown2$value),][markdown2$value[!is.na(markdown2$value)]<1.6,] #removes extreme outliers.
markdown_reg <- lm(ln_markdown~Brand+Size.Range+Display.Format+year+quarter,markdown2)
markdown2$cook <- 0
markdown2$cook[!is.na(markdown2$value)]<-cooks.distance(markdown_reg) 
markdown3 <- markdown2[markdown2$cook<0.2,] #removes the worst outliers/high leverage points
markdown_reg2 <- lm(ln_markdown~Brand+Size.Range+Display.Format+year+quarter,markdown3)
print(summary(markdown_reg2))
plot(exp(fitted(markdown_reg2)),exp(markdown3$ln_markdown[!is.na(markdown3$value)]),
     xlab="fitted",ylab="actuals",main="ASP markdown against fitted values")
#Plots fitted valus against actuals.Exponentiated to get back markup values.
abline(0,1,col="red") 
#Gives line of best fit on plot of fitted values against actuals.
markdown_final <- markdown
markdown_final$ln_markdown[is.na(markdown_final$ln_markdown)] <-
  predict(markdown_reg2,markdown_final[is.na(markdown_final$ln_markdown),c(1:4,8,9)])
markdown_final$value[is.na(markdown_final$value)] <-
  exp(markdown_final$ln_markdown[is.na(markdown_final$value)])
markdown_final <- markdown_final[,1:6]
markdown_final <- cast(markdown_final,...~variable,fun.aggregate=mean) 
#Cast function reproduces 2d output for the "time" variable.
markdown <- cast(markdown[,1:6],...~variable,fun.aggregate=mean) 
#Same is necessary for original table's reproduction in 2d. Fun.aggregate has to be specified in both cases
#as there is degeneracy in the Others field requiring aggregation. Appropriate choice is mean, otherwise
#defaults to using length.
write.csv(markdown_final,"markdown_final.csv")
write.csv(markdown,"markdown_initial.csv")