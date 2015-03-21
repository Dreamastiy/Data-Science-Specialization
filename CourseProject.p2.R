data(ToothGrowth) #load data
pairs(ToothGrowth) #look at dependecies
hist(ToothGrowth$len) #hist of lengths
summary(ToothGrowth) #summary
qqnorm(ToothGrowth$len) #Q-Q plot (length against normal)
suppMean <- tapply(ToothGrowth$len, ToothGrowth$supp, mean)
#Find mean a var of different length groups and calculate number of elements in each group
suppVar <- tapply(ToothGrowth$len, ToothGrowth$supp, var)
suppOJ <- sum(ToothGrowth$supp=='OJ')
suppVC <- sum(ToothGrowth$supp=='VC')
doseMean <- tapply(ToothGrowth$len, ToothGrowth$dose, mean)
doseVar <- tapply(ToothGrowth$len, ToothGrowth$dose, var)
dose0.5 <- sum(ToothGrowth$dose==0.5)
dose2.0 <- sum(ToothGrowth$dose==2.0)
# Hypothesis testing supp, var is equal
t.test(ToothGrowth$len ~ ToothGrowth$supp, 
       alternative='greater',
       var.equal=T)
# confidence interval
suppMean[[1]]-suppMean[[2]] + 
     c(1)*qt(0.95, df=(60-2), lower.tail=F) * 
     sqrt(1/suppVC+ 1/suppOJ) * 
     sqrt((suppVar[[1]]*(suppOJ-1)+suppVar[[2]]*(suppVC-1))/58) 
# t.test supp. var is not equal
t.test(ToothGrowth$len ~ ToothGrowth$supp, 
       alternative='greater',
       var.equal=F)
# confidence interval for difference of dose=0.5, dose=2.0
doseMean[[1]]-doseMean[[3]] + 
     c(1) *qt(0.95, df = 38, lower.tail=T) *
     sqrt(1/dose0.5+ 1/dose2.0) * 
     sqrt((doseVar[[1]]*(dose0.5-1)+doseVar[[3]]*(dose2.0-1))/38) 
# t.test for dose var is  equal
t.test(ToothGrowth$len[ToothGrowth$dose == 0.5] , ToothGrowth$len[ToothGrowth$dose == 1.0], 
       alternative='less',
       var.equal=T)
t.test(ToothGrowth$len[ToothGrowth$dose == 1.0] , ToothGrowth$len[ToothGrowth$dose == 2.0], 
       alternative='less',
       var.equal=T)
t.test(ToothGrowth$len[ToothGrowth$dose == 0.5] , ToothGrowth$len[ToothGrowth$dose == 2.0], 
       alternative='less',
       var.equal=T)
# t.test for dose var is not equal
t.test(ToothGrowth$len[ToothGrowth$dose == 0.5] , ToothGrowth$len[ToothGrowth$dose == 1.0], 
       alternative='less',
       var.equal=F)
t.test(ToothGrowth$len[ToothGrowth$dose == 1.0] , ToothGrowth$len[ToothGrowth$dose == 2.0], 
       alternative='less',
       var.equal=F)
t.test(ToothGrowth$len[ToothGrowth$dose == 0.5] , ToothGrowth$len[ToothGrowth$dose == 2.0], 
       alternative='less',
       var.equal=F)



