# Adam Patterson
# April 25, 2021
# Dr. Dunn ARE 6695
# Problem Set 2, Part 4

library(micEconAids)

# Call WD and previously created data
setwd("~/Desktop/Dunn")
data<-read.csv("2005q1_beverages.csv")

# Create price matrix 
prices<-data.frame(exp(data$log.price.sug),exp(data$log.price.bev),exp(data$log.price.wat))
price<-c("sug_price","bev_price","wat_price")
prices<-`colnames<-`(prices,price)

# Create budget share variables 
sug_share<- data$sug.exp / data$total.exp
bev_share<-data$bev.exp / data$total.exp
wat_share<- data$wat.exp / data$total.exp

# Create budget share matrix
budget_shares<-data.frame(sug_share,bev_share,wat_share)
# 
data<-data.frame(data,prices,budget_shares)


share<-c("sug_share","bev_share","wat_share")


# Estimate Almost Ideal Demand System with Linear Approximation method and Stone Price Index selected 
aidsEst(price,share,"total.exp",data,method = "LA",priceIndex = "S")




