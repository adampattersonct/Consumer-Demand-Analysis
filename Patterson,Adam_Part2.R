# Adam Patterson
# April 29, 2021
# Dr. Dunn ARE 6695
# Problem Set 2, Part 2

#Clear R memory from previous memory storage/scripts
rm(list=ls())

setwd("~/Desktop/Dunn")

data<-read.csv("2005q1_beverages.csv")

library(systemfit)
library(dplyr)
attach(data)


#Linear Expenditure System
sug<-sug_share~total.exp+sug.price+bev.price+wat.price
bev<-bev_share~total.exp+sug.price+bev.price+wat.price
wat<-wat_share~total.exp+sug.price+bev.price+wat.price

sug.lm<-lm(sug)
bev.lm<-lm(bev)
wat.lm<-lm(wat)

system<-systemfit(list(sugar=sug,beverages=bev,water=wat),method = "SUR")
summary(test)

44
test<-systemfit(list(sugar=sug, beverages=bev,water=wat),method="SUR")
test1<-nlsystemfit(list(sugar=sug, beverages=bev,water=wat),method="SUR", start.values)
start.values <- c(h0=-0.5, h1=0.5, h2=-0.001, h3=0.0001, h4=0.08,
                  d0=-0.5, d1=0.009, d2=0.25, d3=0.005, d4=-0.02 )
nlsystemfit()

test2<-systemfit(list(sugar=sug, beverages=bev,water=wat),method="SUR")

texreg(test,test2)

stargazer(test, type = "latex")



















# Estimate essential edpenditure by obtaining lagged fitted values. Estimate using one of methods in Pollack 1969
essential<-lm(wat.exp~lag(wat.exp))
ess.exp<-fitted(essential)


sug<-sug.exp~I(wat.exp-ess.exp)
bev<-bev.exp~I(wat.exp-ess.exp)
wat<-wat.exp~I(wat.exp-ess.exp)

les<-systemfit(list(sug=sug, bev=bev,wat=wat),method="SUR")
summary(les)

nlsystemfit()

nlsystemfit()
texreg(les)











hg.formula <- hg ~ exp( h0 + h1*log(tht) + h2*tht^2 + h3*elev + h4*cr)
dg.formula <- dg ~ exp( d0 + d1*log(dbh) + d2*hg + d3*cr + d4*ba )
labels <- list( "height.growth", "diameter.growth" )
inst <- ~ tht + dbh + elev + cr + ba
start.values <- c(h0=-0.5, h1=0.5, h2=-0.001, h3=0.0001, h4=0.08,
                  d0=-0.5, d1=0.009, d2=0.25, d3=0.005, d4=-0.02 )
model <- list( hg.formula, dg.formula )
model.ols <- nlsystemfit( "OLS", model, start.values, data=ppine, eqnlabels=labels )







