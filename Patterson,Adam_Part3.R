# Adam Patterson
# April 26, 2021
# Dr. Dunn ARE 6695
# Problem Set 2, Part 3

#Clear R memory from previous memory storage/scripts
rm(list=ls())

setwd("~/Desktop/Dunn")

data<-read.csv("2005q1_beverages.csv")
#install.packages("nnet")
#install.packages("lmtest")
#install.packages("mgcv")
#install.packages("quantreg")
#install.packages("systemfit")
#install.packages("foreign")
#install.packages("car")
#install.packages("Rcpp")
#install.packages("dplyr")
#install.packages("BSDA")
#install.packages("stargazer")
library(systemfit)
library(dplyr)
library(stargazer)
library(texreg)
attach(data)


##Create first difference for each logged variable 
sug.price<-diff(log.price.sug)
bev.price<-diff(log.price.bev)
wat.price<-diff(log.price.wat)
exp.dif<-diff(log.exp)

## Create budget share data 
sug_share<-data$sug.exp / data$total.exp
bev_share<-data$bev.exp / data$total.exp
wat_share<-data$wat.exp / data$total.exp

#Create moving average budget share between two points of system estimation. 
sug.share<-(sug_share+lag(sug_share,1))/2
bev.share<-(bev_share+lag(bev_share,1))/2
wat.share<-(wat_share+lag(wat_share,1))/2

# Create Price Index. We disregard the first element of share as the first term lag average is NA
dlogP<- sug.share[-1]*sug.price + bev.share[-1]*bev.price + wat.share[-1]*wat.price

# Create Quantity Index. Taking quotient of logs and thus subtract
dlogQ<-exp.dif-dlogP

#Modify dependent variables by multiplying budget shares and taking first difference
sugard<-sug.share[-1]*diff(data$log.quantity.sug)
bevd<-bev.share[-1]*diff(data$log.quantity.bev)
watd<-wat.share[-1]*diff(data$log.quantity.wat)

# Create Rotterdam System of Equations
sug<-sugard~sug.price+bev.price+wat.price+dlogQ
bev<-bevd~sug.price+bev.price+wat.price+dlogQ
wat<-watd~sug.price+bev.price+wat.price+dlogQ



# Estimate elasticities equation by equation 
sug.reg<-lm(sug)
bev.reg<-lm(bev)
wat.reg<-lm(wat)

# Generate table of results ; type ="text" for tradition looking output in R
stargazer(sug.reg,bev.reg,wat.reg,style= "aer", type = "latex")

# Estimate as a system using SUR (these results are the same as equation by equation. I tried method=c("SUR","OLS") but no luck)
Rotter<-systemfit(list(sugar=sug, beverages=bev,water=wat),method="OLS")
Rotterdam<-systemfit(list(sugar=sug, beverages=bev,water=wat),method="SUR")
Rotterdam1<-systemfit(list(sugar=sug, beverages=bev,water=wat),method="WLS")
summary(Rotterdam)
summary(Rotter) # same result =( 
summary(Rotterdam1) # check weighted least squares and same 

# Estimate system imposing homogeneity 
# create restriction vector with desired restrictions 
restrict<-c("sugar_sug.price+sugar_bev.price+sugar_wat.price =0","water_sug.price+water_bev.price+water_wat.price =0", "beverages_sug.price+beverages_bev.price+beverages_wat.price =0")
Rotterdam_homogeneity<-systemfit(list(sugar=sug, beverages=bev,water=wat),method="SUR", restrict.matrix = restrict)

# Estimate system imposing homogenity and symmetry
# create restriction vector with desired restrictions  ; include own price ?
restrict1<-c("sugar_sug.price+sugar_bev.price+sugar_wat.price =0","water_sug.price+water_bev.price+water_wat.price =0", "beverages_sug.price+beverages_bev.price+beverages_wat.price =0","sugar_wat.price = water_sug.price", "sugar_bev.price = beverages_sug.price","beverages_wat.price = water_bev.price")
Rotterdam_homog.sym<-systemfit(list(sugar=sug, beverages=bev,water=wat),method="SUR", restrict.matrix = restrict1)


# Estimate system imposing symmetry
# create restriction vector with desired restrictions  ; include own price ?
restrict2<-c("sugar_wat.price = water_sug.price", "sugar_bev.price = beverages_sug.price","beverages_wat.price = water_bev.price")
Rotterdam_symmetry<-systemfit(list(sugar=sug, beverages=bev,water=wat),method="SUR", restrict.matrix = restrict2)

# Generate table of results
texreg(list(Rotterdam,Rotterdam_homogeneity,Rotterdam_homog.sym,Rotterdam_symmetry))


# Test whether demand satisfies homogeneity : we notice the likelihood ratio does not reject the null. Hence we can say that demand satisfies homogeneity 
a<-lrtest(Rotterdam,Rotterdam_homogeneity)

# Test whether demand satisfies homogeneity and symmetry : we notice the likelihood ratio rejects the null. Demand does not satisfy homogeneity and symmetry 
b<-lrtest(Rotterdam,Rotterdam_homog.sym)

# Test whether demand satisfies symmetry conditional on satisfying homogeneity : we notice the likelihood ratio rejects the null. Demand does not satisfy symmetry conditional on satisfying homogeneity 
c<-lrtest(Rotterdam_homogeneity,Rotterdam_homog.sym)




stargazer(a,b,c, type = "latex", style = "aer")











# Engel holds
sum(coef(sug.reg)[5],coef(bev.reg)[5],coef(wat.reg)[5])
# Courtnot approximately holds
sum(coef(sug.reg)[2],coef(sug.reg)[3],coef(sug.reg)[4])
sum(coef(bev.reg)[2],coef(bev.reg)[3],coef(bev.reg)[4])
sum(coef(wat.reg)[2],coef(wat.reg)[3],coef(wat.reg)[4])


stargazer(sug.reg,bev.reg,wat.reg,style= "aer", type = "text")















# Estimate the system using seemingly unrelated regression method 
Rotterdam<-systemfit(list(sugar=sug, beverages=bev,water=wat),method="SUR")
summary(Rotterdam)




#Zero degree homogeneity assumption 

#NSD of slutsky matrix 






restrict <- c("eq1_p1+eq1_p2+eq1_p3=0", "eq2_p1+eq2_p2+eq2_p3=0") 
and then

system_eqn_restrict <- systemfit(eqlist, method="SUR", restrict.matrix = restrict)
"sug.reg_sug.price+sug.reg_bev.price+sug.reg_wat.price=0"

restrict <- "sug.price + bev.price + wat.price = 0"
restrict<-"sug.reg_sug.price+sug.reg_bev.price+sug.reg_wat.price=0"

restrict <- "eq1_p1+eq1_p2+eq1_p3=0" 
restrict<-""
restrict<-c("sugar_sug.price+sugar_bev.price+sugar_wat.price =0","water_sug.price+water_bev.price+water_wat.price =0", "beverages_sug.price+beverages_bev.price+beverages_wat.price =0")

Rotterdam2<-systemfit(list(sugar=sug, beverages=bev,water=wat),method="SUR", restrict.matrix = restrict)
summary(Rotterdam2)

sum(coef(Rotterdam2)[5],coef(Rotterdam2)[10],coef(Rotterdam2)[15])

sum(coef(Rotterdam2)[2],coef(Rotterdam2)[3],coef(Rotterdam2)[4])
sum(coef(Rotterdam2)[7],coef(Rotterdam2)[8],coef(Rotterdam2)[9])
sum(coef(Rotterdam2)[12],coef(Rotterdam2)[13],coef(Rotterdam2)[14])

library(stargazer)


stargazer(Rotterdam2,style= "aer", type = "latex")


## Thought I could get a different result by using method OLS and SUR , on assumption that regular SUR was nonlinear. The code ran with but results are not different
Rotterd<-systemfit(list(sugar=sug, beverages=bev,water=wat),method=c("OLS","SUR"))
summary(Rotterd)


stargazer(sugar,beverages,water)
mod1<-summary(Rotterdam)[10]$eq[[1]]
stargazer(mod1, type = "latex")



#Coefficents
coef(Rotterdam)

#Check Engel aggregation 
sum(coef(Rotterdam)[5],coef(Rotterdam)[10],coef(Rotterdam)[15])

#Check Cornout Aggregation 
sum(coef(Rotterdam)[2],coef(Rotterdam)[3],coef(Rotterdam)[4])
sum(coef(Rotterdam)[7],coef(Rotterdam)[8],coef(Rotterdam)[9])
sum(coef(Rotterdam)[12],coef(Rotterdam)[13],coef(Rotterdam)[14])




texreg(list(Rotterdam,sug.reg,bev.reg))
