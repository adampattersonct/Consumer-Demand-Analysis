# Adam Patterson
# April 25, 2021
# Dr. Dunn ARE 6695
# Problem Set 2, Part 4
#Clear R memory from previous memory storage/scripts
rm(list=ls())

library(micEconAids)
library(systemfit)
library(texreg)
library(stargazer)

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
aids<-aidsEst(price,share,"total.exp",data,method = "LA",priceIndex = "S", estMethod = "SUR",hom = FALSE,sym = FALSE)

#Estimate Almost Ideal Demand System imposing homogeneity 
aids_homog<-aidsEst(price,share,"total.exp",data,method = "LA",priceIndex = "S", estMethod = "SUR",hom = TRUE,sym = FALSE)

#Estimate Almost Ideal Demand System imposing homogeneity and symmetry
aids_homog.sym<-aidsEst(price,share,"total.exp",data,method = "LA",priceIndex = "S", estMethod = "SUR",hom = TRUE,sym = TRUE)

#Estimate Almost Ideal Demand System imposing symmetry ( imposes homogeneity)
aids_sym<-aidsEst(price,share,"total.exp",data,method = "LA",priceIndex = "S", estMethod = "SUR",hom = FALSE,sym = TRUE)

# Test whether demand satisfies homogeneity : we notice the likelihood ratio does not reject the null. Hence we can say that demand satisfies homogeneity 
a<-lrtest(aids,aids_homog)

# Test whether demand satisfies homogeneity and symmetry : we notice the likelihood ratio rejects the null. Demand does not satisfy homogeneity and symmetry 
b<-lrtest(aids,aids_homog.sym)

# Test whether demand satisfies symmetry conditional on satisfying homogeneity  : we notice the likelihood ratio rejects the null. Demand does not satisfy symmetry conditional on satisfying homogeneity 
c<-lrtest(aids_homog,aids_homog.sym)




## Create output. I could not get tables to merge as in Rotterdam. Tried tidyr, broom,cwhmsic. 
x1<-aids$coef$beta
x2<-aids_homog$coef$beta
x3<-aids_sym$coef$beta
x4<-aids_homog.sym$coef$beta
stargazer(x1,x2,x3,x4, type = "latex",)
stargazer()




stargazer(x1,x2,x3,x4, type = "latex")


stargazer(x3,type = "latex")

a<-summary(aids)

texreg(x1)

x3<-aids$coef$all

x<-tidy(aids$coef$gall)
x.a<-tidy(aids_homog$coef$all)
o<-merge(x, x.a, all.x=T, all.y=T)


aids[2$coef$alpha]
aids1<-aids$coef$alpha

a<-summary(aids)


stargazer(x1,x2,x3,x4, type = "latex")


install.packages("broom")
library(broom)
library(tidyr)
tidy(aids) -> model1_tidy
tidy(aids_homog) -> model2_tidy

tex.table(a,b,c)

install.packages("cwhmisc")
library(cwhmisc)

tex.table(a)


x<-tidy(aids$coef$stat)
tex.table(x1)
texreg(x1,x2)

tex.table




