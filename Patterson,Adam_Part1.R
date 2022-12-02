# Adam Patterson
# April 25, 2021
# Dr. Dunn ARE 6695
# Problem Set 2, Part 1
library(stargazer)
library(texreg)

#Clear R memory from previous memory storage/scripts
rm(list=ls())
# Call WD and previously created data
setwd("~/Desktop/Dunn")
data<-read.csv("2005q1_beverages.csv")


# Create a random sample of 75 % of observations to be used to train the model. We use the complement 25% observations 
# to test the results from our training dataset to determine if the result can be extrapolated. This is a technique in cross validation. Ideally, with larger data
#  set.seed for reproducible result
set.seed(1)
train <- sample(1:dim(data)[1], 0.75*dim(data)[1], replace = FALSE) 
data.train<-data[train,]
data.test<-data[-train,]

# Create equations using log quantity as dependent variable
sugar<-lm(log.quantity.sug~log.exp+log.price.sug+log.price.bev+log.price.wat,data.train)
beverages<-lm(log.quantity.bev~log.exp+log.price.sug+log.price.bev+log.price.wat,data.train)
water<-lm(log.quantity.wat~log.exp+log.price.sug+log.price.bev+log.price.wat,data.train)
# Create output 
stargazer(sugar,beverages,water,type = "latex")

# Obtain predictions for log quantity using trained models and test observations
sug_test<-predict(sugar,data.test)
bev_test<-predict(beverages,data.test)
wat_test<-predict(water,data.test)

#Calculate  predicted budget shares. (each share is log predicted quantity times log price divided by total pq (expenditure))
exp<-((sug_test + data.train$log.price.sug) + (bev_test + data.train$log.price.bev) + (wat_test +data.train$log.price.wat))
sugar.share<- (sug_test+data.train$log.price.sug) / exp
bev.share<-(bev_test + data.train$log.price.bev) / exp 
wat.share<- (wat_test +data.train$log.price.wat) / exp

# Calculate sum of squared residuals from prediction
ssr.sug<- sum((data.test$log.quantity.sug - sug_test)^2)
ssr.bev<- sum((data.test$log.quantity.bev - bev_test)^2)
ssr.wat<- sum((data.test$log.quantity.wat - wat_test)^2)


# Estimate equations using expenditure share as dependent variable
sugar.1<-lm(sug_share~log.exp+log.price.sug+log.price.bev+log.price.wat,data.train)
beverages.1<-lm(bev_share~log.exp+log.price.sug+log.price.bev+log.price.wat,data.train)
water.1<-lm(wat_share~log.exp+log.price.sug+log.price.bev+log.price.wat,data.train )
#Create Output
stargazer(sugar.1,beverages.1,water.1,type = "latex")

# Obtain predictions for expenditure share using trained models and test observations
sug_test1<-predict(sugar.1,data.test)
bev_test1<-predict(beverages.1,data.test)
wat_test1<-predict(water.1,data.test)


# Calculate sum of squared residuals from prediction
ssr.sug.1<- sum((data.test$sug_share - sug_test1)^2)
ssr.bev.1<- sum((data.test$bev_share - bev_test1)^2)
ssr.wat.1<- sum((data.test$wat_share - wat_test1)^2)

# Calculate sum of squared residuals from regression results
sum(sugar.1$residuals^2)
sum(beverages.1$residuals^2)
sum(water.1$residuals^2)

ssr<-paste(round(ssr.sug,3),round(ssr.bev,3),round(ssr.wat,3))
ssr.1<-paste(round(ssr.sug.1,3),round(ssr.bev.1,3),round(ssr.wat.1,3))
library(stargazer)
stargazer(ssr,ssr.1)



# Calculate the sum of squared residuals
sugar_test

data.test$quantity.sug

data.test$log.quantity.wat

s<-sug_test*log.price.sug
b<-bev_test*log.price.bev
w<-wat_test*log.price.wat





# Calculate predicted expenditure share
predicted_sug_share<- sug_test / total_expend
predicted_bev_share<- bev_test / total_expend
predicted_wat_share<- wat_test / total_expend


data.test$total.exp
log(data.test$total.exp)




data.test$sug.price
data.test$bev.price

log(data.test$sug.price)
log(data.test$bev.price)




data.test$log.price.sug
















systemfit(list(sugar=sug,beverages=bev,water=wat), method = "SUR")






# Imposing 
restrict<-c("sugar_log.price.sug+sugar_log.price.bev+sugar_log.price.wat =0","water_log.price.sug+water_log.price.bev+water_log.price.wat =0", "beverages_log.price.sug+beverages_log.price.bev+beverages_log.price.wat =0")
systemfit(list(sugar=sug,beverages=bev,water=wat), method = "SUR", restrict.matrix = restrict)


#Use CES function, made for production function CES, to estimate CES by setting V parameter to -1 to get 1/p 

data$y3 <- cesCalc(xNames = c("sug.price", "wat.price", "bev.price","total.exp"), data = data,coef = c(gamma = 1, delta_1 = 0.7, delta_2 = 0.6, delta = 0.5,rho_1 = 0.3, rho_2 = 0.4, rho = 0.5, nu = 1.1), nested = TRUE)
cesKmenta <- cesEst(yName = "y3", xNames = c("sug.price", "wat.price", "bev.price","total.exp"), data = data,method = "LM", vrs = TRUE)


log qi = log x + log prices 




install.packages("EconDemand")

library(EconDemand)

demand<-DemandQuantity(prices,c(10,.5),method = "Linear", message = TRUE, Plot = FALSE)










data$y3 <- cesCalc(xNames = c("sug.price", "wat.price", "bev.price","total.exp"), data = data,coef = c(gamma = 1, delta_1 = 0.7, delta_2 = 0.6, delta = 0.5,rho_1 = 0.3, rho_2 = 0.4, rho = 0.5, nu = 1.1), nested = TRUE)
cesKmenta <- cesEst(yName = "y3", xNames = c("sug.price", "wat.price", "bev.price","total.exp"), data = data,method = "LM", vrs = TRUE)
summary(cesKmenta)



data

## y3 would be total quantity. take this and times by mean budget share 
data$total_q <- cesCalc(xNames = c("sug.price", "wat.price", "bev.price","total.exp"), data = data,coef = c(gamma = 1, delta_1 = 0.7, delta_2 = 0.6, delta = 0.5,rho_1 = 0.3, rho_2 = 0.4, rho = 0.5, nu = 1.1), nested = TRUE)
, tName = NULL, rhoApprox = 5e-6)

data$q_sug<-data$total_q*sug_share
data$q_bev<-data$total_q*bev_share
data$q_wat<-data$total_q*wat_share


ces_sug <- cesEst(yName = "q_sug", xNames = c("sug.price", "wat.price", "bev.price","total.exp"), data = data,method = "LM", vrs = TRUE)
summary(ces_sug)





log.qt.sug <- cesCalc(xNames = c("log.exp", "log.price.sug", "log.price.bev","log.price.wat"), data = data,coef = c(gamma = 1, delta_1 = 0.7, delta_2 = 0.6, delta = 0.5,rho_1 = 0.3, rho_2 = 0.4, rho = 0.5, nu = 1.1), nested = TRUE, tName = NULL, rhoApprox = 5e-6)
cesKmenta <- cesEst(yName = "log.qt.sug", xNames = c("log.exp", "log.price.sug", "log.price.bev","log.price.wat"), data = data,method = "Newton", vrs = TRUE)


summary(cesKmenta)

is.na(data)


y3<-cesCalc(xNames = c("log.exp", "log.price.sug"), data = data,coef = c(gamma = 1, delta = 0.6, rho = 0.5, nu = 1.1))
y2 <- cesCalc(xNames = c("log.exp", "log.price.sug"), data = data,coef = c(gamma = 1, delta = 0.6, rho = 0.5, nu = 1.1))
cesKmenta <- cesEst(yName = "y4", xNames = c("x1", "x2"), data = cesData,method = "Kmenta", vrs = TRUE)




ces
cesCalc()

cesEst( yName, xNames, data, tName = NULL, vrs = FALSE, method = "LM",
        start = NULL, lower = NULL, upper = NULL, multErr = FALSE,
        rho1 = NULL, rho2, rho = NULL, returnGridAll = FALSE,
        returnGrad = FALSE, random.seed = 123,
        rhoApprox = c( y = 5e-6, gamma = 5e-6, delta = 5e-6,
                       rho = 1e-3, nu = 5e-6 ),
        checkStart = TRUE, ... )


data
x<-cesEst("log.quantity.sug", c("log.exp", "log.price.sug", "log.price.bev","log.price.wat") ,data,  tName = NULL, vrs = FALSE, method = "PORT",
       start = NULL, lower = NULL, upper = NULL, multErr = FALSE,
       rho1 = NULL, rho2=NULL, rho = NULL, returnGridAll = FALSE,
       returnGrad = FALSE, random.seed = 123,
       rhoApprox = c( y = 5e-6, gamma = 5e-6, delta = 5e-6,
                      rho = 1e-3, nu = 5e-6 ),
       checkStart = FALSE)
log
"log.quantity.bev", xNames = c("log.exp", "log.price.sug"), data = data


y<-data$log.quantity.sug
x1<-data$log.quantity.bev
x2<-data$log.quantity.wat  
x3<-data$log.exp 
Kmenta <- cesEst(yName = y, xNames = c(x1,x2,x3), data = data,method = "CG", vrs = TRUE)

rhoVec <- c(seq(-1, 1, 0.1), seq(1.2, 4, 0.2), seq(4.4, 14, 0.4))
cesCalc("log.quantity.bev", c("log.exp", "log.price.sug", "log.price.bev","log.price.wat") ,data,  tName = NULL, vrs = FALSE, method = "LM",
       checkStart = FALSE)

cesEst(yName = "log.quantity.bev", xNames = c("log.exp", "log.price.sug","log.price.bev","log.price.wat"), data = data,rho1 = rhoVec, rho = rhoVec, control = nls.lm.control(maxiter = 1000,maxfev = 2000))


data$log.
data$log
cesKmenta <- cesEst(yName = "log.quantity.bev", xNames = c("log.exp", "log.price.sug","log.price.bev","log.price.wat"), data = data,method = "CG", vrs = TRUE, checkStart = FALSE)


cesKmenta <- cesEst("log.quantity.bev", xNames = c("log.exp", "log.price.sug"), data = data,method = "PORT", control = list(iter.max = 1000,eval.max = 2000))















#Create budget share variables 
sug_share<- mean(data$sug.exp / data$total.exp)
bev_share<- mean(data$bev.exp / data$total.exp)
wat_share<- mean(data$wat.exp / data$total.exp)

# Check our variables by summing all of the budget shares. Total is 1, a good sign. 
total_share<-sug_share+bev_share+wat_share
#attach(data.train)



# Cre
#sug<-log.quantity.sug~log.exp+log.price.sug+log.price.bev+log.price.wat
#bev<-log.quantity.bev~log.exp+log.price.sug+log.price.bev+log.price.wat
#wat<-log.quantity.wat~log.exp+log.price.sug+log.price.bev+log.price.wat


function()

