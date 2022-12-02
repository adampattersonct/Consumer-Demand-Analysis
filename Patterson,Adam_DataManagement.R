# Adam Patterson
# April 22, 2021
# Dr. Dunn ARE 6695
# Problem Set 2, Part 1

#Clear R memory from previous memory storage/scripts
rm(list=ls()) 

#Beverages data obtained from https://www.ers.usda.gov/data-products/quarterly-food-at-home-price-database/quarterly-food-at-home-price-database/#Quarterly%20Food-at-Home%20Price%20Database-1%20(QFAHPD-1)

# Install and load packages

## Packages specific to demand analysis
# Install and load  micEconCES package for Constant Elasticity System ; https://cran.r-project.org/web/packages/micEconCES/micEconCES.pdf
install.packages("micEconCES")
library(micEconCES)
# Install and load  micEconAIDS package for Almost Ideal Demand System ; https://cran.r-project.org/web/packages/micEconAids/micEconAids.pdf
# https://cran.r-project.org/web/packages/micEconAids/vignettes/micEconAids_vignette.pdf is a great reference 
install.packages("micEconAids")
library(micEconAids)
# Install and load  micEcon package for Microeconomic Analysis ; https://cran.r-project.org/web/packages/micEcon/micEcon.pdf
install.packages("micEcon")
library(micEcon)



install.packages("readxl")
library(readxl)



# Set working directory to where the excel file is stored
setwd("~/Desktop/Dunn")

sug<-read_excel("fatsandpreparedfoods_q1.xls", sheet = 5)
bev<-read_excel("fatsandpreparedfoods_q1.xls", sheet = 6)
wat<-read_excel("fatsandpreparedfoods_q1.xls", sheet = 8)

#Rename salient variables for identification when merging datasets
sug$sug.price<-sug$price
sug$sug.exp<-sug$totexp
bev$bev.price<-bev$price
bev$bev.exp<-bev$totexp
wat$wat.price<-wat$price
wat$wat.exp<-wat$totexp



#Column bind the datasets while subselecting 5 specific columns (group, year, quarter, price, and expenditure)
data<-cbind(sug[,c(1,2,3,11,12)],bev[,c(1,2,3,11,12)],wat[,c(1,2,3,11,12)])

# Subset original data by year and quarter of interest.
# We subset observations with the properties called for, 2005 and quarter 1, thus we need the
# last , before closing the argument. Leaving the empty space after this , denotes that we want to call all
# columns 
data<-data[data$year==2005 & data$quarter == 1,]

# Subset variables into salient information
data<-data[,c(4,5,9,10,14,15)]


#Create a variable for total expenditure on the three goods
data$total.exp <- data$sug.exp + data$bev.exp + data$wat.exp

#Create budget share variables 
sug_share<- mean(data$sug.exp / data$total.exp)
bev_share<- mean(data$bev.exp / data$total.exp)
wat_share<- mean(data$wat.exp / data$total.exp)

# Check our variables by summing all of the budget shares. Total is 1, a good sign. 
total_share<-sug_share+bev_share+wat_share


data$quantity.sug<-data$sug.exp / data$sug.price
data$quantity.bev<-data$bev.exp / data$bev.price
data$quantity.wat<-data$wat.exp / data$wat.price

#Create logs last, making it easier to subset the final columns when creating our finished dataset
data$log.exp<-log(data$total.exp)
data$log.quantity.sug<-log(data$quantity.sug)
data$log.quantity.bev<-log(data$quantity.bev)
data$log.quantity.wat<-log(data$quantity.wat)
data$log.price.sug<-log(data$sug.price)
data$log.price.bev<-log(data$bev.price)
data$log.price.wat<-log(data$wat.price)

#Create budget share variables 
data$sug_share<- data$sug.exp / data$total.exp
data$bev_share<- data$bev.exp / data$total.exp
data$wat_share<- data$wat.exp / data$total.exp

write.csv(data, "2005q1_beverages.csv")




