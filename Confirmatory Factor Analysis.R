# script for confirmatory factor analysis (CFA)
  # author: Molly Grant (Bergquist-O'Sullivan)
  # date: 2 May 2024 
  # R version 3.6.2 (2019-12-12)
  # running under: OS X Snow Leopard 13.6.5

########################
# set-up ###
########################
#### install and load packages ####
library(semTools) 
library(dplyr)
library(lavaan)
library(semPlot)
library(sem)
library(psych)
library(naniar)
library(haven)
library(GPArotation)
library(ggplot2)

#### read in your data sets ####
setwd("/Users/mollybergquist/Desktop") # you want to set it to the pathway where your data is saved (e.g. on your desktop)
data <- read.csv("dataset.csv") # change the data name to whatever your data are called. this function will need to change if your data are not csv. 

#### confirmatory factor analysis #####
model  <- ' factor  =~ f01 + f02 + f03 + f04 + f05 + f06 + f07 + f08 + f09 + f10' # this step tells R you want a unidimensional model. you will need to change the 'f01' etc to the items as they are named in your dataset
fit <- lavaan::cfa(model, data = data) #this runs the CFA
summary(fit, standardized = TRUE, fit.measures = TRUE, rsq = T) # this will give you stats you need to assess the model fit (e.g., CFI, TLI, RMSEA (with confidence intervals), and  SRMR)

modificationindices(fit, sort=TRUE) #if your model is not fitting well, there are some things you can do with this function to improve it.
fitmeasures(fit, fit.measures = "all", baseline.model = NULL) 

semPaths(fit, 
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree",
         rotation = 2,
         what = "std", 
         edge.color = "red" ) # this will produce a diagram of your CFA

fitPredict <- as.data.frame(predict(fit)) # this will give you the factor scores. you will need to show that these are highly correlated with the sum scores if you prefer to use the sums scores. 
data_updated <- cbind(data, fitPredict) #join your data with your factor scores

cor(data_updated$factorscores, data_updated$sumscores, method = "pearson") #test correlation between original sum scores and factor scores
