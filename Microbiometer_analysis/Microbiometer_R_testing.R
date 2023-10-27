# testing out Garibaldi data and organizing the dataset, M.F. Oct 2023

setwd("/Users/mfilewych1/Documents/Github/Garibaldi/Microbiometer_analysis")
microbio <- read.csv("Microbiometer_Data_complete.csv")

head(microbio)
tail(microbio)
str(microbio)

microbio$Site <- as.factor(microbio$Site)
microbio$treatment..W.C. <- as.factor(microbio$treatment..W.C.)
microbio$Plot <- as.factor(microbio$Plot)
dim(microbio)
summary(microbio)

