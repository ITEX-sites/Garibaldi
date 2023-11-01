#Teabag decomposition analysis for the Sentinel and Sphinx Sites - started Nov 1 2023 MF

library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)

setwd("/Users/mfilewych1/Documents/GitHub/Garibaldi/Tea_bag_analysis")
teabag <- read.csv("Tea_bag_decomposition.csv")

#get rid of unimportant rows
teabag <- teabag[c(57:85), ]
teabag1 <- slice(teabag, 6:30)
#set header titles
names(teabag1) <- teabag[1,]

