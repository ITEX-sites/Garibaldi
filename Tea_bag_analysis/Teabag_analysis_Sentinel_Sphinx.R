#Teabag decomposition analysis for the Sentinel and Sphinx Sites - started Nov 1 2023 MF

library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)

setwd("/Users/mfilewych1/Documents/GitHub/Garibaldi/Tea_bag_analysis")
#teabag <- read.csv("Tea_bag_decomposition.csv")

tea <- read.csv("Simplified_tea.csv")
tea <- slice(tea, 1:24)

#get rid of unimportant rows
#teabag <- teabag[c(57:85), ]
#teabag1 <- slice(teabag, 6:30)

#set header titles
#names(teabag1) <- teabag[1,]

tea$Location <- gsub("Heather", "Cassiope", tea$Location)

# Plots ----------------------------------------------

# plot k by treatment
ggplot(tea, aes(x=Location, y=k, fill=Treatment))+
  geom_boxplot(aes(fill=Treatment))+
  ylab("Decomposition rate")+ theme_bw() + xlab("Plant community") +
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"),name = "Treatment",  labels = c("Control", "Warming")) +
  labs(title = "Decomposition rate of teas", fill= "Treatment") #+

# plot s by treatment
ggplot(tea, aes(x=Location, y=S, fill=Treatment))+
  geom_boxplot(aes(fill=Treatment))+
  ylab("Stabilization factor")+ theme_bw() + xlab("Plant community") +
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"),name = "Treatment",  labels = c("Control", "Warming")) +
  labs(title = "Stabilization factor by site", fill= "Treatment") #+

#ANOVA for k --------------------------------------------
# check distribution
hist(tea$k)

#Check transformation to normal
hist(sqrt(tea$k)) #basically same as non-transformed
hist(log(tea$k)) #not great
hist(tea$k^(1/3)) #very bad
hist(tea$k/(1/tea$k)) #very bad

#trying something else
install.packages("rcompanion")
transf_tea <- rcompanion::transformTukey(tea$k)
hist(transf_tea)

#trying a box-cox
install.packages("MASS")
library(MASS)
#tea.bc <- lm(k ~ Location*Treatment, data = tea)
#tea.k.bc <- boxcox(tea.bc)
#(lambda <- tea.k.bc$x[which.max(tea.k.bc$y)])
# Transform the data using this lambda value
#tea.k <- tea %>%
 # mutate(k.bc = ((k^lambda-1)/lambda))

# Plot a histogram of the Box-Cox transformed data
hist(tea.k$k.bc)

Modelk <- aov(data = tea, formula = transf_tea ~ Treatment*Location)
summary(Modelk)
TukeyHSD(Modelk)

#ANOVA for S ----------------------------

#check
hist(tea$S)
ModelS <- anova_test(data = tea, formula = S ~ Treatment*Location)
summary(ModelS)
