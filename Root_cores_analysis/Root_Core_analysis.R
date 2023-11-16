# Starting analysis on Root Core Data October 2023

library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)

#setwd("/Users/mfilewych1/Documents/GitHub/Garibaldi")
roots <- read.csv("Root_cores_analysis/Alpine_cores_names_2023_Complete.csv")

str(roots)
names(roots)

#changing Meadow to Sedge
roots <- mutate(roots, Subsite = if_else(Subsite== "Meadow", "Sedge", Subsite))

#adding new colum with duration of cores in the ground
roots <-mutate(roots, Time_in_ground=interval(mdy(Date.of.installment), mdy(Date.of.removal))) %>% 
mutate(., Duration= int_length(Time_in_ground)/86400) %>%
  mutate(., Season=case_when(Duration<360 ~ "Early Summer",
                             Duration>400 ~ "Late Summer",
                             TRUE ~"Mid Summer"))
              
# plots
ggplot(roots, aes(x=Subsite, y=Root..g.bulk.dens., fill=W.C))+
  geom_boxplot(aes(fill=W.C))+
  ylab("Roots g/bulk Density")+ theme_bw() + xlab("Plant Community") +
  scale_fill_manual(values=c( "#89C5DA", "#DA5724")) #+


rootplot<-ggplot(roots, aes(x=Duration, y=RootsBulkCubr, fill=W.C))+
  geom_point(aes(fill=W.C))+
  geom_smooth(method="lm")+
  facet_wrap(~Subsite)+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14)
        ) +
  ylab("Root g/bulk density")+ theme_bw() + xlab("Duration in ground (Days)") +
  scale_fill_manual(values=c("#89C5DA", "#DA5724"), name = "Treatment",  labels = c("Control", "Warming"))

ggplot(roots, aes(x=Duration, y=Roots.dry.weight..g., fill=W.C))+
  geom_point(aes(fill=W.C))+
  geom_smooth(method="lm")+
  facet_wrap(~Subsite)+
  ylab("Roots dry weight")+ theme_bw() + xlab("Duration (Days)") +
  scale_fill_manual(values=c( "#89C5DA", "#DA5724")) #+

# ANOVA analysis

# check distribution
hist(roots$Root..g.bulk.dens.)
#Check transformation to normal
hist(log(roots$Root..g.bulk.dens.))
hist((roots$Root..g.bulk.dens)^(1/3)) 
roots <- mutate(roots, RootsBulkCubr = Root..g.bulk.dens.^(1/3))

ModelA <- aov(data = roots , formula = RootsBulkCubr ~ Subsite*W.C*Season)
summary(ModelA)
tukey<-TukeyHSD(ModelA)
tukey
#sedge has more roots than salix 
#warming has more roots than control 
#late season more roots than early season- 
#roots still increasing despite aboveground senescence  

# linear  model
LinearA <- lm(data = roots, formula = roots$RootsBulkCubr ~ Duration*Subsite*W.C)
summary(LinearA)

