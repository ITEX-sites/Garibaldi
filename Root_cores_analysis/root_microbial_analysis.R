library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)

soil<-read.csv("Microbiometer_analysis/Microbiometer_Data_complete.csv")
soil<-rename(soil, treatment=treatment..W.C.)
roots <- read.csv("Root_cores_analysis/Alpine_cores_names_2023_Complete.csv")
roots<-rename(roots, treatment=W.C)

#clean up 
soil$plot<-as.numeric(soil$Plot)

#add doy info
unique(soil$Date)
soil$year <- lubridate::year(soil$Date)
soil$doy <- lubridate::yday(soil$Date)

#split into seasons (both years) 
soil<-group_by(soil, year)%>%
  mutate(., Season=case_when(doy<200 ~ "Early Summer",
                             doy>240 ~ "Late Summer",
                             TRUE ~"Mid Summer"))
#changing Meadow to Sedge
roots <- mutate(roots, Subsite = if_else(Subsite== "Meadow", "Sedge", Subsite))

#adding new colums with duration of cores in the ground and seasons
roots <-mutate(roots, Time_in_ground=interval(mdy(Date.of.installment), mdy(Date.of.removal))) %>% 
  mutate(., Duration= int_length(Time_in_ground)/86400) %>%
  mutate(., Season=case_when(Duration<360 ~ "Early Summer",
                             Duration>400 ~ "Late Summer",
                             TRUE ~"Mid Summer"))

#make dataframes match
names(roots)
names(soil)
str(roots)
str(soil)
unique(soil$Plot)
unique(roots$Plot)

soil$Subsite<-soil$Site
soil$Site<-NULL
roots$year<-2023

soilx<-dplyr::select(soil,Subsite, treatment, microbial.biomass.C..ug.g., F.B, Plot, year, doy, Season)
rootsx<-dplyr::select(roots, Subsite, treatment,Root..g.bulk.dens., GWC.., Plot, year, Season)
rootsx$Rootscub<-rootsx$Root..g.bulk.dens.^(1/3)
hist(rootsx$Rootscub)
hist(log(soilx$microbial.biomass.C..ug.g.))

all<-left_join(soilx, rootsx)

plot(all$microbial.biomass.C..ug.g., all$Root..g.bulk.dens.)
plot( all$Root..g.bulk.dens., all$microbial.biomass.C..ug.g.)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=Rootscub, fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Root biomass (g/bulk dens)") 

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=Root..g.bulk.dens.^(1/3), fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Root biomass (g/bulk dens)") +
  facet_wrap(~Season, scales="free")

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=Root..g.bulk.dens.^(1/3), fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Root biomass (g/bulk dens)") +
  facet_wrap(~Subsite, scales="free")

summary(lm(log(microbial.biomass.C..ug.g.)~Rootscub * treatment, all))
#interaction positive relationship b/w microbial and root biomass under warming 
