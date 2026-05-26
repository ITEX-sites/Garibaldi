library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)

#read in data & combine----
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

#adding new columns with duration of cores in the ground and seasons
roots <-mutate(roots, Time_in_ground=interval(mdy(Date.of.installment), mdy(Date.of.removal))) %>% 
  mutate(., Duration= int_length(Time_in_ground)/86400) %>%
  mutate(., Season=case_when(Duration<360 ~ "Early Summer",
                             Duration>400 ~ "Late Summer",
                             TRUE ~"Mid Summer"))

#make dataframes match
roots$Site<-NULL
roots<-rename(roots, Site=Subsite)
roots$year<-2023

soilx<-dplyr::select(soil,Site, treatment, microbial.biomass.C..ug.g., F.B, Plot, year, doy, Season, Notes)
rootsx<-dplyr::select(roots, Site, treatment,Root..g.bulk.dens., GWC.., Plot, year, Season, Duration)
all<-left_join(soilx, rootsx)

#read in TOMST data 
microclimALL<-read.csv("TOMST_analysis/TOMST_2022_2023_daily.csv")

#remove values prior to installation for Sentinel plots 
microclimALLx<-subset(microclimALL, datetime=='2022-07-21' & Site=="Sedge")
microclimALLxx<-subset(microclimALL, datetime=='2023-06-26' & Site=="Sedge")
microclimALLx<-rbind(microclimALLx, microclimALLxx)

microclimALL<-anti_join(microclimALL, microclimALLx)
rm(microclimALLx, microclimALLxx)

microclimALL$doy<-lubridate::yday(microclimALL$datetime)
microclimALL$year<-lubridate::year(microclimALL$datetime)
microclimALL$datetime<-NULL
microclimALL$time_to<-NULL
microclimALL$X<-NULL

#join tomst data with root & soil 
all<-left_join(all, microclimALL) #can update microclimate values to 2 week windows?
#all<-mutate(all, Season = fct_relevel(Season, "Early Summer", "Mid Summer", "Late Summer"))#doing weird things- maybe just manually adjust in ggplot

#visualizations----
## Plots for temp and moisture loggers ####
microclimALL <- microclimALL %>% drop_na(T1)

# plot with all temp trackers
microclimALL_long <- microclimALL %>%
  pivot_longer(cols = c(T1, T2, T3), names_to = "Temperature_Measure", values_to = "Temperature")

ggplot(microclimALL_long, aes(x = doy, y = Moisture, color = treatment)) + #you can see flooding event in Salix in 2022
  geom_point() + geom_smooth()+
  facet_wrap(~year+ Site) +
  scale_color_manual(values=c("C" = "#89C5DA","W" = "#DA5724"))  +
  theme_minimal() +
  labs(y = "Average Daily Moisture", x = "DOY")+ ggtitle("Soil Moist")

ggplot(microclimALL_long, aes(x = as.factor(year), y = Moisture, color = treatment)) +
  geom_boxplot() +
  facet_wrap(~Site) +
  scale_color_manual(values=c("C" = "#89C5DA","W" = "#DA5724"))  +
  theme_minimal() +
  labs(y = "Average Daily Moisture", x = "DOY")+ ggtitle("Soil Moist")

ggplot(subset(microclimALL_long,Temperature_Measure=="T1"), aes(x = doy, y = Temperature, color = treatment)) +
  geom_point() + geom_smooth()+
  facet_wrap(~Site) +
  scale_color_manual(values=c("C" = "#89C5DA","W" = "#DA5724"))  +
  theme_minimal() +
  #geom_text(data = sig_labels, aes(x = treatment, y = Moisture, label = label), size = 4, color = "black")+ 
  labs(y = "Average Daily Temp", x = "DOY")+ ggtitle("T1- Soil temp -6cm")

ggplot(subset(microclimALL_long,Temperature_Measure=="T2"), aes(x = doy, y = Temperature, color = treatment)) +
  geom_point() + geom_smooth()+
  facet_wrap(~ Site) +
  scale_color_manual(values=c("C" = "#89C5DA","W" = "#DA5724"))  +
  theme_minimal() +
  #geom_text(data = sig_labels, aes(x = treatment, y = Moisture, label = label), size = 4, color = "black")+ 
  labs(y = "Average Daily Temp", x = "DOY")+ ggtitle("T2-ground temp 2cm ")

ggplot(subset(microclimALL_long,Temperature_Measure=="T3"), aes(x = doy, y = Temperature, color = treatment)) +
  geom_point() + geom_smooth()+
  facet_wrap(~Site) +
  scale_color_manual(values=c("C" = "#89C5DA","W" = "#DA5724"))  +
  theme_minimal() +
  #geom_text(data = sig_labels, aes(x = treatment, y = Moisture, label = label), size = 4, color = "black")+ 
  labs(y = "Average Daily Temp", x = "DOY")+ ggtitle("T3-Air temp 15cm")

ggplot(subset(microclimALL_long,Temperature_Measure=="T1"), aes(x = doy, y = Temperature, color = treatment)) +
  geom_point() + geom_smooth()+
  facet_wrap(~year+ Site) +
  scale_color_manual(values=c("C" = "#89C5DA","W" = "#DA5724"))  +
  theme_minimal() +
  #geom_text(data = sig_labels, aes(x = treatment, y = Moisture, label = label), size = 4, color = "black")+ 
  labs(y = "Average Daily Temp", x = "DOY")+ ggtitle("T1- Soil temp -6cm")

ggplot(subset(microclimALL_long,Temperature_Measure=="T2"), aes(x = doy, y = Temperature, color = treatment)) +
  geom_point() + geom_smooth()+
  facet_wrap(~year+ Site) +
  scale_color_manual(values=c("C" = "#89C5DA","W" = "#DA5724"))  +
  theme_minimal() +
  #geom_text(data = sig_labels, aes(x = treatment, y = Moisture, label = label), size = 4, color = "black")+ 
  labs(y = "Average Daily Temp", x = "DOY")+ ggtitle("T2-ground temp 2cm ")

ggplot(subset(microclimALL_long,Temperature_Measure=="T3"), aes(x = doy, y = Temperature, color = treatment)) +
  geom_point() + geom_smooth()+
  facet_wrap(~year+ Site) +
  scale_color_manual(values=c("C" = "#89C5DA","W" = "#DA5724"))  +
  theme_minimal() +
  #geom_text(data = sig_labels, aes(x = treatment, y = Moisture, label = label), size = 4, color = "black")+ 
  labs(y = "Average Daily Temp", x = "DOY")+ ggtitle("T3-Air temp 15cm")


#KEY FIGURES
ggplot(all, aes(y=Rootscub, Duration, fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Root g/bulk density")+ theme_bw() + xlab("doy") +
  facet_wrap(~Site)

ggplot(subset(all,year>2022), aes(y=microbial.biomass.C..ug.g., doy, fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("doy") +
  facet_wrap(~Site)

ggplot(subset(all,year>2022), aes(y=F.B, doy, fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Fungal:Bacterial")+ theme_bw() + xlab("doy") +
  facet_wrap(~Site)

ggplot(all, aes(y=Rootscub, x=microbial.biomass.C..ug.g.))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Root g/bulk density")+ theme_bw() + xlab("Microbial biomass C (ug/g)") +
  facet_wrap(~Site, scales="free")

ggplot(all, aes(y=Rootscub, x=microbial.biomass.C..ug.g.))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Root g/bulk density")+ theme_bw() + xlab("Microbial biomass C (ug/g)") +
  facet_wrap(~treatment, scales="free")

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=T1, fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Soil T -6cm") +
  facet_wrap(~Site)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=T2, fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Soil T +2 cm") +
  facet_wrap(~Site)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=T3, fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Soil T + 15cm") +
  facet_wrap(~Site)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=T2, fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Soil Temp") #+
  #facet_wrap(~Subsite)

ggplot(all, aes(y=Rootscub, x=T2, fill=treatment))+#confounded by doy 
  geom_point(aes(color=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
scale_color_manual(values=c( "#89C5DA", "#DA5724"))  #+
# ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Soil Temp") #+
#facet_wrap(~Subsite)

ggplot(all, aes(y=Rootscub, x=T1, fill=treatment))+
  geom_point()+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  #+

ggplot(all, aes(y=Rootscub, x=T3, fill=treatment))+
  geom_point()+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  #+

ggplot(all, aes(y=Rootscub, x=Site))+
  geom_boxplot()

ggplot(all, aes(y=Rootscub, x=treatment))+
  geom_boxplot()

ggplot(all, aes(y=Rootscub, x=Moisture, fill=treatment))+
  geom_point()+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  #+

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=Duration))+
  geom_point()+
  geom_smooth(method="lm")+ 
  #scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Soil Temp") #+
#facet_wrap(~Subsite)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=T2, fill=tre))+
  geom_point(aes(fill=Season))+
  geom_smooth(method="lm")+ 
  #scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Soil Temp") #+
#facet_wrap(~Subsite)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=T2, fill=tre))+
  geom_point(aes(fill=Season))+
  geom_smooth(method="lm")+ 
  #scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Soil Temp") #+
#facet_wrap(~Subsite)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=T2, fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Soil Temp") #+
#facet_wrap(~Subsite)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=T3, fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Soil Temp") #+
#facet_wrap(~Subsite)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=T1, fill=Subsite))+
  geom_point(aes(fill=Subsite))+
  geom_smooth(method="lm")+ 
  #scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Soil Temp") #+
#facet_wrap(~Subsite)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=Moisture, fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Soil Moisture") +
  facet_wrap(~Site, scales="free")

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
  facet_wrap(~Season)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=Root..g.bulk.dens.^(1/3), fill=Season))+ #relationship changes across growing season from negative to positive 
  geom_point(aes(fill=Season))+
  geom_smooth(method="lm")+ 
  #scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Root biomass (g/bulk dens)") #+
  #facet_wrap(~Site)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=Root..g.bulk.dens.^(1/3)))+
  geom_point(aes(color=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Root biomass (g/bulk dens)") +
  facet_wrap(~Site, scales="free")

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=Root..g.bulk.dens.^(1/3)))+
  geom_point(aes(color=Season))+
  geom_smooth(method="lm")+ 
  #scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Root biomass (g/bulk dens)") +
  facet_wrap(~Site, scales="free")

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=Root..g.bulk.dens.^(1/3)), fill=Season)+
  geom_point(aes(fill=Season))+
  geom_smooth(method="lm")+ 
  #scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Root biomass (g/bulk dens)") +
  facet_wrap(~Site, scales="free")

all_long<-pivot_longer(all, cols=c("Rootscub", "logmic"), names_to = "type", values_to = "biomass")%>%select(-Notes)%>%subset(!is.na(biomass))%>%distinct(.)
ggplot(all_long, aes(y=biomass, x=doy, fill=type))+
  geom_point( )+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  #ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Root biomass (g/bulk dens)") +
  facet_wrap(~Site, scales="free")

ggplot(all_long, aes(y=biomass, x=doy, fill=type))+
  geom_point( )+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724")) # +
  #ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Root biomass (g/bulk dens)") +
  #facet_wrap(~Site, scales="free")

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=as.factor(year) , fill=Season))+
  geom_boxplot() +  facet_wrap(~Site)
  
ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=treatment , fill=Season))+
  geom_boxplot() +  facet_wrap(~year+ Site)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=treatment , fill=Season))+
  geom_boxplot() +  facet_wrap(~Site)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=treatment ))+
  geom_boxplot() +  facet_wrap(~Site)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=Site , fill=Season))+
  geom_boxplot() #+  facet_wrap(~Site)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=Season , fill=Season))+
  geom_boxplot() +  facet_wrap(~Site)

ggplot(all, aes(y=fblog, x=Season , fill=as.factor(year)))+
  geom_boxplot() +  facet_wrap(~Site)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=Site , fill=treatment))+
  geom_boxplot() #+  facet_wrap(~Site)

ggplot(all, aes(y=logmic, x=Site , fill=treatment))+
  geom_boxplot() #+  facet_wrap(~Site)

ggplot(all, aes(y=Rootscub, x=Site , fill=treatment))+
  geom_boxplot() #+  facet_wrap(~Site)

ggplot(all, aes(y=Rootscub, x=treatment , fill=treatment))+
  geom_boxplot() #+  facet_wrap(~Site)

ggplot(all, aes(y=logmic, x=treatment , fill=treatment))+
  geom_boxplot() #+  facet_wrap(~Site)

ggplot(subset(all,year>2022), aes(y=logmic, x=Season ))+
  geom_boxplot() #+  facet_wrap(~Site)

ggplot(subset(all), aes(y=logmic, x=Season ))+
  geom_boxplot() #+  facet_wrap(~Site)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=treatment , fill=Season))+
  geom_boxplot() +  facet_wrap(~Site)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=doy, fill= treatment))+
  geom_point() + geom_smooth(method='lm')+  facet_wrap(~Site)

#statistical tests----
#check for normality of root/microbial response vars
all$Rootscub<-all$Root..g.bulk.dens.^(1/3)
hist(all$Rootscub)
hist(log(all$microbial.biomass.C..ug.g.))
all$logmic<-log(all$microbial.biomass.C..ug.g.)
hist(log(all$F.B))
all$fblog<-log(all$F.B+0.01)

#we asked:  1) How does alpine tundra root growth and soil microbial biomass change across the growing season?
#Season x Community 
roots1<-aov(Rootscub~Season*Site*treatment , all)  
summary(roots1)
TukeyHSD(roots1)

roots2<-aov(Rootscub~Season*treatment , all) 
summary(roots2)
TukeyHSD(roots2)

roots3<-aov(Rootscub~Site*treatment , all) 
summary(roots3)
TukeyHSD(roots3)

mic0<-(lmer(logmic~Season*Site*treatment + (1|Plot), subset(all, year>2022))) 
anova(mic0)# Site, Season x Site, Season x treatment, Site x treatment (weak- not part of Q1)
em0 <- emmeans::emmeans(mic0, "Site")
emmeans::contrast(em0, "pairwise", adjust = "Tukey")
em2 <- emmeans::emmeans(mic0, pairwise~ Season|Site)
em2
em3 <- emmeans::emmeans(mic0, pairwise~ Season|treatment)
em3
em4 <- emmeans::emmeans(mic0, pairwise~ Season|treatment|Site) 
em4 #increase in mic biomass in cassiope over growing season in warming plots 
em5 <- emmeans::emmeans(mic0, pairwise~ treatment|Site|Season ) 
em5 
r2glmm::r2beta(mic0) #r2= 0.58

fb0<-(lmer(fblog~Season*Site*treatment + (1|Plot), subset(all, year>2022))) 
anova(fb0)# Season (weak), Site, Season x Site, Season x treatment (weak)
em0 <- emmeans::emmeans(fb0, "Site")
emmeans::contrast(em0, "pairwise", adjust = "Tukey")
em1 <- emmeans::emmeans(fb0, "Season")
emmeans::contrast(em1, "pairwise", adjust = "Tukey") #early>late p=0.07
em2 <- emmeans::emmeans(fb0, pairwise~ Season|Site)
em2
em3 <- emmeans::emmeans(mic0, pairwise~ Season|treatment)
em3
em4 <- emmeans::emmeans(fb0, pairwise~ Season|treatment|Site) 
em4 
r2glmm::r2beta(fb0) #r2= 0.53

mic1<-(lmer(logmic~Season*Site*treatment + (1|year) + (1|Plot), all)) 
anova(mic1)# Site, Season x Site, Season x treatment (weak), Site x treatment (weak- not part of Q1)
em1 <- emmeans::emmeans(mic1, "Site")
emmeans::contrast(em1, "pairwise", adjust = "Tukey")
em2 <- emmeans::emmeans(mic1, pairwise~ Season|Site)
em2
em3 <- emmeans::emmeans(mic1, pairwise~ Season|treatment)
em3
em4 <- emmeans::emmeans(mic1, pairwise~ Season|treatment|Site) 
em4 #increase in mic biomass in cassiope over growing season in warming plots 

r2glmm::r2beta(mic1) #r2= 0.47

fb1<-(lmer(fblog~Season*Site*treatment + (1|year) + (1|Plot), all)) 
anova(fb1)# Site, Season x Site  
em1.1 <- emmeans::emmeans(fb1, "Site")
emmeans::contrast(em1.1, "pairwise", adjust = "Tukey")
em2.1 <- emmeans::emmeans(fb1, pairwise~ Season|Site)
em2.1
em2.2 <- emmeans::emmeans(fb1, pairwise~ Season|treatment|Site) 
em2.2#weak decrease in fb in Salix over growing season in warming plots 
#increase in fb biomass in cassiope over growing season in warming plots 

r2glmm::r2beta(fb1) #r2= 0.4


#2) Are the timing of root growth and soil microbial biomass linked in the alpine? 
mod1<-(lm(logmic~Rootscub*Site, all)) 
summary(mod1)#R2=0.67 Salix neg, Cassiope pos
emmeans::emtrends(mod1, pairwise~Site, var="Rootscub") #Cassiope pos relationship, Salix/Sedge neg, none sig diff from zero, Cassiope>Salix (p=0.09)

mod2<-(lm(logmic~Rootscub*treatment, all)) #r2=0.07 warming pos 
summary(mod2)#R2=0.07 warming weak pos
emmeans::emtrends(mod2, pairwise~treatment, var="Rootscub") #control neg, warming pos, non sig diff from zero, warming > control p=0.07


#3) How does warming and plant community type influence these relationships? (interactions above)

#4) are changes atributable to microclimate drivers?

##2) Are the timing of root growth and soil microbial biomass linked in the alpine? How do these relationships differ across plant community types?

summary(lm(logmic~Rootscub*treatment*Site , all)) #salix and sedge have negative relationship, Cassiope positive relationship, no int with warming
summary(lm(logmic~Rootscub*doy*Site , all)) #salix and sedge have negative relationship, Cassiope positive relationship, no int with warming

summary(lm(Rootscub~logmic*Season, all)) # weak interaction late summer positive relationship

summary(lm(logmic~Rootscub*treatment, all)) #relationship between root pos in warming
summary(lm(logmic~Rootscub*Site*treatment, all)) #relationship between root and microbial growth differs by community (pos in cassiope, neg in salix, sedge)

aov5<-aov(logmic~Rootscub*Season*Site, all) #NS
summary(aov5)
TukeyHSD(aov5)

##3) How does warming and soil microclimate influence these relationships across alpine plant community types?
summary(lm(logmic~year, all)) #weak interannual signal 

summary(lm(logmic~treatment*Season*Site, all)) #higher microbial biomass in 2022 - maybe due to flooding event in Salix? 

summary(lm(logmic~treatment*Season*Site, all)) #higher microbial biomass in 2022 - maybe due to flooding event in Salix? 

summary(lm(logmic~treatment*year, all)) #higher microbial biomass in 2022 - maybe due to flooding event in Salix? 
summary(lm(logmic~treatment*Season*Site, all)) #no seasonal signal overall or by treatment (even when accounting for differences among years)

#2. Do the effects of warming vary by plant community type? Here can look at different warming depths b/c vary by community  

#can't look at interaction between soil moisture and community type because highly confounded - need to run separately 
summary(lmer(logmic~scale(T1)*Site+ (1|Season), all)) #singular fit
summary(lmer(logmic~T2*Site+ (1|Season), all)) #positive effect of ground temp x Salix, weak positive in sedge 
summary(lmer(logmic~T3*Site+ (1|Season), all)) #positive effect of air temp x Salix and air temp x sedge  

#could be due to higher soil temps in Salix (T1) and/or flooding event (soil moisture)
summary(lm(logmic~T1*Site , all)) 
summary(lm(logmic~scale(Moisture)*Site , all)) ##can't look at interaction between soil moisture and community type because highly confounded - need to run separately 

#How did warming treatments affect microclimates across sites? 
#check normality of microclimate vars
hist((microclimALL$T1))
hist((microclimALL$T2))
hist((microclimALL$T3))
hist((microclimALL$Moisture))
rcompanion::transformTukey(microclimALL$Moisture)#suggests ^1 so no transform 
#include plot as random effect- adding year random effect led to singular fit 
summary(lmer(T1~treatment*Site + (1|Plot), microclimALL))#salix>Cassiope soil temp, weak pos effect of warming ~0.75 C 
summary(lmer(T2~treatment*Site + (1|Plot), microclimALL))#pos effect of warming ~1.2 C on ground surface temps, no difference b/w communities
summary(lmer(T3~treatment*Site + (1|Plot), microclimALL))##sedge has lowest air temp, pos effect of warming ~1C
summary(lmer(Moisture~treatment*Site + (1|Plot), microclimALL))#sedge has highest moisture, no effect of warming

test1<-(aov(T1~treatment*Site, microclimALL))
summary(test1)
TukeyHSD(test1)#warming weakest in salix ~0.6 vs 0.75 in Cassiope, Sedge 
test2<-(aov(T2~treatment*Site, microclimALL))
summary(test2)
TukeyHSD(test2)#warming strongest in salix ~1.35C
test3<-(aov(T3~treatment*Site, microclimALL))
summary(test3)
TukeyHSD(test3) #warming only ~0.73 degrees in sedge, 1C in Salix, Cassiope

#interannaul effects? yes temp varied by location
summary(lmer(T1~year + (1|Plot), microclimALL))#2023 was warmer than 2022 
summary(lmer(T2~year + (1|Plot), microclimALL))#no difference
summary(lmer(T3~year + (1|Plot), microclimALL))#2023 was colder than 2022 
summary(lmer(Moisture~year+ (1|Plot), microclimALL))#2023 was drier than 2022 - mostly driven by Salix differences
summary(lmer(Moisture~Site*scale(year)+ (1|Plot), microclimALL))#mostly driven by Salix differences (flooding event in 2022, dried out in 2023)


#what about soil moisture? have to run communities separately
summary(lm(Rootscub~scale(Moisture)*treatment,  subset(all, Site=="Cassiope"))) #weak pos effect of moisture
summary(lm(Rootscub~scale(Moisture)*treatment,  subset(all, Site=="Salix")))  #weak neg effect of moisture
summary(lm(Rootscub~scale(Moisture)*treatment,  subset(all, Site=="Sedge")))  #nO EFFECT

summary(lm(logmic~scale(Moisture)*treatment,  subset(all, Site=="Cassiope"))) ##nO EFFECT
summary(lm(logmic~scale(Moisture)*treatment,  subset(all, Site=="Salix")))  #pos effect of moisture & warming- no interaction
summary(lm(logmic~scale(Moisture)*treatment,  subset(all, Site=="Sedge")))  #weak neg effect of moisture