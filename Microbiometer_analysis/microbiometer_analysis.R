library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)

dat<-read.csv("Microbiometer_analysis/Microbiometer_Data_complete.csv")
dat<-rename(dat, treatment=treatment..W.C.)
str(dat)

#make pre/post flood 
dat$plot<-as.numeric(dat$Plot)
unique(dat$Date)

dat$year <- lubridate::year(dat$Date)
dat$doy <- lubridate::yday(dat$Date)


#both years 
dat<-group_by(dat, year)%>%
  mutate(., Season=case_when(doy<200 ~ "Early Summer",
                             doy>240 ~ "Late Summer",
                             TRUE ~"Mid Summer"))

  #mutate(dat, doy=yday(Date))%>%
 # mutate(time=if_else(doy<250, "S1", "S2"))%>%
  #      mutate(., flood=case_when(plot<7 ~"flood",
   #                            TRUE~ "no flood"))

#dat<-mutate(dat, time=fct_relevel(time, "pre", "post"))

#trying to organize the seasons 
dat$Season <- factor(dat$Season, levels=c("Early Summer", "Mid Summer", "Late Summer"))

#Mic biomass by treatment 
# may want to log these? for normal distribution?
hist(log(dat$microbial.biomass.C..ug.g.))
          
ggplot(dat, aes(x=Site, y=log(microbial.biomass.C..ug.g.), fill=treatment))+
              geom_boxplot(aes(fill=treatment))+
             ylab("Soil microbial biomass C (ug/g)")+ theme_bw() + xlab("Plant community") +
              facet_grid(year~.+ Season) +
              labs(title = "Microbial biomass development", fill= "Treatment") +
              scale_fill_manual(values=c( "#89C5DA", "#DA5724"),name = "Treatment",  labels = c("Control", "Warming")) 
              #scale_fill_discrete(name = "Treatment",  labels = c("Control", "Warming") ) 
            
ggplot(dat, aes(x=doy, y=microbial.biomass.C..ug.g., fill=treatment))+
      geom_point(aes(fill=treatment))+
      geom_smooth(method="lm")+
      facet_wrap(~year + Site)+
      ylab("Soil microbial biomass C (ug/g)")+ theme_bw() + xlab("Day of Year") +
      scale_fill_manual(values=c( "#89C5DA", "#DA5724")) #+
      
            
microbeplot<-ggplot(subset(dat,year==2023), aes(x=doy, y=log(microbial.biomass.C..ug.g.), fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+
  facet_wrap(~Site)+
  ylab("Soil microbial biomass C (ug/g)")+ theme_bw() + xlab("Day of Year") +
  scale_fill_manual(values=c("#89C5DA", "#DA5724"), name = "Treatment",  labels = c("Control", "Warming"))


            ggplot(dat, aes(x=Site, y=microbial.biomass.C..ug.g.))+
              geom_jitter(mapping = aes(x=Site, y=microbial.biomass.C..ug.g., colour=treatment), 
                          size=1, alpha=0.5, position=position_dodge(width=1))+
              geom_pointrange(mapping = aes(x=Site, y=microbial.biomass.C..ug.g., colour=treatment),
                              stat = "summary", position=position_dodge(width=1), size=1.2)+
              scale_color_manual(values=c( "#89C5DA", "#DA5724"))
            
            ggplot(dat, aes(x=Site, y=Fungal.))+
              geom_jitter(mapping = aes(x=Site, y=microbial.biomass.C..ug.g., colour=treatment), 
                          size=1, alpha=0.5, position=position_dodge(width=1))+
              geom_pointrange(mapping = aes(x=Site, y=microbial.biomass.C..ug.g., colour=treatment),
                              stat = "summary", position=position_dodge(width=1), size=1.2)+
              scale_color_manual(values=c( "#89C5DA", "#DA5724"))
            
            ggplot(dat, aes(x=Site, y=Bacterial.))+
              geom_jitter(mapping = aes(x=Site, y=microbial.biomass.C..ug.g., colour=treatment), 
                          size=1, alpha=0.5, position=position_dodge(width=1))+
              geom_pointrange(mapping = aes(x=Site, y=microbial.biomass.C..ug.g., colour=treatment),
                              stat = "summary", position=position_dodge(width=1), size=1.2)+
              scale_color_manual(values=c( "#89C5DA", "#DA5724"))
            
#F:B by treatment     
            
          ggplot(subset(dat,year==2023), aes(x=doy, y=sqrt(F.B), fill=treatment))+
              geom_point(aes(fill=treatment))+
              geom_smooth(method="lm")+
              facet_wrap(~Site)+
              ylab("Fungal:Bacterial ratio")+ theme_bw() + xlab("Day of Year") +
              scale_fill_manual(values=c( "#89C5DA", "#DA5724"), name = "Treatment",  labels = c("Control", "Warming")) 
            
            ggplot(dat, aes(x=Site, y=F.B, fill=treatment))+
              geom_boxplot(aes(fill=treatment))+
              ylab("F:B")+ theme_bw() + xlab("Plant community") +
              facet_grid(year~.+ Season) +
              labs(title = "Fungal to bacterial ratio", fill= "Treatment") +
              scale_fill_manual(values=c( "#89C5DA", "#DA5724"),name = "Treatment",labels = c("Control", "Warming"))
              #scale_color_manual(aesthetics = "#89C5DA", "#DA5724") #+
              #facet_wrap(~Season + year)
            
            ggplot(dat, aes(x=doy, y=F.B, fill=treatment))+
              geom_point(aes(fill=treatment))+
              geom_smooth(method="lm")+
              facet_wrap(~Site)+
              ylab("F:B")+ theme_bw() + xlab("Day of Year") +
              scale_fill_manual(values=c( "#89C5DA", "#DA5724")) #+
            #facet_wrap(~Season + year)
            
             ggplot(dat, aes(x=Site, y=F.B, fill=treatment))+
            geom_boxplot(aes(fill=treatment))+
              ylab("Fungal:Bacterial ratio")+ theme_bw() + xlab("Plant community")+
              scale_fill_manual(values=c( "#89C5DA", "#DA5724")) 
            
            ggplot(dat, aes(x=Site, y=F.B))+
            geom_jitter(mapping = aes(x=Site, y=F.B, colour=treatment), 
                        size=1, alpha=0.5, position=position_dodge(width=1))+
              geom_pointrange(mapping = aes(x=Site, y=F.B, colour=treatment),
                              stat = "summary", position=position_dodge(width=1), size=1.2)+
              scale_color_manual(values=c( "#89C5DA", "#DA5724")) 
            

    ggplot(dat, aes(x=Site, y=F.B, fill=treatment))+
              geom_boxplot(aes(fill=treatment))+
              ylab("Fungal:Bacterial ratio")+ theme_bw() + xlab("Plant community")+
              scale_fill_manual(values=c( "#89C5DA", "#DA5724")) 

#plots for BC PARF talk 
#first trip vs. second trip by treatment     
#MBC
ggplot(dat, aes(x=time, y=microbial.biomass.C..ug.g., fill=treatment))+
      geom_boxplot(aes(fill=treatment))+
      ylab("Soil Microbial biomass C")+ theme_bw() + xlab("Plant community")+
      scale_fill_manual(values=c( "#89C5DA", "#DA5724")) +
      facet_wrap(~Site)
#F:B
ggplot(dat, aes(x=time, y= F.B, fill=treatment))+
  geom_boxplot(aes(fill=treatment))+
  ylab("Fungi:Bacteria")+ theme_bw() + xlab("Plant community")+
  scale_fill_manual(values=c( "#89C5DA", "#DA5724")) +
  facet_wrap(~Site)

#during vs. post flood 
#Subset for flooded plots 
#MBC
ggplot(subset(dat,Site=="Salix"&flood!="no flood"), aes(x=flood, y=microbial.biomass.C..ug.g., fill=treatment))+
  geom_boxplot(aes(fill=treatment))+
  ylab("Soil microbial biomass C (ug/g)")+ theme_bw() + xlab("Plant community")+
  scale_fill_manual(values=c( "#89C5DA", "#DA5724")) +
  facet_wrap(~time)

#F:B
ggplot(subset(dat,Site=="Salix"&flood!="no flood"), aes(x=flood, y=F.B, fill=treatment))+
  geom_boxplot(aes(fill=treatment))+
  ylab("Fungi:Bacteria")+ theme_bw() + xlab("Plant community")+
  scale_fill_manual(values=c( "#89C5DA", "#DA5724")) +
  facet_wrap(~time)

# ANOVAs
# check distribution
hist(dat$microbial.biomass.C..ug.g.)
#Check transformation to normal
hist(log(dat$microbial.biomass.C..ug.g.))
hist(log(dat$Bacterial.))
hist(log(dat$F.B))

Model1 <- aov(data = dat, formula = log(microbial.biomass.C..ug.g.) ~ treatment*Season*Site*as.factor(year))
summary(Model1)
TukeyHSD(Model1)
#Cassiope has highest microbial biomass more than sedge, salix
#2022 had more microbial biomass than 2023 (mostly for Salix)
#microbial biomass increased over summer in Cassiope (Late>early) (mostly in 2023 and in warming).

#years are very different so model separately 
Modelyear1 <- aov(data = subset(dat,year==2022), formula = log(microbial.biomass.C..ug.g.) ~ treatment*Season*Site)
summary(Modelyear1) #only site effect- Cassiope higher than sedge, salix
TukeyHSD(Modelyear1)

Modelyear2 <- aov(data = subset(dat,year==2023), formula = log(microbial.biomass.C..ug.g.) ~ treatment*Season*Site)
summary(Modelyear2) #site, treatment x season (weak), treatment x site, season x site interactions all sig 
TukeyHSD(Modelyear2)
#site- Cassiope higher mic biomass than sedge, salix
#trt x season- warming effects (increased mic biomass) strongest in late summer (p=0.08)
#trt x site - warming effect (increased mic biomass) strongest at salix (p=0.07)
#season x site - none
#trt x season x site - warming late summer > warming early summer Cassiope (p=0.09)

Modelyear2fb <- aov(data = subset(dat,year==2023), formula = sqrt(F.B) ~ treatment*Season*Site)
summary(Modelyear2fb) #site,treatment x site (weak), season x site interactions all sig 
TukeyHSD(Modelyear2)

#look at cass only
Modelcass <- aov(data = subset(dat, Site=="Cassiope"), 
                 formula = log(microbial.biomass.C..ug.g.) ~ treatment*Season*as.factor(year))
summary(Modelcass)

TukeyHSD(Modelcass)#late summer > early summer and mid summer > early summer (2023)- especially in warming, control plots did not differ


plotmic<-ggplot(subset(dat,year=2023), aes(x=doy, y=log(microbial.biomass.C..ug.g.), fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+
  #facet_wrap(~year+ Site) +
  facet_wrap(~Site) +
    scale_fill_manual(values=c( "#89C5DA", "#DA5724"),name = "Treatment",  labels = c("Control", "Warming")) +
  theme_bw()

# F:B
#check distribution
hist(dat$F.B)
# transforming the data (cube root)
dat$F.Bcubr <- (dat$F.B^(1/3))
# running the model
ModelFB <- aov(data = dat, formula = F.Bcubr ~ treatment*Season*Site)
summary(ModelFB)   
TukeyHSD(ModelFB)

#Linear model biomass
Linear1 <- lm(data = dat, formula = log(microbial.biomass.C..ug.g.) ~ doy*Site*treatment)
summary(Linear1)
hist(dat$microbial.biomass.C..ug.g.)

#Linear model F:B
Linear2 <- lm(data = dat, formula = F.Bcubr ~ doy*Site*treatment)
summary(Linear2)



