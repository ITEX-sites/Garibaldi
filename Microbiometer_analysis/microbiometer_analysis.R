library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)

dat<-read.csv("Microbiometer_Data_complete.csv")
dat<-rename(dat, treatment=treatment..W.C.)
str(dat)

#make pre/post flood 
dat$plot<-as.numeric(dat$Plot)
unique(dat$Date)

dat$year <- lubridate::year(dat$Date)
dat$doy <- lubridate::yday(dat$Date)


#S1= first sampling S2= second sampling
dat<-group_by(dat, year)%>%
  mutate(., Season=case_when(doy<200 ~ "Early Summer",
                             doy>240 ~ "Late Summer",
                             TRUE ~"Mid Summer"))

  #mutate(dat, doy=yday(Date))%>%
 # mutate(time=if_else(doy<250, "S1", "S2"))%>%
  #      mutate(., flood=case_when(plot<7 ~"flood",
   #                            TRUE~ "no flood"))

#dat<-mutate(dat, time=fct_relevel(time, "pre", "post"))

#Mic biomass by treatment 
            ggplot(dat, aes(x=Site, y=microbial.biomass.C..ug.g., fill=treatment))+
              geom_boxplot(aes(fill=treatment))+
             ylab("Soil Microbial biomass C")+ theme_bw() + xlab("Plant community") +
            scale_fill_manual(values=c( "#89C5DA", "#DA5724")) +
              facet_wrap(~Season + year)
            
            ggplot(dat, aes(x=doy, y=microbial.biomass.C..ug.g., fill=treatment))+
              geom_point(aes(fill=treatment))+
              geom_smooth(method="lm")+
              facet_wrap(~Site)+
              ylab("Soil Microbial biomass C")+ theme_bw() + xlab("Day of Year") +
              scale_fill_manual(values=c( "#89C5DA", "#DA5724")) #+
              #facet_wrap(~Season + year)
            
            ggplot(dat, aes(x=Site, y=microbial.biomass.C..ug.g.))+
              geom_jitter(mapping = aes(x=Site, y=microbial.biomass.C..ug.g., colour=treatment), 
                          size=1, alpha=0.5, position=position_dodge(width=1))+
              geom_pointrange(mapping = aes(x=Site, y=microbial.biomass.C..ug.g., colour=treatment),
                              stat = "summary", position=position_dodge(width=1), size=1.2)+
              scale_color_manual(values=c( "#89C5DA", "#DA5724")) 
            
#F:B by treatment             
            ggplot(dat, aes(x=Site, y=F.B, fill=treatment))+
              geom_boxplot(aes(fill=treatment))+
              ylab("F:B")+ theme_bw() + xlab("Plant community") +
              scale_fill_manual(values=c( "#89C5DA", "#DA5724")) +
              facet_wrap(~Season + year)
            
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
  ylab("Soil Microbial biomass C")+ theme_bw() + xlab("Plant community")+
  scale_fill_manual(values=c( "#89C5DA", "#DA5724")) +
  facet_wrap(~time)

#F:B
ggplot(subset(dat,Site=="Salix"&flood!="no flood"), aes(x=flood, y=F.B, fill=treatment))+
  geom_boxplot(aes(fill=treatment))+
  ylab("Fungi:Bacteria")+ theme_bw() + xlab("Plant community")+
  scale_fill_manual(values=c( "#89C5DA", "#DA5724")) +
  facet_wrap(~time)

# ANOVAs
Model1 <- aov(data = dat, formula = microbial.biomass.C..ug.g. ~ treatment*Season*Site)
summary(Model1)
TukeyHSD(Model1)

#look at cass only
Modelcass <- aov(data = subset(dat, Site=="Cassiope"), 
                 formula = microbial.biomass.C..ug.g. ~ treatment*Season*as.factor(year))
summary(Modelcass)
TukeyHSD(Modelcass)

# F:B
ModelFB <- aov(data = dat, formula = F.B ~ treatment*Season*Site)
summary(ModelFB)                 
