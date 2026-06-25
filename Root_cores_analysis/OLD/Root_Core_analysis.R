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

#OLD/Other----

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

#create density plots from Codyn VR estimates
# Example CI bounds
lower <- 45.2
upper <- 54.8
n     <- 18  # assumed sample size

se_val   <- (1.478 - 0.443) / (2 * 1.96)  # 1.96 for 95% CI

# Step 2: Convert SE to standard deviation
sd_val <- se_val * sqrt(n)


df0 <- data.frame(value = rnorm(1000, mean = 1.015, sd = 0.886))
df0$Site="All"
df0$estimate<-1.024
df <- data.frame(value = rnorm(1000, mean = 0.995, sd = 0.958))
df$Site="Heather"
df$estimate<-1.579
df1<-data.frame(value = rnorm(1000, mean = 1.003, sd = 0.766))
df1$Site="Salix"
df1$estimate<-0.583
df2<-data.frame(value = rnorm(1000, mean = 1.014, sd = 0.964))
df2$Site="Sedge"
df2$estimate<-0.911
df3<-data.frame(value = rnorm(1000, mean = 0.987, sd = 1.068))
df3$Site="Warming"
df3$estimate<-1.238
df4<-data.frame(value = rnorm(1000, mean = 1.003, sd = 1.12))
df4$Site="Control"
df4$estimate<-0.81

df<-rbind(df0, df, df1, df2, df3, df4)
library(ggridges)
ggplot(df, aes(x = value, y=Site, , fill = Site, color=Site)) +
  geom_pointrange() +
  geom_point(aes(x=estimate))+
  scale_fill_brewer(palette = "Set1", name = "Group") + 
  scale_color_brewer(palette = "Set1", ) + xlab("Variance Ratio") + guides(color = "none")+
  theme_minimal()+ geom_vline(xintercept = 1, linetype = 2, color="black")

mod1<-(lm(logmic~Rootscub*Site, all)) 
summary(mod1)#R2=0.67 Salix neg, Cassiope pos
emm1<-emmeans::emtrends(mod1, pairwise~Site, var="Rootscub") #Cassiope pos relationship, Salix/Sedge neg, none sig diff from zero, Cassiope>Salix (p=0.09)

emm1<-as.data.frame(emm1)
emm1$contrast<-NULL
emm1<-subset(emm1, Site!=".")
df <- data.frame(value = rnorm(1000, mean = 0.497, sd = 0.52))
df$Site="Cassiope"
df1<-data.frame(value = rnorm(1000, mean = -1.734, sd = 0.869))
df1$Site="Salix"
df2<-data.frame(value = rnorm(1000, mean = -1.327, sd = 1.009))
df2$Site="Sedge"
df<-rbind(df, df1, df2)

ggplot(df, aes(x = value, fill = Site, color=Site)) +
  geom_density(alpha=0.3) +
  scale_fill_brewer(palette = "Set1", name = "Community") + 
  scale_color_brewer(palette = "Set1", ) + xlab("Slope") + guides(color = "none")+
  theme_minimal()+ geom_vline(xintercept = 0, linetype = 2, color="black")

#not correlatedNULL#not correlated
cor.test(all$logmic, all$Rootscub)
cor.test(all$fblog, all$Rootscub)

mod2<-(lm(logmic~Rootscub*treatment, all)) #r2=0.07 warming pos 
summary(mod2)#R2=0.07 warming weak pos
emmeans::emtrends(mod2, pairwise~treatment, var="Rootscub") #control neg, warming pos, non sig diff from zero, warming > control p=0.07

mod3<-(lm(fblog~Rootscub*Site, all))
summary(mod3)
emmeans::emtrends(mod3, pairwise~Site, var="Rootscub") 

mod4<-(lm(fblog~Rootscub*treatment, all))
summary(mod4)
emmeans::emtrends(mod4, pairwise~treatment, var="Rootscub") #control neg, warming pos, non sig diff from zero, warming > control p=0.07




#3) How does warming and plant community type influence these relationships? (interactions above)

#4) are changes atributable to microclimate drivers?

##2) Are the timing of root growth and soil microbial biomass linked in the alpine? How do these relationships differ across plant community types?
summary(lm(logmic~Rootscub*treatment*Site , all)) #salix and sedge have negative relationship, Cassiope positive relationship, no int with warming

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
#also can't include multiple temps in one model bc highly covary 
summary(lm(logmic~scale(Moisture), subset(all, year>2022&Site=="Salix"))) #singular fit
summary(lm(logmic~scale(T1)*doy+ scale(Moisture), subset(all, year>2022&Site=="Cassiope")))
summary(lm(logmic~scale(T2)*doy+ scale(Moisture), subset(all, year>2022&Site=="Cassiope")))
summary(lm(logmic~scale(T3)+ scale(Moisture), subset(all, year>2022&Site=="Cassiope")))
summary(lm(logmic~scale(T1)+ scale(Moisture), subset(all, year>2022&Site=="Salix")))
summary(lm(logmic~scale(T2)+ scale(Moisture), subset(all, year>2022&Site=="Salix")))
summary(lm(logmic~scale(T3)+ scale(Moisture), subset(all, year>2022&Site=="Salix")))
summary(lm(logmic~scale(T1)+ scale(Moisture), subset(all, year>2022&Site=="Salix")))
summary(lm(logmic~scale(T2)+ scale(Moisture), subset(all, year>2022&Site=="Sedge"))) 
summary(lm(logmic~scale(T3)+ scale(Moisture), subset(all, year>2022&Site=="Salix")))



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


ggplot(subset(all,year>2022), aes(y=F.B, doy, fill=treatment))+
  geom_point(aes(fill=treatment, color=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  scale_color_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Fungal:Bacterial")+ theme_bw() + xlab("doy") +
  facet_wrap(~Site)

ggplot(all, aes(y=Rootscub, x=microbial.biomass.C..ug.g.))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Root g/bulk density")+ theme_bw() + xlab("Microbial biomass C (ug/g)") +
  facet_wrap(~Site, scales="free")

ggplot(all, aes(y=Rootscub, x=microbial.biomass.C..ug.g., fill=treatment))+
  geom_point(aes(fill=treatment, color=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  scale_color_manual(values=c( "#89C5DA", "#DA5724"))  +
  
  ylab("Root g/bulk density")+ theme_bw() + xlab("Microbial biomass C (ug/g)") #+
#facet_wrap(~treatment, scales="free")

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

ggplot(all, aes(y=fblog, x=Rootscub, fill=treatment))+
  geom_point(aes(fill=treatment))+
  geom_smooth(method="lm")+
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Root biomass (g/bulk dens)") 

ggplot(all, aes(y=fblog, x=Rootscub))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Fungal:Bacterial")+ theme_bw() + xlab("Root biomass (g/bulk dens)") + facet_wrap(~treatment+Site)

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

ggplot(subset(all_long,year>2022), aes(y=biomass, x=doy, fill=type))+
  geom_point( )+
  geom_smooth(method='lm')+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  #ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Root biomass (g/bulk dens)") +
  facet_wrap(~treatment+ Site, scales="free")

ggplot(all_long, aes(y=biomass, x=doy, fill=type))+
  geom_point( )+
  geom_smooth()+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724")) # +
#ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("Root biomass (g/bulk dens)") +
#facet_wrap(~Site, scales="free")

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=as.factor(year) , fill=Season))+
  geom_boxplot() +  facet_wrap(~Site)

ggplot(all, aes(y=log(microbial.biomass.C..ug.g.), x=treatment , fill=Season))+
  geom_boxplot() +  facet_wrap(~year+ Site)

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
