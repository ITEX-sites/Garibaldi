library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)

#read in data & combine----
soil<-read.csv("Root_cores_analysis/Microbiometer_Data_complete.csv")
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
#update naming
roots <- mutate(roots, Subsite = if_else(Subsite== "Meadow", "Sedge", Subsite))
roots <- mutate(roots, Subsite = if_else(Subsite== "Cassiope", "Heather", Subsite))
soil <- mutate(soil, Site = if_else(Site== "Cassiope", "Heather", Site))

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
microclimALL<-read.csv("Root_cores_analysis/TOMST_2022_2023_daily.csv")
microclimALL <- mutate(microclimALL, Site = if_else(Site== "Cassiope", "Heather", Site))

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

microclimALL<-group_by(microclimALL, year)%>%
  mutate(., Season=case_when(doy<200 ~ "Early Summer",
                             doy>240 ~ "Late Summer",
                             TRUE ~"Mid Summer"))

#calibrate soil moisture with relevant curves 
#https://tomst.com/web/wp-content/uploads/Doc/Calibration-set-TMS3.pdf
microclimALL<-mutate(microclimALL, VWC =case_when(
                     Site=="Sedge"~  0.000000017*(Moisture)^2 + 0.000118119*(Moisture) - 0.101168511, 
                     Site== "Salix"~ -0.000000019*(Moisture)^2 + 0.000265610*(Moisture) - 0.154089291,
                     Site== "Heather"~ -0.000000009*(Moisture)^2  + 0.000261847*(Moisture) - 0.158618303, TRUE~0))


#join tomst data with root & soil 
all<-left_join(all, microclimALL) #can update microclimate values to 2 week windows?
#all<-mutate(all, Season = fct_relevel(Season, "Early Summer", "Mid Summer", "Late Summer"))#doing weird things- maybe just manually adjust in ggplot

#check for normality of root/microbial response vars
all$Rootscub<-all$Root..g.bulk.dens.^(1/3)
hist(all$Rootscub)
hist(log(all$microbial.biomass.C..ug.g.))
all$logmic<-log(all$microbial.biomass.C..ug.g.)

hist(log(all$F.B))
all$fblog<-log(all$F.B+0.01)
hist(all$fblog)

#visualizations----
microclimALL <- microclimALL %>% drop_na(T1)

# plot with all temp trackers
microclimALL_long <- microclimALL %>%
  pivot_longer(cols = c(T1, T2, T3), names_to = "Temperature_Measure", values_to = "Temperature")

#KEY FIGURES
#Fig 2 
microclimALL_long<-mutate(microclimALL_long, 
                          Temp_Measure=case_when(Temperature_Measure=="T1"~"Soil", Temperature_Measure=="T2"~"Surface", 
                                                 Temperature_Measure=="T3"~"Air"))
microclimALL_long$Temp_Measure <- factor(microclimALL_long$Temp_Measure, levels = c("Soil", "Surface", "Air"))
dat_text <- data.frame(
  label = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)", "i)"),
  Temp_Measure   = c("Soil", "Soil", "Soil", "Surface", "Surface", "Surface","Air", "Air", "Air"), 
  Site = c("Heather", "Salix", "Sedge"), 
  treatment= c("C", "C", "C")
)
dat_text$Temp_Measure <- factor(dat_text$Temp_Measure, levels = c("Soil", "Surface", "Air"))

p<-ggplot(subset(microclimALL_long,year>2022), aes(x = doy, y = Temperature, color = treatment)) +
  geom_point() + geom_smooth()+
  facet_grid(rows = vars(Temp_Measure) , 
             cols = vars(Site))+ 
  theme(strip.placement = "outside")+ 
  scale_color_manual(values=c("C" = "#89C5DA","W" = "#DA5724"), name="Treatment")  +
  theme_minimal() +
  labs(y = "Average Daily Temp", x = "DOY") 

#PNG
p+
  geom_text(
  data = dat_text,
  aes(x = 170, y = max(microclimALL_long$Temperature, na.rm = TRUE), label = label),
  inherit.aes = FALSE,
  hjust = 0,
  vjust = 1,
  color = "black"
)


#PDF7x7 in
p +
  geom_text(
    data = dat_text,
    mapping = aes(x = -Inf, y = -Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.3,
    vjust = -17,
    color = "black"
  )

    
#Fig S1
microclimALL$Season <- factor(microclimALL$Season, levels = c("Early Summer", "Mid Summer", "Late Summer"))
ggplot(subset(microclimALL,year>2022 ), aes(x = as.factor(Season), y = VWC*100, color = treatment)) +
  geom_boxplot() +
  facet_grid(~Site) +
  scale_color_manual(values=c("C" = "#89C5DA","W" = "#DA5724"), name='Treatment')  +
  theme_minimal() +
  labs(y = "Average Daily Moisture (VWC %)", x = " ")#+ ggtitle("Soil Moisture")

#Fig 3
dat_text <- data.frame(
  label = c("a)", "b)", "c)"),
  Site = c("Heather", "Salix", "Sedge"), 
  treatment= c("C", "C", "C"))
dat_text2 <- data.frame(
  label = c("d)", "e)", "f)"),
  Site = c("Heather", "Salix", "Sedge"), 
  treatment= c("C", "C", "C"))

a<-ggplot(all, aes(y=Rootscub, Duration, fill=treatment))+
  geom_point(aes(fill=treatment, color=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"), name="Treatment")  +
  scale_color_manual(values=c( "#89C5DA", "#DA5724"), name="Treatment")  +
  ylab("Root biomass (g/bulk dens)")+ theme_bw() + xlab("Duration (days)") +
  facet_wrap(~Site)+  geom_text(
  data = dat_text,
  mapping = aes(x =345 , y = 1.7, label = label),
  inherit.aes = FALSE,
  hjust = -0.01,
  vjust = -0.1,
  color = "black")

b<-ggplot(subset(all,year>2022), aes(y=microbial.biomass.C..ug.g., doy, fill=treatment))+
  geom_point(aes(fill=treatment, color=treatment))+
  geom_smooth(method="lm")+ 
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))  +
  scale_color_manual(values=c( "#89C5DA", "#DA5724"))  +
  ylab("Microbial biomass C (ug/g)")+ theme_bw() + xlab("DOY") +
  facet_wrap(~Site)+  geom_text(
  data = dat_text2,
  mapping = aes(x = 180, y = 1000, label = label),
  inherit.aes = FALSE,
  hjust = -0.01,
  vjust = -0.1,
  color = "black")

ggpubr::ggarrange(a,b, nrow=2, common.legend = T)

#Fig S2
ggplot(all, aes(y=F.B, x=microbial.biomass.C..ug.g.))+
  geom_point(aes())+
  geom_smooth(method="lm") + theme_bw() + xlab("Microbial biomass C (ug/g)") + 
  ylab("Fungi:Bacteria")
cor.test(all$F.B, all$microbial.biomass.C..ug.g.)

str(all)

#statistical tests----
#treatment effects
test1<-(aov(T1~treatment*Site*doy, subset(microclimALL, year>2022)))
summary(test1)
TukeyHSD(test1)#warming weakest in salix ~0.6 vs 0.75 in Cassiope, Sedge -NS
test1.1<-(aov(T1~treatment*Season, subset(microclimALL, year>2022)))
summary(test1.1)
TukeyHSD(test1.1)#warming strongest in early summer 
test2<-(aov(T2~treatment*Site, subset(microclimALL, year>2022)))
summary(test2)
TukeyHSD(test2)#warming strongest in salix ~1.35C
test2.1<-(aov(T2~treatment*Season, subset(microclimALL, year>2022)))
summary(test2.1)
TukeyHSD(test2.1)#warming strongest in early summer 
test3<-(aov(T3~treatment*Site, subset(microclimALL, year>2022)))
summary(test3)
TukeyHSD(test3) #warming only ~0.73 degrees in sedge, 1C in Salix, Cassiope
test3.1<-(aov(T3~treatment*Season, subset(microclimALL, year>2022)))
summary(test3.1)
TukeyHSD(test3.1)#warming strongest in early summer 

moist<-lmer(VWC~treatment*Site*doy+ (1|Plot), subset(microclimALL, year>2022))#
summary(moist)

emmeans::emmeans(moist, pairwise~treatment)
emmeans::emmeans(moist, pairwise~treatment|Site) #soil moisture not sig diff between OTC and ctl at any site

moist2<-lmer(VWC~treatment*Season+ (1|Plot), subset(microclimALL, year>2022))#
summary(moist2)
emmeans::emmeans(moist2, pairwise~treatment|Season) #soil moisture not sig diff between OTC and ctl 
emmeans::emtrends(moist, pairwise~treatment, var="doy") 

temp1<-lmer(T1~treatment*Site*doy+ (1|Plot), subset(microclimALL, year>2022))#
summary(temp1)
emmeans::emmeans(temp1, pairwise~treatment)
emmeans::emmeans(temp1, pairwise~treatment|Site) 
emmeans::emtrends(temp1, pairwise~treatment|Site, var="doy") 
emmeans::emtrends(temp1, pairwise~treatment, var="doy") 

temp2<-lmer(T2~treatment*Site*doy+ (1|Plot), subset(microclimALL, year>2022))#
summary(temp2)
emmeans::emmeans(temp2, pairwise~treatment) 
emmeans::emmeans(temp2, pairwise~treatment|Site) 
emmeans::emtrends(temp2, pairwise~treatment, var="doy") 

temp3<-lmer(T3~treatment*Site*doy + (1|Plot), subset(microclimALL, year>2022))#removed plot random intercept because singular fit
summary(temp3)

emmeans::emmeans(temp3, pairwise~treatment) 
emmeans::emmeans(temp3, pairwise~treatment|Site) 
emmeans::emtrends(temp3, pairwise~treatment, var="doy") 

#plant responses

#we asked:  1) How does alpine tundra root growth and soil microbial biomass change across the growing season?
#Season x Community 
roots1<-aov(Rootscub~Season*Site*treatment, all)  
summary(roots1)
TukeyHSD(roots1)
roots2<-aov(Rootscub~Season*treatment , all) 
summary(roots2)
TukeyHSD(roots2)

roots3<-aov(Rootscub~Site*treatment , all) 
summary(roots3)
TukeyHSD(roots3)

roots4<-lmer(Rootscub~VWC*Season + (1|Site) , all) 
anova(roots4)
summary(roots4)
emmeans::emmeans(roots4, specs = c("VWC", "Season"))
emmeans::emtrends(roots4, pairwise~Season, var="VWC")

mic0<-(lmer(logmic~Season*Site*treatment + (1|Plot), subset(all, year>2022))) 
anova(mic0)# Site, Season x Site, Season x treatment, Site x treatment (weak- not part of Q1)
em <- emmeans::emmeans(mic0, pairwise~ Season)
em
em0 <- emmeans::emmeans(mic0, pairwise~ treatment)
em0
em1 <- emmeans::emmeans(mic0, pairwise~ Site)
em1
em2 <- emmeans::emmeans(mic0, pairwise~ Season|Site)
em2
em3 <- emmeans::emmeans(mic0, pairwise~ Season|treatment)
em3
em3 <- emmeans::emmeans(mic0, pairwise~ treatment|Site)
em3
em4 <- emmeans::emmeans(mic0, pairwise~ Season|treatment|Site) 
em4 #increase in mic biomass in cassiope over growing season in warming plots 
em5 <- emmeans::emmeans(mic0, pairwise~ treatment|Site|Season ) 
em5 
r2glmm::r2beta(mic0) #r2= 0.58

mic1<-(lmer(logmic~ VWC * Season+ (1|Plot) + (1|Site), subset(all, year>2022))) 
anova(mic1)
summary(mic1)
emmeans::emmeans(mic1, specs = "VWC")
emmeans::emmeans(mic1, specs = c("VWC", "Season"))
emmeans::emtrends(mic1, pairwise~Season, var="VWC")

#2) Are the timing of root growth and soil microbial biomass linked? 
all2<-subset(all, !is.na(Rootscub))
all_long2<-pivot_longer(all2, cols=c("Rootscub", "logmic"), names_to = "type", values_to = "biomass")%>%select(-Notes)%>%subset(!is.na(biomass))%>%distinct(.)
all_long2<-group_by(all_long2, Plot, year, Season, type)%>%mutate(biomass2=mean(biomass))
all_long3<-select(all_long2, Site, treatment, Plot, year, Season,doy, type, biomass2)%>%distinct(.)

codyn::synchrony(all_long3, time.var = 'doy', species.var = 'type', abundance.var = 'biomass2', replicate.var = "Plot") #plot level synchrony - Cassiope high both, Sedge high warming, Salix weak both 
codyn::synchrony(all_long3, time.var = 'doy', species.var = 'type', abundance.var = 'biomass2', replicate.var = "Plot", metric = "Gross") #plot level synchrony - Cassiope high both, Sedge high warming, Salix weak both 
options(digits=7)
vr<-codyn::variance_ratio(all_long3, time.var = 'doy', bootnumber = 1000, species.var = 'type', abundance.var = 'biomass2', replicate.var = "Plot")#total covariance ratio= 1
vr$Site<-"All"
vr1<-codyn::variance_ratio(subset(all_long3, Site=="Salix"), time.var = 'doy', bootnumber = 1000, species.var = 'type', abundance.var = 'biomass2', replicate.var = "Plot")#total covariance ratio= 1
#Salix VR = 0.6 ~<< 1 negative covariance 
vr1$Site<-"Salix"
vr2<-codyn::variance_ratio(subset(all_long3, Site=="Heather"), time.var = 'doy', bootnumber = 1000, species.var = 'type', abundance.var = 'biomass2', replicate.var = "Plot")#total covariance ratio= 1
#Heather VR = 1.5 ~ >> 1 positive covariance 
vr2$Site<-"Heather"
vr3<-codyn::variance_ratio(subset(all_long3, Site=="Sedge"), time.var = 'doy', bootnumber = 1000, species.var = 'type', abundance.var = 'biomass2', replicate.var = "Plot")#total covariance ratio= 1
#Sedge VR=0.9 ~ close to 1, suggests independent patterns b/w roots and microbes
vr3$Site<-"Sedge"
vr4<-codyn::variance_ratio(subset(all_long3, treatment=="W"), time.var = 'doy', bootnumber = 1000, species.var = 'type', abundance.var = 'biomass2', replicate.var = "Plot")#total covariance ratio= 1
#warming VR=1.23 ~slightly >1 positive covariance
vr4$Site<-"Warming"
vr5<-codyn::variance_ratio(subset(all_long3, treatment=="C"), time.var = 'doy', bootnumber = 1000, species.var = 'type', abundance.var = 'biomass2', replicate.var = "Plot")#total covariance ratio= 1
#control VR=0.8 ~slightly <1, negative covariance 
vr5$Site<-"Control"

vr<-rbind(vr, vr1, vr2, vr3, vr4, vr5)
vr$Site <- factor(vr$Site,
                  levels = c("Control", "Warming", "Sedge", "Salix", "Heather", "All"))
#Fig 4
vr%>%
group_by(Site) %>%
  #median_qi(.width = c(.8, .95)) %>%
  ggplot(aes(y = Site, x = nullmean, xmin = lowerCI, xmax = upperCI)) +
  geom_point(aes(y=Site, x= VR), color='black', fill='red', shape =23, size=3 )+ 
  geom_vline(xintercept = 1, lty=2, linewidth = 1.1, alpha=0.5)+
  ggdist::geom_pointinterval() + theme_bw() + ylab(" ") + xlab("Variance Ratio")



