## reading, cleaning, and analyzing TOMST data 
# using code from https://cran.r-project.org/web/packages/myClim/vignettes/myclim-demo.html

install.packages("myClim")
library(myClim)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(emmeans)
library(rstatix)
library(cowplot)
library(lme4)

setwd("/Users/evankohn/Desktop/Garibaldi/TOMST_2023")


## Read in and clean ####
## read in all TOMST files in directory
tms.d <- mc_read_files(".", dataformat_name = "TOMST", recursive = F, silent = T)

## info about data
mc_info_count(tms.d)
mc_info_meta(tms.d)
mc_info_clean(tms.d)
mc_info(tms.d)

## Renaming locality_id to plot name and treatment
tms1 <- tms.d
locality_list <- as.list(setNames(
  c("1C", "2W", "3C", "4W", "5C", "6W", "7C", "8W", "9C", "10W", 
    "11C", "12W", "13C", "14W", "15C", "16W", "17C", "18W", "19C", "20W", 
    "21C", "22W", "23C", "24W"),
  c("94235781", "94235782", "94235783", "94235784", "94235785", "94235786", 
    "94235787", "94235788", "94235789", "94235790", "94235791", "94235792", 
    "94235793", "94235794", "94235775", "94235776", "94235779", "94235780", 
    "94235777", "94235778", "94235773", "94235774", "94235772", "94235771")
))
tms1 <- mc_prep_meta_locality(tms1, locality_list,  param_name = "locality_id")




## 2022 data by Day ####
# crop to correct dates
start2022 <- as.POSIXct("2022-07-21", tz = "UTC")
end2022   <- as.POSIXct("2022-10-01", tz = "UTC")
loggerdata2022 <- mc_prep_crop(tms1, start2022, end2022)

## working with daily averages
microclim2022_daily <- mc_agg(loggerdata2022, fun = c("mean"), period = "day", min_coverage = 0.95)

#create dataset
microclim2022_tab <- mc_reshape_long(microclim2022_daily) %>% separate(locality_id, into = c("Plot", "treatment"), sep = "(?<=\\d)(?=\\D)")

## edit dataset to have each sensor as a seperate column
microclim2022T1 <- microclim2022_tab %>% filter(sensor_name == "TMS_T1_mean") %>% rename(T1 = value) %>% select(-c("serial_number","sensor_name", "height"))
microclim2022Moist <- microclim2022_tab %>% filter(sensor_name == "TMS_moist_mean") %>% rename(Moisture = value) %>% select(-c("serial_number","sensor_name", "height"))
microclim2022T2 <- microclim2022_tab %>% filter(sensor_name == "TMS_T2_mean") %>% rename(T2 = value) %>% select(-c("serial_number","sensor_name", "height"))
microclim2022T3 <- microclim2022_tab %>% filter(sensor_name == "TMS_T3_mean") %>% rename(T3 = value)%>% select(-c("serial_number","sensor_name", "height"))


microclim2022_all <- merge(microclim2022T1, microclim2022T2, by = c("Plot", "treatment", "datetime", "time_to"))
microclim2022_all <- merge(microclim2022_all, microclim2022T3, by = c("Plot", "treatment", "datetime", "time_to"))
microclim2022_all <- merge(microclim2022_all, microclim2022Moist, by = c("Plot", "treatment", "datetime", "time_to"))


## 2023 by day ####
start2023 <- as.POSIXct("2023-06-26", tz = "UTC")
end2023   <- as.POSIXct("2023-09-23", tz = "UTC")
loggerdata2023 <- mc_prep_crop(tms1, start2023, end2023)

microclim2023_daily <- mc_agg(loggerdata2023, fun = c("mean"), period = "day", min_coverage = 0.95)

microclim2023_tab <- mc_reshape_long(microclim2023_daily) %>% separate(locality_id, into = c("Plot", "treatment"), sep = "(?<=\\d)(?=\\D)")

## edit dataset to have each sensor as a seperate column
microclim2023T1 <- microclim2023_tab %>% filter(sensor_name == "TMS_T1_mean") %>% rename(T1 = value) %>% select(-c("serial_number","sensor_name", "height"))
microclim2023Moist <- microclim2023_tab %>% filter(sensor_name == "TMS_moist_mean") %>% rename(Moisture = value) %>% select(-c("serial_number","sensor_name", "height"))
microclim2023T2 <- microclim2023_tab %>% filter(sensor_name == "TMS_T2_mean") %>% rename(T2 = value) %>% select(-c("serial_number","sensor_name", "height"))
microclim2023T3 <- microclim2023_tab %>% filter(sensor_name == "TMS_T3_mean") %>% rename(T3 = value)%>% select(-c("serial_number","sensor_name", "height"))


microclim2023_all <- merge(microclim2023T1, microclim2023T2, by = c("Plot", "treatment", "datetime", "time_to"))
microclim2023_all <- merge(microclim2023_all, microclim2023T3, by = c("Plot", "treatment", "datetime", "time_to"))
microclim2023_all <- merge(microclim2023_all, microclim2023Moist, by = c("Plot", "treatment", "datetime", "time_to"))


## combined 2022 and 2023 dataset ####
microclimALL <- rbind(microclim2022_all, microclim2023_all)
## microclimALL <- microclimALL %>% separate(datetime, into = c("year", "month", "day"), sep = "-") 
microclimALL <- microclimALL %>%
  mutate(Site = case_when(
    Plot == "1" ~ "Salix",
    Plot == "2" ~ "Salix",
    Plot == "3" ~ "Salix",
    Plot == "4" ~ "Salix",
    Plot == "5" ~ "Salix",
    Plot == "6" ~ "Salix",
    Plot == "7" ~ "Salix",
    Plot == "8" ~ "Salix",
    Plot == "9" ~ "Cassiope",
    Plot == "10" ~ "Cassiope",
    Plot == "11" ~ "Cassiope",
    Plot == "12" ~ "Cassiope",
    Plot == "13" ~ "Cassiope",
    Plot == "14" ~ "Cassiope",
    Plot == "15" ~ "Cassiope",
    Plot == "16" ~ "Cassiope",
    Plot == "17" ~ "Sedge",
    Plot == "18" ~ "Sedge",
    Plot == "19" ~ "Sedge",
    Plot == "20" ~ "Sedge",
    Plot == "21" ~ "Sedge",
    Plot == "22" ~ "Sedge",
    Plot == "23" ~ "Sedge",
    Plot == "24" ~ "Sedge",
    TRUE ~ "Unknown"
  ))


## Dif between treatments? ####
t.test(T1 ~ treatment, data = microclimALL)
t.test(T2 ~ treatment, data = microclimALL)
t.test(T3 ~ treatment, data = microclimALL)
t.test(Moisture ~ treatment, data = microclimALL)
## all but moisture sig dif


## differences between sites? ####

## Between Group temp
ggplot(microclimALL, aes(x = T2)) +   
  geom_histogram() + 
  facet_wrap(~ Site, ncol = 1)

anova1 <- lm(T2 ~ Site, data = microclimALL)
anova(anova1)


comparisonPairs <- emmeans(anova1, "Site")
comparisonUnplanned <- contrast(comparisonPairs, method = "pairwise", adjust = "tukey")
comparisonUnplanned


##kruskal.test(T1 ~ Site, data = microclim2022_all)
## sig diff between sites
##install.packages("rstatix")
##library(rstatix)
##dunn_test(microclim2022_all, T1 ~ Site, p.adjust.method = "holm", detailed = FALSE)
## sig diff between Cassiope and salix, salix and sedge

## Within-group Temp
Salix <- microclimALL %>% filter(Site == "Salix")
Sedge <- microclimALL %>% filter(Site == "Sedge")
Cassiope <- microclimALL %>% filter(Site == "Cassiope")


t.test(T1 ~ treatment, data = Salix)
t.test(T1 ~ treatment, data = Sedge)
t.test(T1 ~ treatment, data = Cassiope)

t.test(T2 ~ treatment, data = Salix)
t.test(T2 ~ treatment, data = Sedge)
t.test(T2 ~ treatment, data = Cassiope)

## Between group moisture

ggplot(microclimALL, aes(x = Moisture)) +   
  geom_histogram() + 
  facet_wrap(~ Site, ncol = 1)


kruskal.test(Moisture ~ Site, data = microclimALL)
## sig diff between sites

dunn_test(microclimALL, Moisture ~ Site, p.adjust.method = "holm", detailed = FALSE)
## sig diff between Cassiope and salix, salix and sedge

wilcox.test(Moisture ~ treatment, data=Salix)
#no sig
wilcox.test(Moisture ~ treatment, data=Sedge)
# sig
wilcox.test(Moisture ~ treatment, data=Cassiope)
# sig


## Plots for temp and moisture loggers ####
microclimALL <- microclimALL %>% drop_na(T1)

# plot with all temp trackers
microclimALL_long <- microclimALL %>%
  pivot_longer(cols = c(T1, T2, T3), names_to = "Temperature_Measure", values_to = "Temperature")

ggplot(microclimALL_long, aes(x = treatment, y = Temperature, fill = Temperature_Measure)) +
  geom_boxplot() +
  facet_wrap(~Site) +
  theme_minimal()+
  labs(y= "Average Temperature", x = "Treatment", fill = "Temperature Sensor")

## T2 Plot

p1 <- ggplot(microclimALL %>% filter(Site == "Cassiope"), 
             aes(x = treatment, y = T2, fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values=c("C" = "#89C5DA", "W" = "#DA5724")) +
  theme_minimal() +
  labs(y= "Average Temperature (T2)", x = NULL) +
  theme(legend.position = "none") +
  geom_text(aes(x = "C", y = 23, label = "*"), size = 8, color = "black")+
  geom_text(aes(x = "W", y = 23, label = "**"), size = 8, color = "black")+
  annotate("text", x = 1.5, y =25, label = "a", size = 8)
p1

p2 <- ggplot(microclimALL %>% filter(Site == "Salix"), 
             aes(x = treatment, y = T2, fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values=c("C" = "#89C5DA", "W" = "#DA5724")) +
  theme_minimal() +
  labs(y= NULL, x = "Treatment") +
  theme(legend.position = "none") +
  geom_text(aes(x = "C", y = 23, label = "*"), size = 8, color = "black")+
  geom_text(aes(x = "W", y = 23, label = "**"), size = 8, color = "black")+
  annotate("text", x = 1.5, y =25, label = "a", size =8)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
p2

p3 <- ggplot(microclimALL %>% filter(Site == "Sedge"), 
             aes(x = treatment, y = T2, fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values=c("C" = "#89C5DA", "W" = "#DA5724")) +
  theme_minimal() +
  labs(y= NULL, x = NULL) +
  theme(legend.position = "none") +
  geom_text(aes(x = "C", y = 23, label = "*"), size = 8, color = "black")+
  geom_text(aes(x = "W", y = 23, label = "**"), size = 8, color = "black")+
  annotate("text", x = 1.5, y =25, label = "b", size =8)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
p3

final_plot_temp <- plot_grid(p1, p2, p3, labels = c("Cassiope", "Salix", "Sedge"), ncol = 3)
final_plot_temp

# Plot with moisture trackers

## sig_labels <- data.frame(
##  Site = c("Cassiope", "Cassiope", "Salix", "Salix", "Sedge", "Sedge"), 
##  treatment = c("C", "W", "C", "W", "C", "W"),  
##  Moisture = c(4000, 4000, 4000, 4000, 4000, 4000),  
##  label = c("a", "b", "a","a", "a", "b"))

## ggplot(microclimALL_long, aes(x = treatment, y = Moisture, fill = treatment)) +
##   geom_boxplot() +
##   facet_wrap(~Site) +
##   scale_fill_manual(values=c("C" = "#89C5DA","W" = "#DA5724"))  +
##   theme_minimal() +
##   geom_text(data = sig_labels, aes(x = treatment, y = Moisture, label = label), size = 4, color = "black")+ 
##   labs(y = "Average Daily Moisture", x = "Treatment")+
##   theme(legend.position = "none")
  
m1 <- ggplot(microclimALL %>% filter(Site == "Cassiope"), 
             aes(x = treatment, y = Moisture, fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values=c("C" = "#89C5DA", "W" = "#DA5724")) +
  theme_minimal() +
  labs(y= "Average Soil Moisture", x = NULL) +
  theme(legend.position = "none") +
  geom_text(aes(x = "C", y = 3000, label = "*"), size = 8, color = "black")+
  geom_text(aes(x = "W", y = 3000, label = "**"), size = 8, color = "black")+
  annotate("text", x = 1.5, y =4000, label = "a", size =8)+
  ylim(0, 4200)
m1

m2 <- ggplot(microclimALL %>% filter(Site == "Salix"), 
             aes(x = treatment, y = Moisture, fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values=c("C" = "#89C5DA", "W" = "#DA5724")) +
  theme_minimal() +
  labs(y= NULL, x = "Treatment") +
  theme(legend.position = "none") +
  geom_text(aes(x = "C", y = 3850, label = "*"), size = 8, color = "black")+
  geom_text(aes(x = "W", y = 3850, label = "*"), size = 8, color = "black")+
  annotate("text", x = 1.5, y =4000, label = "b", size =8)+
  ylim(0, 4200)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
m2


m3 <- ggplot(microclimALL %>% filter(Site == "Sedge"), 
             aes(x = treatment, y = Moisture, fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values=c("C" = "#89C5DA", "W" = "#DA5724")) +
  theme_minimal() +
  labs(y= NULL, x = NULL) +
  theme(legend.position = "none") +
  geom_text(aes(x = "C", y = 3700, label = "*"), size = 8, color = "black")+
  geom_text(aes(x = "W", y = 3700, label = "**"), size = 8, color = "black")+
  annotate("text", x = 1.5, y =4000, label = "c", size =8)+
  ylim(0, 4200)+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
m3

final_plot_moist <- plot_grid(m1, m2, m3, labels = c("Cassiope", "Salix", "Sedge"), ncol = 3)
final_plot_moist

## overall temp trend
ggplot(microclim2022_all, aes(x = datetime, y = T1, color = treatment)) +
  geom_point() +
  theme_minimal() +
  xlab("Date")+
  ylab("Temp")


## custom date range averages ####

date_range_avg <- function(data, start_date, end_date) {
  data %>%
    filter(datetime >= as.POSIXct(start_date) & 
             datetime <= as.POSIXct(end_date)) %>%
    group_by(Plot, treatment, Site) %>% 
    summarize(avg_temp = mean(T1, na.rm = TRUE),
              avg_moisture = mean(Moisture, na.rm = TRUE),
              min_temp = min(T1, na.rm = TRUE),
              max_temp = max(T1, na.rm = TRUE),
              sd_temp = sd(T1, na.rm = TRUE),
              avg_t2 = mean(T2, na.rm = TRUE),
              n_observations = n(),
              date = end_date)
}

Aug22 <- date_range_avg(microclimALL, "2022-08-08", "2022-08-22")
Sept22 <- date_range_avg(microclimALL, "2022-09-11", "2022-09-25")

Aug23 <- date_range_avg(microclimALL, "2023-07-18", "2023-08-01")
Sept23 <- date_range_avg(microclimALL, "2023-09-08", "2023-09-22")

avg_microclim <- rbind(Aug22, Sept22, Aug23, Sept23) %>% separate(date, into = c("year", "month", "day"), sep = "-") 


Aug22_day <- date_range_avg(microclimALL, "2022-08-21", "2022-08-22")
Sept22_day <- date_range_avg(microclimALL, "2022-09-24", "2022-09-25")

Jul23_day <- date_range_avg(microclimALL, "2023-06-27", "2023-06-28")
Aug23_day <- date_range_avg(microclimALL, "2023-07-31", "2023-08-01")
Sept23_day <- date_range_avg(microclimALL, "2023-09-21", "2023-09-22")

avg_microclim_day <- rbind(Aug22_day, Sept22_day, Jul23_day, Aug23_day, Sept23_day) %>% separate(date, into = c("year", "month", "day"), sep = "-") 


## merging microbiometer and microclim data and root core data ####

##biometer <-read.csv("/Users/evankohn/Desktop/Garibaldi/Microbiometer_analysis/Microbiometer_Data_complete.csv")
#biometer<-rename(biometer, treatment=treatment..W.C.) %>% mutate(Date = dmy(Date)) %>% separate(Date, into = c("year", "month", "day"), sep = "-") ##%>% select(-c("day"))

#microclim_biometerdata <- merge(avg_microclim, biometer, by=c("year", "month","Plot", "treatment" ))

soil<-read.csv("/Users/evankohn/Desktop/Garibaldi/Microbiometer_analysis/Microbiometer_Data_complete.csv")
soil<-rename(soil, treatment=treatment..W.C.)
roots <- read.csv("/Users/evankohn/Desktop/Garibaldi/Root_cores_analysis/Alpine_cores_names_2023_Complete.csv")
roots<-rename(roots, treatment=W.C)

#clean up 
soil$plot<-as.numeric(soil$Plot)

#add doy info
unique(soil$Date)
soil$Date <- as.Date(soil$Date, format = "%d/%m/%Y")
soil$doy <- lubridate::yday(soil$Date)
soil <- soil %>% separate(Date, into = c("year", "month", "day"), sep = "-")

soil_microclim <- merge(avg_microclim, soil, by=c("year", "month","Plot", "treatment" ))
soil_microclim_day <- merge(avg_microclim_day, soil, by=c("year", "month","Plot", "treatment" ))

#split into seasons (both years) 
soil_microclim<-group_by(soil_microclim, year)%>%
  mutate(., Season=case_when(doy<200 ~ "Early Summer",
                             doy>240 ~ "Late Summer",
                             TRUE ~"Mid Summer"))
soil_microclim_day<-group_by(soil_microclim_day, year)%>%
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

## prep and align datasets
soil_microclim$Subsite<-soil_microclim$Site.x
soil_microclim_day$Subsite<-soil_microclim_day$Site.x
roots$year<-2023

soilx<-dplyr::select(soil_microclim,Subsite, treatment, avg_temp, avg_moisture, min_temp, max_temp, sd_temp, avg_t2, microbial.biomass.C..ug.g., F.B, Plot, year, doy, Season)

soilx_day<-dplyr::select(soil_microclim,Subsite, treatment, avg_temp, avg_moisture, min_temp, max_temp, sd_temp, avg_t2, microbial.biomass.C..ug.g., F.B, Plot, year, doy, Season)

rootsx<-dplyr::select(roots, Subsite, treatment,Root..g.bulk.dens., GWC.., Plot, year, Season)
rootsx$Rootscub<-rootsx$Root..g.bulk.dens.^(1/3)

## plot
all<-merge(rootsx,soilx, by = c("Subsite", "treatment", "Plot", "year", "Season"))
all_day<-merge(rootsx,soilx_day, by = c("Subsite", "treatment", "Plot", "year", "Season"))

## Root x microbe biomass, colored by temp

ggplot(all_day, aes(y = log(microbial.biomass.C..ug.g.), x = Rootscub, fill = treatment, colour = avg_t2)) +
  geom_point(size = 2.5) +  
  geom_smooth(method = "lm") +    
  scale_fill_manual(values = c("#89C5DA", "#DA5724")) +
  scale_colour_gradient(low = "yellow", high = "red") + 
  ylab("Microbial biomass C (ug/g)") +
  xlab("Root biomass (g/bulk dens)") +
  labs(color = "Average Daily Temp")+
  theme_bw()
  
  
## Moisture x biomass, colored by site
ggplot(soil_microclim, aes(x = avg_moisture, y = microbial.biomass.C..ug.g., shape = month)) +
  geom_point(aes(color = Site.x), size = 3) +
  theme_minimal() +
  xlab("Average Soil Moisture")+
  ylab("Microbial Biomass (ug/g)") +
  geom_smooth(aes(group = treatment, linetype = treatment), method = lm, color = alpha("black", 0.9), se = FALSE)

ggplot(all, aes(x = avg_moisture, y = Root..g.bulk.dens.)) +
  geom_point(aes(color = Subsite), size = 3) +
  facet_wrap(~Subsite, scales = "free")+
  theme_minimal() +
  xlab("Average Soil Moisture")+
  ylab("Root Biomass (g/bulk dens)") +
  guides(color = "none")+
  geom_smooth(method = lm, color = alpha("black", 0.9), se = FALSE)


## microbe biomass models ####
soil_microclim_Cas <- soil_microclim %>% filter(Site.x == "Cassiope")
soil_microclim_Sed <- soil_microclim %>% filter(Site.x == "Sedge")
soil_microclim_Sal <- soil_microclim %>% filter(Site.x == "Salix")


model1 <- lm(log(microbial.biomass.C..ug.g.) ~ avg_temp*log(avg_moisture) + year, data= soil_microclim_Cas)
qqnorm(resid(model1))
qqline(resid(model1))
plot(model1)
summary(model1)

Sed_model <- lm(microbial.biomass.C..ug.g. ~ avg_temp*log(avg_moisture) + year , data= soil_microclim_Sed)
qqnorm(resid(Sed_model))
qqline(resid(Sed_model))
plot(Sed_model)
summary(Sed_model)

Sal_model <- lm(microbial.biomass.C..ug.g. ~ avg_temp*log(avg_moisture) + year , data= soil_microclim_Sal)
qqnorm(resid(Sal_model))
qqline(resid(Sal_model))
plot(Sal_model)
summary(Sal_model)


library(mgcv)
gam_mod_cas <- gam(log(microbial.biomass.C..ug.g.) ~  s(avg_temp) + s(avg_moisture) + year,data = soil_microclim_Cas)
summary(gam_mod_cas)

gam_mod_sed <- gam(log(microbial.biomass.C..ug.g.) ~  s(avg_temp) + s(avg_moisture) + year,data = soil_microclim_Sed)
summary(gam_mod_sed)

gam_mod_sal <- gam(log(microbial.biomass.C..ug.g.) ~  s(avg_temp) + s(avg_moisture) + year,data = soil_microclim_Sal)
summary(gam_mod_sal)


## Root biomass model ####

root_model <- lm(Root..g.bulk.dens. ~ avg_temp + log(avg_moisture), data= all)
qqnorm(resid(root_model))
qqline(resid(root_model))
plot(root_model)
summary(root_model)

all$Subsite <- as.factor(all$Subsite)

root_mixed_effects <- lm(Root..g.bulk.dens. ~ avg_temp + Subsite, data= all)
qqnorm(resid(root_mixed_effects))
qqline(resid(root_mixed_effects))
plot(root_mixed_effects)
summary(root_mixed_effects)


all_Cas <- all %>% filter(Subsite == "Cassiope")
all_Sed <- all %>% filter(Subsite == "Sedge")
all_Sal <- all %>% filter(Subsite== "Salix")

root_model_cas <- lm(Root..g.bulk.dens. ~ avg_temp + log(avg_moisture), data= all_Cas)
plot(root_model_cas)
summary(root_model_cas)

root_model_sed <- lm(Root..g.bulk.dens. ~ avg_temp + log(avg_moisture), data= all_Sed)
plot(root_model_sed)
summary(root_model_sed)

root_model_sal <- lm(Root..g.bulk.dens. ~ avg_temp + log(avg_moisture), data= all_Sal)
plot(root_model_sal)
summary(root_model_sal)


## Everything else ####

## T1 and microbiometer 
microclim2022T1 <- microclim2022T1 %>%
  separate(locality_id, into = c("Plot", "treatment"), sep = "(?<=\\d)(?=\\D)") %>%
  separate(datetime, into = c("year", "month", "day"), sep = "-")


microclim_biometerdata_t1 <- merge(microclim2022T1, dat2022, by=c("month","Plot"))

## Checking for correlations
ggplot(microclim_biometerdata_t1, aes(x = value, y = microbial.biomass.C..ug.g.)) +
  geom_point() +
  theme_minimal() +
  xlab("soil temp")+
  ylab("microbe biomass") +
  geom_smooth(method = lm, color = alpha("black", 0.9), se = FALSE, size = 1.5)

ggplot(microclim_biometerdata_t1, aes(x = value, y = F.B)) +
  geom_point() +
  theme_minimal() +
  xlab("soil temp")+
  ylab("F/B") +
  geom_smooth(method = lm, color = alpha("black", 0.9), se = FALSE, size = 1.5)

cor.test(microclim_biometerdata_t1$value, microclim_biometerdata_t1$microbial.biomass.C..ug.g., method = "spearman")
## p = 0..15
cor.test(microclim_biometerdata_t1$value, microclim_biometerdata_t1$F.B, method = "spearman")
## p = .11



## moisture and microbiometer 
microclim2022Moist <- microclim2022Moist %>%
  separate(locality_id, into = c("Plot", "treatment"), sep = "(?<=\\d)(?=\\D)") %>%
  separate(datetime, into = c("year", "month", "day"), sep = "-")

microclim_biometerdata_moist <- merge(microclim2022Moist, dat2022, by=c("month","Plot"))

## Checking for correlations
ggplot(microclim_biometerdata_moist, aes(x = value, y = microbial.biomass.C..ug.g., color= month)) +
  geom_point() +
  theme_minimal() +
  xlab("soil moisture")+
  ylab("microbe biomass") +
  scale_fill_manual(values = c("red", "blue")) +
  geom_smooth(method = lm, color = alpha("black", 0.9), se = FALSE, size = 1.5)

ggplot(microclim_biometerdata_moist, aes(x = value, y = F.B, color= month)) +
  geom_point() +
  theme_minimal() +
  xlab("soil moisture")+
  ylab("F/B") +
  scale_fill_manual(values = c("red", "blue")) +
  geom_smooth(method = lm, color = alpha("black", 0.9), se = FALSE, size = 1.5)

ggplot(microclim_biometerdata_moist, aes(x = value, y = Fungal.)) +
  geom_point() +
  theme_minimal() +
  xlab("soil temp")+
  ylab("F/B") +
  geom_smooth(method = lm, color = alpha("black", 0.9), se = FALSE, size = 1.5)
ggplot(microclim_biometerdata_moist, aes(x = value, y = Bacterial.)) +
  geom_point() +
  theme_minimal() +
  xlab("soil temp")+
  ylab("F/B") +
  geom_smooth(method = lm, color = alpha("black", 0.9), se = FALSE, size = 1.5)

cor.test(microclim_biometerdata_moist$value, microclim_biometerdata_moist$microbial.biomass.C..ug.g.)
## p = 1.819e10-9 Strong negative correlation

cor.test(microclim_biometerdata_moist$value, microclim_biometerdata_moist$F.B)
## p = 3.09 e-10

cor.test(microclim_biometerdata_moist$value, microclim_biometerdata_moist$Fungal.)
## 

microclim_biometerdata_moist_W <- microclim_biometerdata_moist %>% filter(treatment.x == "W")

ggplot(microclim_biometerdata_moist_W, aes(x = value, y = F.B)) +
  geom_point() +
  theme_minimal() +
  xlab("soil temp")+
  ylab("F/B") +
  geom_smooth(method = lm, color = alpha("black", 0.9), se = FALSE, size = 1.5)

cor.test(microclim_biometerdata_moist_W$value, microclim_biometerdata_moist_W$microbial.biomass.C..ug.g.)
## p = 1.819e10-9 Strong negative correlation

cor.test(microclim_biometerdata_moist$value, microclim_biometerdata_moist$F.B)
## p = 3.09 e-10




## Air temp and microbiometer 
## Air temp 2 and microbiometer 