# Aims:
# 1. Define and calculate a standardized reproductive metric

# Authors: Nathalie Chardon, Cassandra Elphinstone, Philippa Stone
# Date created: 15 Feb 2023
# Date updated: 28 July 2023 (NC)


# # LIBRARIES # # 
library(ggplot2)
library(ggtext)
library(scales)
library(tidyverse)
library(lmerTest)
library(egg) #for labelling plot facets


rm(list=ls()) 


# # INPUT FILES # #
load('trampling_analyses/compiled_data/quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)


# # OUTPUT FILES # #
load('trampling_analyses/compiled_data/quad.RData') ##updated with reproductive metric & plant area (repro.R)



####################################################################################################

# # CALCULATE REPRODUCTIVE METRIC # # 

####################################################################################################

# Data
load('trampling_analyses/compiled_data/quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)

dat <- quad #generic dataframe name
str(dat)

# Remove P. grandiflora because only sampled at 1 site
dat <- dat %>% 
  filter(!species == 'phygla')

# Convert categorical predictor variables to factor
ff <- c('transect', 'species', 'dist', 'trans.pair')
dat[ff] <- lapply(dat[ff], as.factor)

str(dat) #check data structure


# Calculate plant area for reproductive output
dat$plantArea_cm2 <- (dat$mxdiam_mm * dat$height_mm)/100

# Calculate reproductive metric combining buds, flws, frts and dividing by plant area
dat$repro <- NA #initialize column
for (i in 1:nrow(dat)) { #loop through each row of DF
  
  dat$repro[i] <- sum(c(dat$buds[i], dat$flws[i], dat$frts[i]), na.rm = T) / dat$plantArea_cm2[i]
}
summary(dat$repro)
hist(dat$repro, breaks = 100)

# dat <- dat %>% 
#   mutate_if(is.numeric, ~ replace(., is.infinite(.), NA)) #Inf to NAs


# Calculate relative reproduction
mm <- dat %>% #create DF with max value per species
  group_by(species) %>% 
  summarise(max = max(repro, na.rm = T)) #NA listed in plots not seeded

dat <- left_join(dat, mm, by = 'species') #combine DFs
dat$rel_repro <- dat$repro/dat$max
summary(dat$rel_repro)
hist(dat$rel_repro, breaks = 50)

dat <- dat %>% 
  select(-c(max)) #remove max column


# Check outliers
quad <- dat

foo <- quad %>% filter(repro > 2) #Phyemp outlier is so large because a very small plant had 1 bud and 1 fruit


# Set Carex repro to NA because inconsistently measured in the field 
quad <- quad %>% 
  mutate(flws = if_else(species == 'carspp', NA_real_, flws)) %>% 
  mutate(frts = if_else(species == 'carspp', NA_real_, frts)) %>% 
  mutate(buds = if_else(species == 'carspp', NA_real_, buds)) %>% 
  mutate(repro = if_else(species == 'carspp', NA_real_, repro)) %>% 
  mutate(rel_repro = if_else(species == 'carspp', NA_real_, rel_repro))
  

# Save updated DF
save(quad, file = 'trampling_analyses/compiled_data/quad.RData')


