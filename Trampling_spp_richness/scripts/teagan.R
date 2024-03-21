library(ggplot2)
library(dplyr)
library(tidyverse)
sppdata <- read.csv("./data/processed_data/final_species_matrix.csv")
sppdata %>% rename(species = transect) -> sppdata
sppdatalong <- pivot_longer(sppdata, !species, names_to = "transect", values_to = "frequency")
sppdata$transect <- as.variable(sppdata$transect)
sppdata %>%
  group_by(species) %>%
  mutate(mean.freq = mean()) -> longdata0.25
