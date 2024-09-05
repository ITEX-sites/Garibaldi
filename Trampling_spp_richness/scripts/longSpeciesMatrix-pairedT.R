###script that takes species matrix data in long format to make frequency matrix for t testing
##assumes directory is "scripts" within "Trampling_spp_richness"

#load necessary packages
library(dplyr)

#read in the file
speciesMatrix <- read.csv("./data/processed_data/species-data-long.csv")

#add column to remove text portion of transect ID, so can determine if it's odd/even (trampled/untrampled) in simple loop
speciesMatrix <- speciesMatrix %>%
  mutate(transectNumber = as.numeric(sub(".*\\.(\\d+)", "\\1", transect)))

#add column for trampling treatment
speciesMatrix <- speciesMatrix %>%
  mutate(treatment = ifelse(transectNumber %% 2 == 0, "offTrail", "onTrail"))

#average by species and trampling treatment, add column for log of mean frequency by trampled/untrampled
freqSpeciesMatrix <- speciesMatrix %>%
  group_by(species, treatment) %>%
  summarise(meanFreq = mean(frequency))  %>%
  mutate(LogMeanFreq = log(meanFreq))

#sort final summary dataframe so that rows are ordered by trampling treatment not species
freqSpeciesMatrix <- freqSpeciesMatrix %>%
  arrange(treatment)

#export to csv
write.csv(freqSpeciesMatrix, "./data/processed_data/speciesFreq-pairedT.csv", row.names = FALSE)
