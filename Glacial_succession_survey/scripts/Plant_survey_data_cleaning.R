#############################
# Cleaning the plant survey data from Garibaldi Lake Summer 2023
# Cassandra
# Nov 2023
###############################
# libraries
library(dplyr)
library(tidyr)

# set working directory
setwd("~/GitHub/Garibaldi/Glacial_succession_survey")

# load in the plant data
plant_data <- read.table("./data/Plant_survey_DATA_combined_cleaning.txt", header=TRUE, sep ="\t", dec = ".")

########################

# series of if statements to change raw data into indiv per 1 m^2
plant_data_cleaned <- plant_data
for (i in 1:nrow(plant_data)){
  for (j in 6:ncol(plant_data)){
    data_val <- as.character(plant_data[i,j])
    if (grepl("m", data_val)) {
      new_data_val <- gsub('m','',data_val)
      new_data_val <- as.numeric(new_data_val)
      plant_data_cleaned[i,j] <- 1/(pi * new_data_val^2)
    }
    if (grepl(",", data_val)) {
      data_list <- strsplit(plant_data[i,j], ",")
      data_df <- as.data.frame(data_list)
      data_df[,1] <- as.numeric(data_df[,1])
      plant_data_cleaned[i,j] <- sum(data_df[,1])/2
    }
  }
}

##############################
# check and merge duplicate species

# make a Genus_spp column

plant_data_cleaned$Genus_spp <- paste0(plant_data_cleaned$Genus, "_", plant_data_cleaned$Species)
unique(plant_data_cleaned$Genus_spp)

plant_data_cleaned %>%
  group_by(Genus_spp) %>%
  summarise(across(c(6:45), sum))





