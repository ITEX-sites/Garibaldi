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
plant_data <- read.table("./data/older_backups/Plant_survey_DATA_combined_cleaning.txt", header=TRUE, sep ="\t", dec = ".")

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

plant_data_cleaned <- plant_data_cleaned[,-c(46:85)]
##############################
# check and merge duplicate species

# make a Genus_spp column
plant_data_cleaned$Genus_spp <- paste0(plant_data_cleaned$Genus, "_", plant_data_cleaned$Species)
unique(plant_data_cleaned$Genus_spp)

plant_data_cleaned$Type <- as.factor(plant_data_cleaned$Type)
plant_data_cleaned$Family <- as.factor(plant_data_cleaned$Family)
plant_data_cleaned$EnglishName <- as.factor(plant_data_cleaned$EnglishName)
plant_data_cleaned$Genus <- as.factor(plant_data_cleaned$Genus)
plant_data_cleaned$Species <- as.factor(plant_data_cleaned$Species)
plant_data_cleaned$Genus_spp <- as.factor(plant_data_cleaned$Genus_spp)
plant_data_cleaned <- plant_data_cleaned %>% mutate_if(is.character, as.numeric)

plant_data_summarized <- plant_data_cleaned %>%
  group_by(Genus_spp) %>%
  summarise(across(c(6:45), sum))
plant_data_summarized$sum <- rowSums(plant_data_summarized[,2:ncol(plant_data_summarized)])
write.csv(plant_data_summarized, file = "./data/Garibaldi_plant_data_Genus_spp.csv")

plant_data_summarized <- plant_data_cleaned %>%
  group_by(Genus) %>%
  summarise(across(c(5:44), sum))
plant_data_summarized$sum <- rowSums( plant_data_summarized[,2:ncol(plant_data_summarized)] )
write.csv(plant_data_summarized, file = "./data/Garibaldi_plant_data_Genus.csv")

plant_data_summarized <- plant_data_cleaned %>%
  group_by(Family) %>%
  summarise(across(c(5:44), sum))
plant_data_summarized$sum <- rowSums( plant_data_summarized[,2:ncol(plant_data_summarized)] )
write.csv(plant_data_summarized, file = "./data/Garibaldi_plant_data_Family.csv")

#---------------------------------------------
# join other species information
plant_data_species_info <- plant_data_cleaned[,c(1:5, 46)]
plant_data_species_info <- distinct(plant_data_species_info)
plant_data_summarized1 <- left_join(plant_data_summarized,plant_data_species_info,  by="Genus_spp")

#######################################
# Look at with Glacial Retreat data and waypoints data
waypoint_data <- read.table("./data/Glacial_retreat_times/SurveyPoints-Deglacial.csv", header=TRUE, sep =",", dec = ".")
nrow(waypoint_data)

waypoint_model_data <- read.table("./data/waypoints_with_model_val.csv", header=TRUE, sep =",", dec = ".")

waypoints_surveyed <- as.data.frame(colnames(plant_data_cleaned[,c(6:45)]))
colnames(waypoints_surveyed) <- c("Name")
nrow(waypoints_surveyed)

# join lat long and glacial retreat to the waypoints we have plant data for
waypoints_used0 <- left_join(waypoints_surveyed, waypoint_data, by="Name")

# join the model data
waypoints_used <- left_join(waypoints_used0, waypoint_model_data, by="Name")

#---------------------
# join in the metadata
waypoint_metadata <- read.table("./data/older_backups/Waypoint_meta_data.txt", header=TRUE, sep ="\t", dec = ".")
waypoints_used <- left_join(waypoints_used, waypoint_metadata, by="Name")

waypoints_used <- distinct(waypoints_used)

write.csv(waypoints_used, file = "./data/waypoints_used.csv")
