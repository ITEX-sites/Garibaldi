#############################
# Cleaning the plant survey data from Garibaldi Lake Summer 2023
# Cassandra
# Nov 2023
###############################

# set working directory
setwd("~/GitHub/Garibaldi/Glacial_succession_survey")


plant_data <- read.table("./data/Plant_survey_DATA_combined_cleaning.txt", header=TRUE, sep ="\t", dec = ".")
