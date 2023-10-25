##########################
# Data summarizing
# Oct 24 2023
##########################

# install libraries
install.packages("dplyr")

# load libraries
library(dplyr)

#####################
# read in data
df0 = read.csv("./data/raw_data/data_2023 - Species.csv")

df <- df0[,-c(1,3)]
#----------------------
summary_df <- df %>% group_by(transect) %>%
  summarise(across(everything(), sum))


#write file to csv
write.csv(summary_df, file = "./data/garibaldi_trampling_species_matrix.csv")
