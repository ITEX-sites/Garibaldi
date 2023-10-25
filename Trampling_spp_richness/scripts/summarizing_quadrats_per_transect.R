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

# remove quadrat and date columns
df <- df0[,-c(1,3)]

# make transect a factor
df$transect <- as.factor(df$transect)

# add zeros
df[is.na(df)] <- 0

#----------------------
summary_df <- df %>% group_by(transect) %>%
  summarise(across(everything(), sum))


#write file to csv
write.csv(summary_df, file = "./data/garibaldi_trampling_species_matrix.csv")
