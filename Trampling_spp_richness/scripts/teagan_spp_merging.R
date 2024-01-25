 names(garibaldi_trampling_species_matrix)
library("tidyverse")
gtsm %>% select(order(colnames(gtsm))) -> gtsm_alphabetical
gtsm <- garibaldi_trampling_species_matrix
gtsm_alphabetical <- rename(gtsm_alphabetical, Castilleja.parvifolia = "Castilleja_hybrid_PR18")
