###script that takes "final_species_matrix.csv" and converts it to same format as "garibaldi_trampling_species_matrix.csv"
##assumes directory is "scripts" within "Trampling_spp_richness"


#read in the file 
speciesMatrix <- read.csv("../data/processed_data/final_species_matrix.csv")

# transpose the dataframe so species names are in columns
speciesMatrix <- t(speciesMatrix)

#convert from matrix to dataframe
speciesMatrix=as.data.frame(speciesMatrix)

#make the species names the column names
colnames(speciesMatrix) <- speciesMatrix[1,]

#remove first row of dataframe which contains species names
speciesMatrix <-speciesMatrix[2:29,]

#making row names for transect names the first column
speciesMatrix <- data.frame(transect=row.names(speciesMatrix), speciesMatrix)

#remove row names
rownames(speciesMatrix) <- NULL

#write df to csv
write.csv(speciesMatrix, "../data/processed_data/finalCSV-GaribaldiSpeciesMatrixFormat.csv")
