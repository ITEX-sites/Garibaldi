# Sorenson's Dissimilarity

## Nina Hewitt 2022 using example script for Sorenson's dissimilarity shared by Ben Freeman 2021

# R Script for calculating Sorenson's dissimilarity between sites or elevation bands.
# This sheet is for computations by elevation bands in Hewitt's 2016 Karakoram surveys.

rm(list = ls()) # remove objects from the workspace

mydata <- read.csv(file.choose())
# read csv into R; species are columns, sites are rows, presences = 0, absences = 1

mydata
# test data - file: "thale_altitude.csv" 
# Data to test the Sørensen dissimilarity index. 
# This file has each plot's elevation with presence absence; Columns are species;
# elevations (or sites) are rows; 1 indicates a presence; 0 an absence.
# Next, choose file: "altitude_baumharel.csv" ; etc., for all 4 sites (MirKhun; Deosai) investigated

# install.packages("vegan") # install vegan package; remove hashtag if not yet installed

library(vegan) # load "vegan" package which performs various community ecology functions

help(package="vegan") # get help to find out about package

# calculate Sorenson dissimilarity between different sites
# (A+B-2*J)/(A+B)	"binary"	Sørenson
designdist(mydata, method = "(A+B-2*J)/(A+B)")

# RESULTS
## These analyses are by elevation zone of survey plots to which abundance and 
# presence absence measures were linked

## Thale
#           1         2         3         4
# 2 0.2238806                              
# 3 0.7272727 0.6610169                    
# 4 0.7611940 0.6666667 0.1864407          
# 5 0.6666667 0.5932203 0.6896552 0.5593220

## Legend:
# 1 3900 m
# 2 3991 m
# 3 4002 m
# 4 4023 m
# 5 4100 m

# THALE Results: Confirms that sampled elevations that are non-adjacent are dissimilar (higher Sorenson's value) and 
# that spp composition in elevations that are at opposite ends of the elevation spectrum sampled
# are most different.
# Thus, it appears that the higher elevations are very dissimilar from lower elevation samples


# Try Jaccard (A+B-2*J)/(A+B-J)	"binary"	Jaccard
designdist(mydata, method = "(A+B-2*J)/(A+B-J)")

# THALE
#          1         2         3         4
# 2 0.3658537                              
# 3 0.8421053 0.7959184                    
# 4 0.8644068 0.8000000 0.3142857          
# 5 0.8000000 0.7446809 0.8163265 0.7173913

# 1 3900
# 2 3991
# 3 4002
# 4 4023
# 5 4100

# Try the Bray-Curtis - (A+B-2*J)/(A+B) "minimum"	Bray-Curtis
# will be the same as Sorenson's if data are simple presence absence

# designdist(mydata, method = "(A+B-2*J)/(A+B)")  # doesn't work; See below for correct command to 
# recognize non-binary data

# Bray Curtis:
vegdist(mydata, method = "bray", binary=FALSE, diag=FALSE, upper=FALSE,
        na.rm = FALSE)

# THALE -- Bray-Curtis with percent cover (abundance) data # 
#          1         2         3         4
# 2 0.3009259                              
# 3 0.4159292 0.4478873                    
# 4 0.4330709 0.4307305 0.3026316          
# 5 0.5471698 0.4590909 0.5389049 0.4293059

# 1 3900
# 2 3991
# 3 4002
# 4 4023
# 5 4100



#######
## EXAMPLE
## Shared with Nina by Ben Freeman, post-doctoral research associate Beaty Biodiversity Museum 
# who studies species range shifts in montane environments ( https://www.biorxiv.org/content/10.1101/2020.05.18.102848v3 ).

# Example: 
# mydata
# sp1 sp2 sp3 sp4 sp5 sp6 sp7 sp8 sp9 sp10 sp11 sp12 sp13 sp14 sp15 sp16 sp17 sp18 sp19 sp20
# 1   1   1   1   1   1   1   1   1   1    1    0    0    0    0    0    0    0    0    0    0
# 2   1   1   0   1   1   1   1   1   0    0    0    1    0    0    0    0    0    0    0    0
# 3   0   1   1   1   1   1   1   1   1    0    0    0    0    1    0    1    0    0    0    0
# 4   0   0   1   0   1   1   1   1   1    0    0    0    0    0    0    0    0    0    0    0
# 5   0   0   0   0   1   0   0   1   0    1    0    1    1    0    1    1    1    1    1    1

# calculate Sorenson dissimilarity between different sites
# designdist(mydata, method = "(A+B-2*J)/(A+B)")
#               1                2             3          4
# 2 0.2222222                              
# 3 0.2000000 0.3333333                    
# 4 0.2500000 0.4285714 0.2500000          
# 5 0.7142857 0.6842105 0.7142857 0.7647059

# shows that in this made-up example, dissimilarity is high between site 5 and all other sites
# (closer to 1), but relatively similar between other sites (closer to 0)