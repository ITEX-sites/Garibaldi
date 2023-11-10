## Heatmaps and Correlograms for Sorensons Dissimilarity Coefficients Matrix
## Code shareed by Nina Hewitt, November 9, 2023
## Edited ___

rm(list = ls()) # remove objects from the workspace

# See where you are right now:
getwd()

## Find the location of your file.
## The file path has a name so we put it in "quotes"
## R likes forward slashes, not back slashes
wd = "INSERT YOUR WORKING DIRECTORY HERE"

## To put R in the right place, we now have to command it to switch.
setwd(wd)
getwd()  # quick double check

## This code assumes data arranged in csv file as a matrix:

#	       3900m	     3991m	    4002m	     4023m	    4100m
# 3900m	 0	         0.2238806	0.7272727 	0.761194	0.6666667
# 3991m	 0.2238806	 0	        0.6610169	 0.6666667	0.5932203
# 4002m  0.7272727	 0.6610169	0	         0.1864407	0.6896552
# 4023m  0.761194	   0.6666667	0.1864407	 0	        0.559322
# 4100m  0.6666667	 0.5932203	0.6896552	 0.559322	  0

# Bring in your data:

# mydata <- read.csv("your drive") # # read csv into R OR use file.choose:
mydata <- read.csv(file = "./data/example_data/sorenson_dissim_matrix.csv", row.name = 1) # bring in csv, matrix in file, and ignore name in first column

# mydata <- read.csv(file.choose(), row.name = 1) # bring in csv, matrix in file and ignore name in first column

mydata # view your data
head(mydata) # view first 6 lines
tail(mydata) # last 6 lines

# To detect that, use "class()" command:
class(mydata) # Built-in matrix

# Load required packages

install.packages("corrplot") # delete hashtag first time to install
library(corrplot)

# Relabel categories
colnames(mydata) <- c("3900m", "3991m", "4002m", "4023m", "4100m")

# Compute the actual values matrix
actual_values <- as.matrix(mydata)

# Create the correlogram plot with black category labels and "m" in the labels
corrplot(actual_values, is.corr = FALSE, method = "color", type = "upper", diag = FALSE, col = colorRampPalette(c("black", "white"))(200), tl.col = "black")

# Add self comparisons - Create the correlogram plot with black category labels and "m" in the labels; show all comparisons (remove diag = false)
corrplot(actual_values, is.corr = FALSE, method = "color", type = "upper", col = colorRampPalette(c("black", "white"))(200), tl.col = "black")

### Optional: To add labels to the squares use code above, but replace last step (create correlogram) with the following:

# Create the correlogram plot with black category labels, "m" in the labels, values as labels in each square, and greyscale for the boxes
corrplot(actual_values, is.corr = FALSE, method = "color", type = "upper", col = colorRampPalette(c("black", "white"))(200), tl.col = "black", addCoef.col = "black", number.cex = 0.7, addCoefasPercent = FALSE, number.digits = 4)

## Try a heatmap:
col <- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = actual_values, col = col, symm = TRUE)


######
