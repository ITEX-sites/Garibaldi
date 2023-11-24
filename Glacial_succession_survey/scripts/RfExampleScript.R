#Install needed packages (below) and dependecies
library(raster)
library(terra)
library(sp)
library(caret)
library(dplyr)
library(randomForest)

################################################################################

#Image 1 
rgb.1 <- rast("path/to/file/image.tif")
#If you have a mask file (.shp) for clipping / subsetting image
mask.1 <- vect("path/to/file/mask.shp")
rgb.1 <- terra::mask(rgb.1, mask.1)

#Example vegitation indexes: ExG & ExGR 
##Normalize each band, here 255 is the max value for each band (8-bit)
Rn.1 <- rgb.1$`Red`/255
Gn.1 <- rgb.1$`Green`/255
Bn.1 <- rgb.1$`Blue`/255

RGB.denom.1 <- Rn.1+Gn.1+Bn.1

#Second normalization
Rn2.1 <- Rn.1/RGB.denom.1
Gn2.1 <- Gn.1/RGB.denom.1
Bn2.1 <- Bn.1/RGB.denom.1

#Calculate the indexes
##ExG
exg.1 <- 2*(Gn2.1)-(Rn2.1)-(Bn2.1)

#EXGR (Vieira & Rodrigues, 2021)
EXGR.1 <- exg.1-1.4*(Rn2.1)-(Gn2.1)

#Stack the data
rast.stack.1 <- c(rgb.1$Red, rgb.1$Green, rgb.1$Blue, exg.1, EXGR.1)
names(rast.stack.1) <- c("Red","Green", "Blue", "ExG", "ExGR")

################################################################################

#Seting up training / validation data (from homogeneous polygons)
#call in polygons
poly.1 <- vect("path/to/file/Polygons.shp")

#make an empty raster from the image
empty.1 <- rast(ext(rgb.1$Red), 
                    resolution = res(rgb.1$Red))

#convert the training polygons to a raster with the stored value being the class
polyRst.1 <- rasterize(Poly.1, empty.1, field="ClassID")

#convert the raster to points then a data frame
polyDf.1 <- as.data.frame(rasterToPoints(raster(polyRst.1)))

#extract values for each training point
cords.1 <- polyDf.1[c("x", "y")]
df.1 <- extract(rast.stack.1, cords.1, xy=TRUE)
#Add the class to the extracted values
df.1$ClassID <- factor(polyDf.1$ClassID)

#partition the data into training and eval sets
##Set seed for reproducability
set.seed(321)

#Partition the data
##Here I'm using a 50% stratified random sample based on the classes 
train.1 <- slice_sample(subset(df.1, df.4$ClassID=="1"), prop=0.5) %>%
  union(slice_sample(subset(df.1, df.1$ClassID=="2"), prop=0.5)) %>%
  union(slice_sample(subset(df.1, df.1$ClassID=="3"), prop=0.5)) %>%
  union(slice_sample(subset(df.1, df.1$ClassID=="4"), prop=0.5)) %>%
  union(slice_sample(subset(df.1, df.1$ClassID=="5"), prop=0.5))

#The remaining 50% will be validation
test.1 <- anti_join(df.1, train.1)

#Subset the training / validation sets to only have dependant and independant vars.
train.1 <- relocate(train.1, "ClassID", .before = "x") %>%
  select(Red:ClassID)
test.1 <- relocate(test.1, "ClassID", .before = "x") %>%
  select(Red:ClassID)

################################################################################

#Classification (random forest)

#Set control parameters
##Here I am using a 10-fold cross-validation with grid search optimization
###Grid search is very computationally expensive but best practice
trcontrol <- trainControl(method = "cv", number = 10, search = "grid")

#Train the model with the training control parameters
RF.1 <- caret::train(ClassID ~., data = train.1, method = "rf", 
                         metric = "Accuracy", trcontrol = trcontrol)
#The caret package stores alot of useful info in the model object
##Printing will provide some basic info including basic performace measures 
print(RF.1)

#Confusion matrix is a useful function to get an initial idea of class seperability
##Here I am using the test set
confusionMatrix(data = predict(RF.1, newdata = test.1),
                               test.1$ClassID)
##The model can be saved as a RDS
saveRDS(RF.1, file = "path/to/destination/RF.1.rds")

#for looking at variable importance
##finalModel is the best model based on the optimization parameters 
randomForest::varImpPlot(RF.1$finalModel)

#Apply the model to the imagery and indexes
classOut.1 <- terra::predict(rast.stack.1, RF.1$finalModel, na.rm=TRUE)



