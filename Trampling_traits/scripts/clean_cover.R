## Edited by N. Chardon (28 Mar 2023) to check plant percent cover data for errors

library(dplyr)

#   R code for:
#   A protocol for data exploration to avoid common statistical problems
#   Methods in Ecology and Evolution
#
#   Alain F. Zuur (1,2), Elena N. Ieno (1,2), Chris S. Elphick (3)
#
#   1 Highland Statistics Ltd., 6 Laverock Road, Newburgh, AB41 6FN, UK
#   2 University of Aberdeen, Oceanlab, Main Street, Newburgh, AB41 6AA, UK
#   3 University of Connecticut, Department of Ecology and Evolutionary Biology and Center for Conservation Biology, 75 N. Eagleville Road, U-43, Storrs, CT 06269-3043, USA
#
#
#   This file was produced by:
#   Alain Zuur (highstat@highstat.com)
#   www.highstat.com
#
#   A detailed explanation of the R code used in the paper can be found in:
#   A Beginner's Guide to R (2009).
#   Zuur, AF, Ieno, EN, Meesters, EHWG. Springer
#   http://www.springer.com/statistics/computational/book/978-0-387-93836-3
#
#
######################################################################
#    DISCLAIMER
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
######################################################################
#
#    The R code below was tested on R version 2.9.1
#    October 2009

####################################################################################################

## 1. OUTLIERS: observations that stick out from the rest ##

####################################################################################################

## First with Zuur data ##

#Figure 2: notice difference between boxplot and dotchart

Sparrows <- read.table(file = "resources/SparrowsElphick.txt", header = TRUE)

par(mfrow= c (1,2), mar = c(5,4,2,1))
boxplot(Sparrows$wingcrd,  ylab = "Wing length (mm)")
dotchart(Sparrows$wingcrd, xlab = "Wing length (mm)",
         ylab = "Order of the data")


#Figure 3: notice outliers with dotcharts
library(lattice)
Z <- cbind(Sparrows$wingcrd, Sparrows$tarsus,  Sparrows$head,
           Sparrows$culmen,  Sparrows$nalospi, Sparrows$wt)

colnames(Z) <- c("wing length", "tarsus length", "head length",
                 "culmen length", "nalospi to bill tip", "weight")

dotplot(as.matrix(Z), groups = FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")


## Then with Garibaldi data ##
cov.dat <- read.csv('trampling_analyses/compiled_data/plant-percent-cover.csv')

# Look at data
head(cov.dat) #first 6 rows - do these look correct?
str(cov.dat) #structure of each column - are characters read as characters and numbers as numeric?
summary(cov.dat) #summary stats - are there 1-28 tansects, 10 quads, and a range of percent cover values?

## PROBLEM: There are only 24 transects, when there should be 28
## DIAGNOSIS: Many transects missing
## SOLUTION: Look through raw data and find data entry errors (Carly***)

length(unique(cov.dat$Transect)) #check how many transect in data
sort(unique(cov.dat$Transect)) #check for missing transects

# Plot data
par(mfrow= c (2,2), mar = c(5,4,2,1))
hist(cov.dat$X..Cover, breaks = 20)
boxplot(cov.dat$X..Cover,  ylab = "Percent Cover")
dotchart(cov.dat$X..Cover, xlab = "Percent Cover",
         ylab = "Order of the data")

# Check what percentage of data are above 0.55 (aka what look like outlier values)
outs <- table(cov.dat$X..Cover > 0.55)[2] #number of data points > 0.55
ins <- length(cov.dat$X..Cover <= 0.55) #number of total data points
outs/ins #5%

# Identify outliers
outs.id <- cov.dat %>% 
  
  filter(X..Cover > 0.55) %>% #filter out percent cover data > 0.55
  
  mutate(trans_quad = paste(Transect, Quadrant, sep = '_')) #give each trans-quad pair a unique ID

outs.id$trans_quad #these are the transect-quad photos that need to be checked


## NEED TO CHECK THESE OUTLIERS: DO THESE PHOTOS SHOW SUCH HIGH PLANT PERCENT COVER?
## (Carly***)

## ALSO CHECK PHOTOS WITH MOSS OR LOGS IN THEM TO SEE WHERE THESE PERCENT COVER VALUES LIE IN 
## RELATION WITH GENERAL DISTRIBUTION.


## If you detect outliers:

## 1. Check raw data for errors and assess if values are reasonable

## 2. Discriminant analysis on full data set to check if axes are mainly determined by outliers

## 3. Explore possibility of variable transformation

## 4. Choose statistical method that uses prob. distribution that allows for greater variation for
## large mean values in response variable




####################################################################################################

## 2. HOMOGENEITY OF VARIANCE: variance in residuals is constant ##

####################################################################################################

Godwits <- read.table(file="Godwits.txt", header=TRUE)

Godwits$fSEX <- factor(Godwits$SEX, levels = c(0, 1, 2),
                       labels = c("Not", "Female", "Male"))
Godwits$fPERIOD <- factor(Godwits$PERIOD, levels = c(0, 1, 2),
                          labels = c("Summer", "Pre-migration", "Winter"))


## Plot residuals of fitted model

fit <- lm(mgconsumed ~ fPERIOD * SEX, data = Godwits) #linear regression with interaction

rr <- resid(fit) #residuals
ff <- fitted(fit) #fitted values

par(mfrow = c(1,1))
plot(rr ~ ff, xlab = 'Predicted Values', ylab = 'Residuals')


## If you have heterogeneity of variance: 

## 1. Transformation of response variable to stabilize variance

## 2. Use statistical technique that doesn't require homogeneity


# #Figure 4: Godwit intake rates
# 
# #Data info:
# #Sex
# #sex 1=female		0 should go out
# #sex 2=male
# 
# #Age
# #1= adult
# #2= juvenile
# #0=UNKNOWN
# 
# #Location
# #locationa=0
# #locationb=1
# 
# #Period
# #period=0 ;southern summr
# #period=1; prepare for migration
# #period=2; sourthern winter
# 
# 
# library(lattice)
# 
# 
# bwplot(mgconsumed ~ fPERIOD | fSEX, data = Godwits,
#    strip = strip.custom(bg = 'white'),   subset = SEX!=0,
#    cex = .5, layout = c(2, 1),
#    xlab = "Migration period", ylab = "Intake rate",
#    par.settings = list(
#       box.rectangle = list(col = 1),
#       box.umbrella  = list(col = 1),
#       plot.symbol   = list(cex = .5, col = 1)),
#        scales = list(x = list(relation = "same"),
#                      y = list(relation = "same")))




####################################################################################################

## 3. ASSUMPTION OF NORMALITY: residuals are normally distributed ##

####################################################################################################

# Plot residuals

par(mfrow = c(1,2))

# Histogram of residuals: can see distribution is skewed (has a longer right-hand tail)
hist(rr, breaks = 100)

# Normal qq-plot of residuals: a better way to check for normality
qqnorm(rr, main="normal qq-plot, residuals")
qqline(rr)


# #Figure 5
# Sparrows <- read.table(file="SparrowsElphick.txt", header=TRUE)
# 
# Sparrows$fMonth<-factor(Sparrows$Month,
#                         levels = c(5, 6, 7, 8, 9, 10),
#                         labels = c("May", "June", "July", "August",
#                                    "Sept.", "Oct."))
# 
# 
# Sparrows$I1 <- Sparrows$fMonth =="June" |
#                Sparrows$fMonth =="July" |
#                Sparrows$fMonth =="August"
# 
# 
# hist(Sparrows$wt[Sparrows$I1],
#      xlab = "Weight (g)", breaks = 30,
#      main = "", ylab = "Frequency")
# 
# 
# 
# library(lattice)
# histogram( ~ wt | fMonth, type = "count",
#     xlab = "Weight (g)",
#     ylab = "Frequency",
#     nint=30,layout=c(1,3),
#     strip.left = strip.custom(bg = 'white'),
#     strip = F,
#     col.line = "black", col = "white",
#     scales = list(x = list(relation = "same"),
#                   y = list(relation = "same"),
#                   draw = TRUE),
#     subset = fMonth =="June" | fMonth == "July" |fMonth == "August",
#     data = Sparrows)




####################################################################################################

## 4. ZERO INFLATION: lots of zeros in the data ##

####################################################################################################

## Note: can also use a histogram to check this


#Figure 7: frequency of observed values

RiceField <- read.table(file="ElphickBirdData.txt", header = TRUE)
par(mar = c(4, 4, 3, 2), mfrow = c(1,1))
plot(table(round(RiceField$AREA * RiceField$AQBIRDS)),
     type = "h",
     xlim = c(0, 100),
     xlab = "Observed values", ylab = "Frequency")


## If you have zero-inflation: 

## 1. Consider statistical approaches that can deal with zero-inflated data (e.g. zero inflated GLMs)

## 2. Be careful of double zeros (e.g., two species both absent in certain sets of conditions)



# #Figure 8: checking for double zeros
# 
# RiceField <- read.table(file="ElphickBirdData.txt", header = TRUE)
# 
# #These are all the species
# AllS <- c(
# "TUSW",     "GWFG",     "WHGO",     "CAGO",     "MALL",
# "GADW",     "GWTE",     "CITE",     "UNTE",     "AMWI",     "NOPI",
# "NOSH",     "RIDU",     "CANV",     "BUFF",     "WODU",     "RUDU",
# "EUWI",     "UNDU",     "PBGB",     "SORA",     "COOT",     "COMO",
# "AMBI",     "BCNH",     "GBHE",     "SNEG",     "GREG",     "WFIB",
# "SACR",     "AMAV",     "BNST",     "BBPL",     "KILL",     "LBCU",
# "GRYE",     "LEYE",     "LBDO",     "SNIP",     "DUNL",     "WESA",
# "LESA",     "PEEP",     "RUFF",     "UNSH",     "RBGU",     "HEGU",
# "CAGU",     "GUSP")
# 
# #Determine species richness
# Richness <- colSums(RiceField[,AllS] > 0, na.rm = TRUE)
# 
# #Remove all covariates
# Birds  <- RiceField[,AllS]
# 
# #To reduce the of variables in the figure, we only used the
# #20 species that occured at more than 40 sites.
# #As a result, N = 20. Else it becomes a mess.
# Birds2 <- Birds[, Richness > 40]
# N <- ncol(Birds2)
# 
# 
# AllNames <- names(Birds2)
# A <- matrix(nrow = N, ncol = N)
# 
# for (i in 1:N){
#   for (j in 1:N){
#     A[i,j] <- sum(RiceField[,AllS[i]]==0  & RiceField[,AllS[j]]==0, na.rm=TRUE)
#     }}
# 
# 
# A1 <- A/2035
# print(A1, digits = 2)
# rownames(A1) <- AllNames
# colnames(A1) <- AllNames
# 
# 
# library(lattice)
# 
# panel.corrgram.2 <- function(x, y, z, subscripts, at = pretty(z), scale = 0.8, ...)
# {
#     require("grid", quietly = TRUE)
#     x <- as.numeric(x)[subscripts]
#     y <- as.numeric(y)[subscripts]
#     z <- as.numeric(z)[subscripts]
#     zcol <- level.colors(z, at = at, ...)
#     for (i in seq(along = z))
#     {
#         lims <- range(0, z[i])
#         tval <- 2 * base::pi *
#             seq(from = lims[1], to = lims[2], by = 0.01)
#         grid.polygon(x = x[i] + .5 * scale * c(0, sin(tval)),
#                      y = y[i] + .5 * scale * c(0, cos(tval)),
#                      default.units = "native",
#                      gp = gpar(fill = zcol[i]))
#         grid.circle(x = x[i], y = y[i], r = .5 * scale,
#                     default.units = "native")
#     }
# }
# 
# 
# levelplot(A1,xlab=NULL,ylab=NULL,
#     at=do.breaks(c(0.5,1.01),101),
#     panel=panel.corrgram.2,
#     scales=list(x=list(rot=90)),
#     colorkey=list(space="top"),
#     col.regions=colorRampPalette(c("red","white","blue")))
# 
# 
# #Grey colours
# levelplot(A1,xlab=NULL,ylab=NULL,
#     at=do.breaks(c(0.5,1.01),101),
#     panel=panel.corrgram.2,
#     scales=list(x=list(rot=90)),
#     colorkey=list(space="top"),
#     col.regions=colorRampPalette(c(grey(0.8),grey(0.5),grey(0.2))))




####################################################################################################

## 5. COVARIATE COLLINEARITY: explanatory variables are correlated ##

####################################################################################################

# #Figure 9 & Table 1
Sparrows2 <- read.table(file = "VegSamplesV1.txt", header = TRUE)
# #Different Sparrow object
# 
# names(Sparrows2)
# [1] "Year"                "Site"                "UniversalPlotName"
# [4] "Banded"              "PtCountsum"          "Avgmaxht"
# [7] "Avgdens"             "ht.thatch"           "S.patens"
#[10] "Distichlis"          "S.alternifloraShort" "S.alternifloraTall"
#[13] "Juncus"              "Bare"                "Other"
#[16] "Phragmites"          "Shrub"               "Tallsedge"
#[19] "Water"

#Load our own library files
source("HighstatLib.R")

#Select covariates
Z<-Sparrows2[,c("Avgmaxht", "Avgdens", "ht.thatch",
                "S.patens", "Distichlis")]

# corvif(Z)      #Part of Table 1

## Scatterplot matrix: plot all variables against each other

pairs(Z) #tip: only plot your explanatory variables to better vizualize correlations
cor(Z)


## What to do if you have collinearity:

## 1. Calculate variance inflation factors (VIFs); see code for Table 1 below

corvif(Z)

## 2. Sequentially drop covariate with highest VIF, recalculate VIFs, and repeat until all VIFs are
## smaller than pre-selcted threshold (2-10, rules of thumb on this vary)

## 3. Final model will then illustrate that explanatory variables are sig. that wouldn't be in the 
## presence of collinearity


# 
# #Run linear regression
# M1<-lm(Banded~Avgmaxht + Avgdens + ht.thatch + S.patens +
#               Distichlis + S.alternifloraShort + S.alternifloraTall +
#               Juncus + Bare + Other + Phragmites + Shrub + Tallsedge +
#                Water, data = Sparrows2)
# summary(M1)    #Part of Table 1
# 
# 
# #Chop out covariates
# Z<-Sparrows2[,c("ht.thatch", "S.patens", "Distichlis",
#                 "S.alternifloraShort", "Juncus", "Bare", "Other",
#                 "Phragmites", "Shrub", "Tallsedge", "Water")]
# corvif(Z)      #Part of Table 1
# 
# 
# #Linear regression on subset
# M2 <- lm(Banded ~ ht.thatch + S.patens +
#                   Distichlis + S.alternifloraShort +
#                   Juncus + Bare + Other + Phragmites + Shrub + Tallsedge +
#                   Water, data = Sparrows2)
# summary(M2)    #Part of Table 1
# 
# 
# M2 <- lm(Banded ~ Juncus + Shrub, data = Sparrows2)
# drop1(M2, test = "F")
# coeff(M1)
# step(M2)
# 
# M3 <- lm(Banded ~ Juncus+Shrub, data = Sparrows2)
# summary(M3)    #Part of Table 1
# 
# 
# #Figure 9
# Z <- as.vector(as.matrix(Sparrows2[, c("Avgmaxht", "Avgdens",
#               "ht.thatch", "S.patens", "Distichlis",
#               "S.alternifloraShort", "S.alternifloraTall", "Juncus",
#               "Bare", "Other", "Phragmites", "Shrub", "Tallsedge", "Water")]))
# 
# 
# #Setup the data in vector format for the xyplot
# Y10 <- rep(Sparrows2$Banded, 14)
# 
# MyNames <- names(Sparrows2[,c("Avgmaxht", "Avgdens", "ht.thatch",
#                 "S.patens", "Distichlis", "S.alternifloraShort",
#                 "S.alternifloraTall", "Juncus", "Bare", "Other",
#                 "Phragmites", "Shrub", "Tallsedge", "Water")])
# 
# ID10 <- rep(MyNames, each = length(Sparrows2$Banded))
# library(lattice)
# 
# 
# ID11 <- factor(ID10, labels = c("% Juncus gerardii",
#                "% Shrub", "Height of thatch", "% Spartina patens",
#                "% Distichlis", "% Bare ground", "% Other vegetation",
#                "% Phragmites australis", "% Tall sedge", "% Water",
#                "% Spartina alterniflora (short)",
#                "% Spartina alterniflora (tall)",
#                "Maximum vegetation height",
#                "Vegetation stem density"),
#                levels = c("Juncus", "Shrub", "Avgmaxht", "S.patens",
#                           "Distichlis", "Bare", "Other", "Phragmites",
#                           "Tallsedge", "Water", "S.alternifloraShort",
#                           "S.alternifloraTall", "ht.thatch", "Avgdens"))
# 
# 
# xyplot(Y10 ~ Z | ID11, col = 1,
#   strip = function(bg='white',...) strip.default(bg='white',...),
#   scales = list(alternating = T,
#                 x = list(relation = "free"),
#                 y = list(relation = "same")),
#   xlab = "Covariates",
#   par.strip.text = list(cex = 0.8),
#   ylab = "Banded",
#   panel=function(x, y, subscripts,...){
#     panel.grid(h =- 1, v = 2)
#     panel.points(x, y, col = 1, pch = 16)
#     if(ID10[subscripts][1] != "Tallsedge") {panel.loess(x,y,col=1,lwd=2)}
#     })



# #Figure 10
# 
# Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)
# source(file = "HighstatLib.R")
# MyNames <- c("wing chord", "tarsus length", "head length",
#              "culmen length", "nalospi to bill tip", "weightt")
# pairs(Sparrows[,c(1, 3, 4, 5, 6, 7)],
#       lower.panel = panel.cor,
#       cex.labels=1.3,
#       labels=MyNames)




####################################################################################################

## 7. INTERACTIONS: relationship between Y_1 and X changes as Y_2 changes

####################################################################################################

#Figure 11: coplot to visualize potential presence of interactions
Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)

#Take the data from species 1, Sex = 0 and Wing length >= 65
I1 <- Sparrows$SpeciesCode == 1 & Sparrows$Sex != "0" & Sparrows$wingcrd < 65
Wing1<- Sparrows$wingcrd[I1]
Wei1 <- Sparrows$wt[I1]
Mon1 <- factor(Sparrows$Month[I1])
Sex1<- factor(Sparrows$Sex[I1])


#Define Month and Sex as categorical variables
fMonth1 <- factor(Mon1,levels=c(5,6,7,8,9),
                  labels=c("May","Jun","Jul","Aug","Sep"))
fSex1   <- factor(Sex1, levels=c(4,5),labels=c("Male","Female"))

M1 <- lm(Wei1 ~ Wing1*fMonth1*fSex1)
summary(M1)
anova(M1)


#Make the coplot: if all bivariate regression lines are parallel, then there are probably no 
#significant interactions

coplot(Wei1 ~ Wing1 | fMonth1 * fSex1, ylab = "Weight (g)",
       xlab = "Wing length (mm)",
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })


## Including interactions in your models:

## 1. First visually check with a coplot

## 2. Test for significant interaction by including it as a term in linear model

## Note: interactions only make sense to include if they are ecologically relevant!



####################################################################################################

## 8. INDEPENDENT OBSERVATIONS OF RESPONSE VARIABLE

####################################################################################################

## If observations are not independent (in order of best approach): 

## 1. Consider using hierarchical models, which can account for this (e.g. mixed effects models, such 
## as linear mixed models)

## 2. Take mean value within a grouping to get rid of pseudoreplication if don't want to use 
## hierarchical models (but greatly reduces variance)

## 3. Randomly sample within each random effect (but greatly reduces variance)

## NOTE 1: If you have a nested sampling design, then your observations are not independent. 

## NOTE 2: Machine learning methods (e.g. boosted regression trees and others) generally do NOT 
## account for dependent observations.


# #Figure 12
# Waders <- read.table(file = "wader.txt", header = TRUE)
# 
# #Define the time axis
# Time <- seq(1,25)
# 
# par(mfrow = c(2, 2), mar = c(5, 4, 3, 2))
# plot(Time, Waders$C.fuscicolis, type = "l", xlab = "Time (2 weeks)",
#      ylab = "C. fuscicollis abundance")
# acf(Waders$C.fuscicolis, main = "C. fuscicollis ACF")
# 
# plot(Time, Waders$L.dominicanus, type = "l", xlab = "Time (2 weeks)",
#      ylab = "L. dominicanus abundance")
# acf(Waders$L.dominicanus, main = "L. dominicanus ACF")

#################################################################
