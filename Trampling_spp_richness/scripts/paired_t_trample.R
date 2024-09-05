# Data on trampling effects of near and far from trail
# Nov 2023 NH; Original version based on GEOG 374 Lab 6 by NH

#Clears the workspace
rm(list=ls())

# Ask R where it is right now
getwd()

## PAIRED T TEST on In Person Pre vs Post:

# BRING IN THE DATA
# load the dataset. You can either use file.choose command
mydata <- read.csv(file.choose())

# or use read.csv since our data is in .csv format
# use the command read.csv()
# mydata = read.csv("___.csv")
# mydata = read.csv("___.csv")

# This file has 5 columns, the first with species id #; the 2nd
# with species binomial; the 3rd with treatment/condition (1 = near trail and more trampled ; 2 = far from trail),
# the 4rd with the variable (mean frequency in quadrats across the study area)
# the 5th with the log values

## LOOK AT OUR DATA
mydata          # look at all your data
head(mydata)    # look at the first few entries
tail(mydata)    # look at the last few entries
str(mydata)     # look at the overall structure

#### Paired t test. The data must be ordered so that the first observation of Group 1
#is the same subject as the first observation of Group 2
t.test(meanFreq ~ Treat, data = mydata, paired = TRUE) # this is two-tailed; p=

t.test(meanFreq ~ Treat, data = mydata, paired = TRUE, alternative = "less") # this is one tailed and is more appropriate to our hypothesis
# t.test(Freq ~ Treat, data = mydata, paired = TRUE, alternative = "greater") # this is one tailed but in the wrong direction - don't use

# log values paired t
#### Paired t test. The data must be ordered so that the first observation of Group 1
#is the same subject as the first observation of Group 2
t.test(LogMeanFreq ~ Treat, data = mydata, paired = TRUE) # this is two-tailed; p=

t.test(LogMeanFreq ~ Treat, data = mydata, paired = TRUE, alternative = "less") # this is one tailed and is more appropriate to our hypothesis
# Results: Significantly different:

######
#But is this normal? Need to check and possibly transform, e.g., log data

## LOOK AT OUR DATA with BOXPLOTS

# untransformed
boxplot(Freq ~ Treat, data = mydata, col = "lightgray",
        varwidth = TRUE, notch = FALSE, main = "Frequency by Trampling Treatment",
        ylab = "Frequency")

# Log data (log transformed)
boxplot(Log ~ Treat, data = mydata, col = "lightgray",
        varwidth = TRUE, notch = FALSE, main = "Frequency by Trampling Treatment",
        ylab = "Log Frequency")

# ADJUST WHISKERS TO MIN AND MAX
# MAKE IT HORIZONTAL

boxplot(Log ~ Treat, data = mydata,
        xlab="Log Frequency",
        ylab="Treatment",
        main="Frequency by Trampling Treatment",
        col=c("red","blue"),
        range = 0,
        horizontal=TRUE)


## HISTOGRAMS

#INDEXING: pull them out to plot
Treat.1 = mydata$Freq[mydata$Treat==1]
Treat.2 = mydata$Freq[mydata$Treat==2]

hist(Treat.1,
     xlab = "Frequency",
     main="Near Trail",
     col = "red")
hist(Treat.2,
     xlab = "Frequency",
     main="Far Trail",
     col = "blue")

# Put them together:
par(mfrow=c(1,2))
hist(Treat.1,
     xlab = "Frequency",
     main="Near Trail",
     col = "red")
hist(Treat.2,
     xlab = "Frequency",
     main="Far Trail",
     col = "blue")
## Histograms show data is strongly right skewed. Must transform and try again.
## Log values
#INDEXING: pull them out to plot
Cond.1 = mydata$Log[mydata$Treat==1]
Cond.2 = mydata$Log[mydata$Treat==2]

hist(Cond.1,
     xlab = "Frequency",
     main="Near Trail",
     col = "red")
hist(Cond.2,
     xlab = "Frequency",
     main="Far Trail",
     col = "blue")

# Put them together:
par(mfrow=c(1,2))
hist(Cond.1,
     xlab = "Frequency",
     main="Near Trail",
     col = "red")
hist(Cond.2,
     xlab = "Frequency",
     main="Far Trail",
     col = "blue")
## better, though Far from trail is bimodal.

