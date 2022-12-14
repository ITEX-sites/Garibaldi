###*** Collaborators: this indicates where to make changes/edits

# Aims:
# 1. Set up data
# 2. Choose appropriate model structure with AICc
# 3. Fit LMMs to test effects of trampling and elevation on plant responses
# 4. Check model residuals & goodness of fit

# Author: Nathalie Chardon
# Date created: 11 Nov 2022
# Date updated: 9 Dec 2022 (NC) ###*** edit here if updating script


# # LIBRARIES # # 
# install.packages('tidyverse', lmerTest', 'AICcmodavg', 'MuMIn') ###*** if not installed
library(tidyverse)
library(lmerTest) #lmer from lme4 with added p-values
library(AICcmodavg) #calculate AICc


rm(list=ls()) 

###*** create your own new object 'comp_dat' to mirror your computer's filepath to this folder
# # WORKING DIRECTORIES # #
comp_dat <- '~/Desktop/Code/Garibaldi/trampling_analyses/compiled_data/' #WD for NC


# # INPUT FILES # #
setwd(comp_dat)
load('quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)


# # OUTPUT FILES # #




####################################################################################################

# # DATA # # 

####################################################################################################

# Data
dat <- quad #generic dataframe name
str(dat)


# Create transect pair variable to use as random effect
dat <- dat[order(dat$transect.no), ] #order dataframe by transect number

dat$trans.pair <- NA #set up empty vector
oo <- seq(1, 27, 2) #sequence of all odd transects to create pair numbers
ee <- seq(2, 28, 2) #sequence of all even transects to create pair numbers

for (i in 1:nrow(dat)) { #loop through each data row
  
  for (j in 1:14) { #loop through each pair
    
    if (dat$transect.no[i] == oo[j] | dat$transect.no[i] == ee[j]) {
      
      dat$trans.pair[i] <- paste(oo[j], ee[j], sep = '-')
    }
  }
}


# Convert categorical predictor variables to factor
ff <- c('transect', 'species', 'dist', 'trans.pair')
dat[ff] <- lapply(dat[ff], as.factor)

str(dat) #check data structure


###*** TO DO:
# Calculate reproductive metric combining buds, flws, frts and standardize by plant diameter 


# Check for correlation in predictor variables
# - not needed for this analyses because only 1 continuous variable - 


# Calculate variance inflation factor (VIF)
# - not needed for this analyses because only 1 continuous variable - 




####################################################################################################

# # IDENTIFY BEST MODEL STRUCTURE # # 

####################################################################################################

# # Model types

# Fixed Effects (FE): dist + altitude; Random Effects (RE): trans.pair + species
mod1 <- lmer(height_mm ~ dist + altitude + (1|trans.pair) + (1|species), data = dat, REML = F) #REML = F because need log-likelihood for AICc

# FE: dist * altitude; RE: trans.pair + species
mod2 <- lmer(height_mm ~ dist * altitude + (1|trans.pair) + (1|species), data = dat, REML = F)

# FE: dist + altitude; RE: trans.pair (slopes) + species (intercept)
mod3 <- lmer(height_mm ~ dist + altitude + (species|trans.pair), data = dat, REML = F) #convergence issues => drop

# FE: dist * altitude; RE: trans.pair (slopes) + species (intercept)
mod4 <- lmer(height_mm ~ dist * altitude + (species|trans.pair), data = dat, REML = F) #convergence issues => drop

# FE: dist + altitude; RE: trans.pair (slopes)
mod5 <- lmer(height_mm ~ dist + altitude + (1|trans.pair), data = dat, REML = F)

# FE: dist * altitude; RE: trans.pair (slopes)
mod6 <- lmer(height_mm ~ dist * altitude + (1|trans.pair), data = dat, REML = F)


# # Sort models by AICc
aictab(list(mod1 = mod1, mod2 = mod2, mod5 = mod5, mod6 = mod6)) #mod2 has lowest AICc => best model structure




####################################################################################################

# # COMPILE MODELS # # 

####################################################################################################

# Check out this resource for a visual explanation of hierarchical models: 
# http://mfviz.com/hierarchical-models/

# Question: Does disturbance have varying effects on plant height at different elevations?

# Fit model
mod <- lmer(height_mm ~ dist * altitude + (1|trans.pair) + (1|species), data = dat, REML = F)

# Model summary
summary(mod)

# Goodness of fit
ff <- fitted(mod) #predicted values
oo <- dat$height_mm #observed values
cor(ff, oo) #correlation between predicted and observed values



###*** TO DO: copy code above and modify to:
# Test other questions with additional measured response variables





####################################################################################################

# # CHECK MODEL ASSUMPTIONS # # 

####################################################################################################

# Homogeneity of variance: variance in residuals is constant

rr <- resid(mod) #residuals
ff <- fitted(mod) #fitted values

plot(rr ~ ff, xlab = 'Predicted Values', ylab = 'Residuals')


# Assumption of normality: residuals are normally distributed

par(mfrow = c(1,2))

hist(rr, breaks = 100)

qqnorm(rr, main="normal qq-plot, residuals")
qqline(rr)


###*** TO DO: 
# Check additional assumptions to check with hierarchical models


