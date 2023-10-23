# Aims: Manuscript tables
# 1. MS stats
# 2. Bayesian results
# 3. BRMS details (created manually)


# Author: Nathalie Chardon
# Date created: 14 June 2023
# Date updated: 26 June 2023 (NC)

# # LIBRARIES # #
library(tidyverse)
library(lmerTest)


rm(list=ls()) 


# # INPUT FILES # #
load('trampling_analyses/compiled_data/quad.RData') ##updated with reproductive metric & plant area (repro.R)
#[individual BRMS model files, see below]


# # OUTPUT FILES # #
load('trampling_analyses/outputs/test_int/brm_table.RData') #model results for species traits (ms_tables.R)
brm.tab <- read.csv('trampling_analyses/outputs/test_int/brm_table.csv') #model results for species traits (ms_tables.R)





####################################################################################################

# # MS Stats

####################################################################################################

# # Is lower reproduction on trail compared to off trail because plant sizes are lower?
# Data
load('trampling_analyses/compiled_data/quad.RData') ##updated with reproductive metric & plant area (repro.R)

totalReproStruct <- quad$flws + quad$frts + quad$buds #total repro structures
quad <- cbind(quad, totalReproStruct)

# Across all species
sum_stats <- quad %>% 
  filter(!is.na(totalReproStruct)) %>% 
  group_by(dist) %>% 
  summarize(max = max(totalReproStruct), min = min(totalReproStruct), 
            avg = mean(totalReproStruct), med = median(totalReproStruct)) 
sum_stats #median and mean repro is lower on trail

# Per species
sum_stats <- quad %>% 
  filter(!is.na(totalReproStruct) & repro < 15) %>% #this needs to be first
  group_by(species, dist) %>% 
  summarize(max = max(totalReproStruct), min = min(totalReproStruct), 
            avg = mean(totalReproStruct), med = median(totalReproStruct)) 
sum_stats #median repro is lower on trail

# Histogram of reproduction shows less repro on trail, but this could also be due to less plants
trail <- quad %>% 
  filter(dist == 1 & totalReproStruct > 0) 
offtrail <- quad %>% 
  filter(dist == 0 & totalReproStruct > 0) 

hist(offtrail$totalReproStruct, breaks = 50)
hist(trail$totalReproStruct, breaks = 50, col = 'red', add = T)

# Test that reproduction is smaller with smaller plants per species
plot(totalReproStruct ~ plantArea_cm2, data = quad)

mod <- lmer(totalReproStruct ~ plantArea_cm2 + (1|species) + (1|trans.pair), data = quad)
summary(mod) #larger plants have more repro structures




####################################################################################################

# # Table 1: Estimated parameters of hierarchical models

####################################################################################################

## PLANT PERCENT COVER ## --------------------------------------------------------------------------
sp <- '[All Plants]'

# PLANT PERCENT COVER
mod <- readRDS('trampling_analyses/outputs/test_int/perc-cov_beta_int.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')
ff <- ss$formula[5][[1]][[1]]
ess <- round(min(ss$fixed[6]), 0)
de <- paste(round(ss$fixed[4, 1], 2), '(', round(ss$fixed[4, 3], 2), ',', round(ss$fixed[4, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Intercept = ii, Disturbance = dd, Elevation = ee, `Dist*Elev` = de,
                      Family = ff, Iterations = ss$iter, `Bulk ESS` = ess)
brm.res

brm.tab <- brm.res
brm.tab


## PHYEMP ## --------------------------------------------------------------------------------------
sp <- 'phyemp'

# PHYEMP Height
mod <- readRDS('trampling_analyses/outputs/test_int/phyemp_height_nb_int.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')
ff <- ss$formula[5][[1]][[1]]
ess <- round(min(ss$fixed[6]), 0)
de <- paste(round(ss$fixed[4, 1], 2), '(', round(ss$fixed[4, 3], 2), ',', round(ss$fixed[4, 4], 2), ')')
  
brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Intercept = ii, Disturbance = dd, Elevation = ee, `Dist*Elev` = de,
                      Family = ff, Iterations = ss$iter, `Bulk ESS` = ess)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# PHYEMP Diameter
mod <- readRDS('trampling_analyses/outputs/test_int/phyemp_diam_nb_int.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')
ff <- ss$formula[5][[1]][[1]]
ess <- round(min(ss$fixed[6]), 0)
de <- paste(round(ss$fixed[4, 1], 2), '(', round(ss$fixed[4, 3], 2), ',', round(ss$fixed[4, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Intercept = ii, Disturbance = dd, Elevation = ee, `Dist*Elev` = de,
                      Family = ff, Iterations = ss$iter, `Bulk ESS` = ess)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# PHYEMP Reproduction
mod <- readRDS('trampling_analyses/outputs/test_int/phyemp_repro_beta_int.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')
ff <- ss$formula[5][[1]][[1]]
ess <- round(min(ss$fixed[6]), 0)
de <- paste(round(ss$fixed[4, 1], 2), '(', round(ss$fixed[4, 3], 2), ',', round(ss$fixed[4, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Intercept = ii, Disturbance = dd, Elevation = ee, `Dist*Elev` = de,
                      Family = ff, Iterations = ss$iter, `Bulk ESS` = ess)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


## CASMER ## --------------------------------------------------------------------------------------
sp <- 'casmer'

# CASMER Height
mod <- readRDS('trampling_analyses/outputs/test_int/height_nb_casmer_int.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')
ff <- ss$formula[5][[1]][[1]]
ess <- round(min(ss$fixed[6]), 0)
de <- paste(round(ss$fixed[4, 1], 2), '(', round(ss$fixed[4, 3], 2), ',', round(ss$fixed[4, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Intercept = ii, Disturbance = dd, Elevation = ee, `Dist*Elev` = de,
                      Family = ff, Iterations = ss$iter, `Bulk ESS` = ess)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# CASMER Diameter
mod <- readRDS('trampling_analyses/outputs/test_int/diam_nb_casmer_int.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')
ff <- ss$formula[5][[1]][[1]]
ess <- round(min(ss$fixed[6]), 0)
de <- paste(round(ss$fixed[4, 1], 2), '(', round(ss$fixed[4, 3], 2), ',', round(ss$fixed[4, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Intercept = ii, Disturbance = dd, Elevation = ee, `Dist*Elev` = de,
                      Family = ff, Iterations = ss$iter, `Bulk ESS` = ess)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# CASMER Reproduction
mod <- readRDS('trampling_analyses/outputs/test_int/repro_beta_casmer_int.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')
ff <- ss$formula[5][[1]][[1]]
ess <- round(min(ss$fixed[6]), 0)
de <- paste(round(ss$fixed[4, 1], 2), '(', round(ss$fixed[4, 3], 2), ',', round(ss$fixed[4, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Intercept = ii, Disturbance = dd, Elevation = ee, `Dist*Elev` = de,
                      Family = ff, Iterations = ss$iter, `Bulk ESS` = ess)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


## VACOVA ## --------------------------------------------------------------------------------------
sp <- 'vacova'

# VACOVA Height
mod <- readRDS('trampling_analyses/outputs/test_int/height_nb_vacova_int.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')
ff <- ss$formula[5][[1]][[1]]
ess <- round(min(ss$fixed[6]), 0)
de <- paste(round(ss$fixed[4, 1], 2), '(', round(ss$fixed[4, 3], 2), ',', round(ss$fixed[4, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Intercept = ii, Disturbance = dd, Elevation = ee, `Dist*Elev` = de,
                      Family = ff, Iterations = ss$iter, `Bulk ESS` = ess)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# VACOVA Diameter
mod <- readRDS('trampling_analyses/outputs/test_int/diam_nb_vacova_int.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')
ff <- ss$formula[5][[1]][[1]]
ess <- round(min(ss$fixed[6]), 0)
de <- paste(round(ss$fixed[4, 1], 2), '(', round(ss$fixed[4, 3], 2), ',', round(ss$fixed[4, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Intercept = ii, Disturbance = dd, Elevation = ee, `Dist*Elev` = de,
                      Family = ff, Iterations = ss$iter, `Bulk ESS` = ess)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# VACOVA Reproduction
mod <- readRDS('trampling_analyses/outputs/test_int/repro_beta_vacova_int.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')
ff <- ss$formula[5][[1]][[1]]
ess <- round(min(ss$fixed[6]), 0)
de <- paste(round(ss$fixed[4, 1], 2), '(', round(ss$fixed[4, 3], 2), ',', round(ss$fixed[4, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Intercept = ii, Disturbance = dd, Elevation = ee, `Dist*Elev` = de,
                      Family = ff, Iterations = ss$iter, `Bulk ESS` = ess)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


## CARSPP ## --------------------------------------------------------------------------------------
sp <- 'carspp'

# CARSPP Height
mod <- readRDS('trampling_analyses/outputs/test_int/height_nb_carspp_int.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')
ff <- ss$formula[5][[1]][[1]]
ess <- round(min(ss$fixed[6]), 0)
de <- paste(round(ss$fixed[4, 1], 2), '(', round(ss$fixed[4, 3], 2), ',', round(ss$fixed[4, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Intercept = ii, Disturbance = dd, Elevation = ee, `Dist*Elev` = de,
                      Family = ff, Iterations = ss$iter, `Bulk ESS` = ess)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# CARSPP Diameter
mod <- readRDS('trampling_analyses/outputs/test_int/diam_nb_carspp_int.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')
ff <- ss$formula[5][[1]][[1]]
ess <- round(min(ss$fixed[6]), 0)
de <- paste(round(ss$fixed[4, 1], 2), '(', round(ss$fixed[4, 3], 2), ',', round(ss$fixed[4, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Intercept = ii, Disturbance = dd, Elevation = ee, `Dist*Elev` = de,
                      Family = ff, Iterations = ss$iter, `Bulk ESS` = ess)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


## CLEAN UP & SAVE RESULTS TABLE ## ----------------------------------------------------------------
brm.tab <- brm.tab %>% 
  rename(Trait = resp) #rename column

brm.tab <- brm.tab %>% 
  mutate(Species = if_else(Species == 'phyemp', 'P. empetriformis', Species)) %>% #replace species codes with species names
  mutate(Species = if_else(Species == 'casmer', 'C. mertensiana', Species)) %>% 
  mutate(Species = if_else(Species == 'vacova', 'V. ovalifolium', Species)) %>% 
  mutate(Species = if_else(Species == 'carspp', 'Carex spp.', Species)) %>% 
  mutate(Trait = if_else(Trait == 'heightmm', 'Height', Trait)) %>% #rename trait variables
  mutate(Trait = if_else(Trait == 'mxdiammm', 'Diameter', Trait)) %>%
  mutate(Trait = if_else(Trait == 'relrepro', 'Reproduction', Trait)) %>% 
  mutate(Trait = if_else(Trait == 'perccov', 'Percent Cover', Trait)) %>% 
  mutate(Family = if_else(Family == 'negbinomial', 'Negative Binomial', Family)) %>% #rename family
  mutate(Family = if_else(Family == 'beta', 'Beta', Family))
brm.tab


save(brm.tab, file = 'trampling_analyses/outputs/test_int/brm_table.RData')
write.csv(brm.tab, file = 'trampling_analyses/outputs/test_int/brm_table.csv', row.names = F)




####################################################################################################

# # Table 2: Measured parameters

####################################################################################################

# Filled out manually by CH

# Scale of plant measurements
load('trampling_analyses/compiled_data/quad.RData') ##updated with reproductive metric & plant area (repro.R)

min(quad$height_mm, na.rm = T) #height
max(quad$height_mm, na.rm = T)

min(quad$mxdiam_mm, na.rm = T) #diameter
max(quad$mxdiam_mm, na.rm = T)

dat$reprocounts <- NA #initialize column
for (i in 1:nrow(dat)) { #loop through each row of DF
  
  dat$reprocounts[i] <- sum(c(dat$buds[i], dat$flws[i], dat$frts[i]), na.rm = T)
}
summary(dat$reprocounts)




####################################################################################################

# # Table S1: Model fitting details

####################################################################################################

# Manually created when running models in bayesian.R; ohter info taken from Table [RESULTS] above


