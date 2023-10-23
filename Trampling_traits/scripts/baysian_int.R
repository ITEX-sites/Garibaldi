# Aims:
# 1. Analyses in Bayesian framework to model disturbance*elev effects on plant height, diameter, repro, and plant cover
# 2. Visualize model fit
# 3. Plot results

# Note: script revised from bayesian.R

# Author: Courtney Collins, Nathalie Chardon
# Date created: 21 June 2023
# Date updated: 28 July 2023 (NC)

# # LIBRARIES # #
library(rjags)
library(R2jags)
library(brms)
library(bayesplot)
library(priorsense) # remotes::install_github("n-kall/priorsense")
library(tidyverse)
library(loo)


rm(list=ls()) 


# # INPUT FILES # #
load('trampling_analyses/compiled_data/quad.RData') ##updated with reproductive metric & plant area (repro.R)


# # OUTPUT FILES # #
# [rds model files for each species' trait, see below]




####################################################################################################

# # SET UP DATA # # 

####################################################################################################

# Coding Club tutorial: https://ourcodingclub.github.io/tutorials/brms/
# Towards Data Science: https://towardsdatascience.com/evaluating-bayesian-mixed-models-in-r-python-27d344a03016

# Data
load('trampling_analyses/compiled_data/quad.RData') #updated with reproductive metric & plant area (LMMs.R)

str(quad) #check that categorical explanatory variables are factors, others numeric

# Data transformations
quad <- quad %>% 
  
  #Transform 0s and 1s for Beta distribution repro models
  mutate(rel_repro = if_else(rel_repro == 0, 0.0001, rel_repro)) %>% 
  mutate(rel_repro = if_else(rel_repro == 1, 0.9999, rel_repro)) %>% 
  
  # Integer values for negative binomial distribution height & diam models
  mutate(height_mm = round(height_mm, 0)) %>% 
  mutate(mxdiam_mm = round(mxdiam_mm, 0)) 

summary(quad) #check that no infinite values exist


# Skew function (from: https://towardsdatascience.com/evaluating-bayesian-mixed-models-in-r-python-27d344a03016_)
skew <- function(y){ # Fisher-Pearson Skew function based on NIST definition
  n <- length(y)
  dif <- y - mean(y)
  skew_stat <- (sqrt(n-1)/(n-2))*n *(sum(dif^3)/(sum(dif^2)^1.5))
  return(skew_stat)
}




####################################################################################################

# # CHOOSE APPROPRIATE FAMILY FOR REPRO MODEL # #
# Family: Beta or ZI Beta
# Fixed effects: dist * altitude
# Random effects: (1|trans.pair)

####################################################################################################

## Model for REPRO for PHYEMP

dat <- quad %>% #rename DF
  filter_at(vars(rel_repro, dist, altitude), all_vars(!is.na(.))) %>% #remove NAs in variables used in model
  filter(species == 'phyemp')

repro_beta <- brms::brm(rel_repro ~ dist* altitude + (1|trans.pair), seed = 050523,
                        data = dat, family = Beta(link = "logit", link_phi = "log"), 
                        init = '0',
                        
                        # fitting information
                        chains = 3, iter = 5000, warmup = 1000, cores = 4, #for computers with 4 cores
                        file = 'trampling_analyses/outputs/test_int/phyemp_repro_beta.rds_int', file_refit = 'on_change')

repro_zibeta <- brms::brm(rel_repro ~ dist* altitude + (1|trans.pair), seed = 050523,
                        data = dat, family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"), 
                        init = '0',
                        
                        # fitting information
                        chains = 3, iter = 5000, warmup = 1000, cores = 4, #for computers with 4 cores
                        file = 'trampling_analyses/outputs/test_int/phyemp_zirepro_beta.rds_int', file_refit = 'on_change')

# CHECK MODELS

pp_check(repro_beta, ndraws = 100) 
pp_check(repro_zibeta, ndraws = 100)

pairs(repro_beta)
pairs(repro_zibeta)

ppc_stat(y = dat$rel_repro, yrep = posterior_predict(repro_beta, ndraws = 1000), stat = "skew") #very bad
ppc_stat(y = dat$rel_repro, yrep = posterior_predict(repro_zibeta, ndraws = 1000), stat = "skew") #very bad

beta_loo <- loo(repro_beta, save_psis = TRUE, cores=4)
w <- weights(beta_loo$psis_object)
ppc_loo_pit_overlay(dat$rel_repro, posterior_predict(repro_beta), lw = w) #very bad

zibeta_loo <- loo(repro_zibeta, save_psis = TRUE, cores=4)
w <- weights(zibeta_loo$psis_object)
ppc_loo_pit_overlay(dat$rel_repro, posterior_predict(repro_zibeta), lw = w) #very bad

loo_compare(beta_loo, zibeta_loo)

# # Conclusion: pp_check, skew, and PIT are almost identical and poor for both models, better elpd for Beta
# # => use Beta because better elpd & fits faster




####################################################################################################

# # PHYEMP # #
# Family: negative binomial (height & diameter), Beta (rel_repro)
# Fixed effects: dist * altitude
# Random effects: (1|trans.pair)

####################################################################################################

# # Model for HEIGHT

dat <- quad %>% #rename DF
  filter_at(vars(height_mm, dist, altitude), all_vars(!is.na(.))) %>% #remove NAs in variables used in model
  filter(species == 'phyemp')

height_nb <- brms::brm(height_mm ~ dist * altitude + (1|trans.pair), seed = 050523,
                       data = dat, family = negbinomial(link = "log", link_shape = "log"), 
                       # fitting information
                       chains = 3, iter = 5000, warmup = 1000, cores = 4, #for computers with 4 cores
                       file = 'trampling_analyses/outputs/test_int/phyemp_height_nb_int.rds', file_refit = 'on_change')
mod <- height_nb #generic model name


# CHECK MODEL

# Prior distribution
ps <- powerscale_sensitivity(mod) #look at 'diagnosis' column to see if prior isn't appropriate
unique(ps$sensitivity$diagnosis)

# Model Fit
summary(mod)
plot(mod) #model convergence (L: does distribution mean match estimate? R: did all values get explored?)
pairs(mod) #should show Gaussian blobs

# Posterior predictive check: Does data match model? (could be wrong distribution, not all effects modeled)
pp_check(mod, ndraws = 100) #posterior predictive checks - are predicted values similar to posterior distribution?

# Skewness: observed (black line) and simulated (grey distribution) SKEW metric (1000 simulated datasets)
ppc_stat(y = dat$height_mm,
         yrep = posterior_predict(mod, ndraws = 1000),
         stat = "skew")

# Leave-one-out Crossvalidation (LOO) for marginal (pointwise) posterior predictive checks
model_loo <- loo(mod, save_psis = TRUE, cores=4) #higher elpd => better fit
w <- weights(model_loo$psis_object)

# Probability integral transform (PIT) uses CDF properties to convert continuous random
# variables into uniformly distributed ones. However, this only holds exactly if the distribution
# used to generate the continuous random variables is the true distribution.
# -> If LOO-PIT values concentrated at 0/1 => possibly under-dispersed model
# -> If LOO-PIT values concentrated near 0.5 => possibly over-dispersed model
ppc_loo_pit_overlay(dat$height_mm,
                    posterior_predict(mod),
                    lw = w)


## Model for DIAMETER

dat <- quad %>% #rename DF
  filter_at(vars(mxdiam_mm, dist, altitude), all_vars(!is.na(.))) %>% #remove NAs in variables used in model
  filter(species == 'phyemp')

diam_nb <- brms::brm(mxdiam_mm ~ dist*altitude + (1|trans.pair), seed = 050523,
                     data = dat, family = negbinomial(link = "log", link_shape = "log"), 
                     # fitting information
                     chains = 3, iter = 5000, warmup = 1000, cores = 4, #for computers with 4 cores
                     file = 'trampling_analyses/outputs/test_int/phyemp_diam_nb_int.rds', file_refit = 'on_change')
mod <- diam_nb #generic model name

# CHECK MODEL
summary(mod)

ps <- powerscale_sensitivity(mod) #look at 'diagnosis' column to see if prior isn't appropriate
unique(ps$sensitivity$diagnosis)

plot(mod) #model convergence (L: does distribution mean match estimate? R: did all values get explored?)
pp_check(mod, ndraws = 100) #posterior predictive checks - are predicted values similar to posterior distribution?
pairs(mod)

ppc_stat(y = dat$mxdiam_mm,
         yrep = posterior_predict(mod, ndraws = 1000),
         stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4) #higher elpd => better fit
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$mxdiam_mm,
                    posterior_predict(mod),
                    lw = w)


## Model for REPRO

dat <- quad %>% #rename DF
  filter_at(vars(rel_repro, dist, altitude), all_vars(!is.na(.))) %>% #remove NAs in variables used in model
  filter(species == 'phyemp')

repro_beta <- brms::brm(rel_repro ~ dist* altitude + (1|trans.pair), seed = 050523,
                        data = dat, family = Beta(link = "logit", link_phi = "log"), 
                        init = '0',
                        
                        # fitting information
                        chains = 3, iter = 5000, warmup = 1000, cores = 4, #for computers with 4 cores
                        file = 'trampling_analyses/outputs/test_int/phyemp_repro_beta_int.rds', file_refit = 'on_change')
mod <- repro_beta

# CHECK MODEL

summary(mod)

# Can define priors in brm(priors = ...)
ps <- powerscale_sensitivity(mod) #look at 'diagnosis' column to see if prior isn't appropriate
unique(ps$sensitivity$diagnosis)

plot(mod) #model convergence (L: does distribution mean match estimate? R: did all values get explored?)
pp_check(mod, ndraws = 100) #posterior predictive checks - are predicted values similar to posterior distribution?
pairs(mod)

ppc_stat(y = dat$rel_repro,
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4) #higher elpd => better fit
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$rel_repro,
                    posterior_predict(mod), lw = w)




####################################################################################################

# # CASMER # #
# Family: negative binomial (height & diameter), Beta (rel_repro)
# Fixed effects: dist * altitude
# Random effects: (1|trans.pair)

####################################################################################################

# # Model for HEIGHT

dat <- quad %>% #rename DF
  filter_at(vars(height_mm, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'casmer') ###*** change here

height_nb <- brms::brm(height_mm ~ dist*altitude + (1|trans.pair), data = dat, seed = 050523,
                       family = negbinomial(link = "log", link_shape = "log"), 
                       chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                       file = 'trampling_analyses/outputs/test_int/height_nb_casmer_int.rds', ###*** change here
                       file_refit = 'on_change')
mod <- height_nb #generic model name

# Model results
summary(mod)

# Model fit
ps <- powerscale_sensitivity(mod)
unique(ps$sensitivity$diagnosis)

plot(mod)
pp_check(mod, ndraws = 100)
pairs(mod)

ppc_stat(y = dat$height_mm, ###*** change here
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4)
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$height_mm, ###*** change here
                    posterior_predict(mod), lw = w)


## Model for DIAMETER

dat <- quad %>% #rename DF
  filter_at(vars(mxdiam_mm, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'casmer') ###*** change here

diam_nb <- brms::brm(mxdiam_mm ~ dist*altitude + (1|trans.pair), data = dat, seed = 050523,
                     family = negbinomial(link = "log", link_shape = "log"), 
                     chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                     file = 'trampling_analyses/outputs/test_int/diam_nb_casmer_int.rds', ###*** change here
                     file_refit = 'on_change')
mod <- diam_nb #generic model name

# Model results
summary(mod)

# Model fit
ps <- powerscale_sensitivity(mod)
unique(ps$sensitivity$diagnosis)

plot(mod)
pp_check(mod, ndraws = 100)
pairs(mod)

ppc_stat(y = dat$mxdiam_mm, ###*** change here
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4)
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$mxdiam_mm, ###*** change here
                    posterior_predict(mod), lw = w)


## Model for REPRO

dat <- quad %>% #rename DF
  filter_at(vars(rel_repro, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'casmer') ###*** change here

repro_beta <- brms::brm(rel_repro ~ dist*altitude + (1|trans.pair), data = dat, seed = 050523,
                        family = Beta(link = "logit", link_phi = "log"), init = '0',
                        chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                        file = 'trampling_analyses/outputs/test_int/repro_beta_casmer_int.rds', ###*** change here
                        file_refit = 'on_change')
mod <- repro_beta

# Model results
summary(mod)

# Model fit
ps <- powerscale_sensitivity(mod)
unique(ps$sensitivity$diagnosis)

plot(mod)
pp_check(mod, ndraws = 100)
pairs(mod)

ppc_stat(y = dat$rel_repro, ###*** change here
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4)
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$rel_repro, ###*** change here
                    posterior_predict(mod), lw = w)



####################################################################################################

# # VACOVA # #
# Family: negative binomial (height & diameter), Beta (rel_repro)
# Fixed effects: dist * altitude
# Random effects: (1|trans.pair)

####################################################################################################

# # Model for HEIGHT

dat <- quad %>% #rename DF
  filter_at(vars(height_mm, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'vacova') ###*** change here

height_nb <- brms::brm(height_mm ~ dist*altitude + (1|trans.pair), data = dat, seed = 050523,
                       family = negbinomial(link = "log", link_shape = "log"), 
                       chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                       file = 'trampling_analyses/outputs/test_int/height_nb_vacova_int.rds', ###*** change here
                       file_refit = 'on_change')
mod <- height_nb #generic model name

# Model results
summary(mod)

# Model fit
ps <- powerscale_sensitivity(mod)
unique(ps$sensitivity$diagnosis)

plot(mod)
pp_check(mod, ndraws = 100)
pairs(mod)

ppc_stat(y = dat$height_mm,
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4)
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$height_mm,
                    posterior_predict(mod), lw = w)


## Model for DIAMETER

dat <- quad %>% #rename DF
  filter_at(vars(mxdiam_mm, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'vacova') ###*** change here

diam_nb <- brms::brm(mxdiam_mm ~ dist*altitude + (1|trans.pair), data = dat, seed = 050523,
                     family = negbinomial(link = "log", link_shape = "log"), 
                     chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                     file = 'trampling_analyses/outputs/test_int/diam_nb_vacova_int.rds', ###*** change here
                     file_refit = 'on_change')
mod <- diam_nb #generic model name

# Model results
summary(mod)

# Model fit
ps <- powerscale_sensitivity(mod)
unique(ps$sensitivity$diagnosis)

plot(mod)
pp_check(mod, ndraws = 100)
pairs(mod)

ppc_stat(y = dat$mxdiam_mm,
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4)
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$mxdiam_mm,
                    posterior_predict(mod), lw = w)


## Model for REPRO

dat <- quad %>% #rename DF
  filter_at(vars(rel_repro, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'vacova') ###*** change here

repro_beta <- brms::brm(rel_repro ~ dist*altitude + (1|trans.pair), data = dat, seed = 050523,
                        family = Beta(link = "logit", link_phi = "log"), init = '0',
                        chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                        file = 'trampling_analyses/outputs/test_int/repro_beta_vacova_int.rds', ###*** change here
                        file_refit = 'on_change')
mod <- repro_beta

# Model results
summary(mod)

# Model fit
ps <- powerscale_sensitivity(mod)
unique(ps$sensitivity$diagnosis)

plot(mod)
pp_check(mod, ndraws = 100)
pairs(mod)

ppc_stat(y = dat$rel_repro,
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4)
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$rel_repro,
                    posterior_predict(mod), lw = w)




####################################################################################################

# # CARSPP # #
# Family: negative binomial (height & diameter) - no repro model because no repro data
# Fixed effects: dist * altitude
# Random effects: (1|trans.pair)

####################################################################################################

# # Model for HEIGHT

dat <- quad %>% #rename DF
  filter_at(vars(height_mm, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'carspp') ###*** change here

height_nb <- brms::brm(height_mm ~ dist*altitude + (1|trans.pair), data = dat, seed = 050523,
                       family = negbinomial(link = "log", link_shape = "log"), 
                       chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                       file = 'trampling_analyses/outputs/test_int/height_nb_carspp_int.rds', ###*** change here
                       file_refit = 'on_change')
mod <- height_nb #generic model name

# Model results
summary(mod)

# Model fit
ps <- powerscale_sensitivity(mod)
unique(ps$sensitivity$diagnosis)

plot(mod)
pp_check(mod, ndraws = 100)
pairs(mod)

ppc_stat(y = dat$height_mm,
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4)
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$height_mm,
                    posterior_predict(mod), lw = w)


## Model for DIAMETER

dat <- quad %>% #rename DF
  filter_at(vars(mxdiam_mm, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'carspp') ###*** change here

diam_nb <- brms::brm(mxdiam_mm ~ dist*altitude + (1|trans.pair), data = dat, seed = 050523,
                     family = negbinomial(link = "log", link_shape = "log"), 
                     chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                     file = 'trampling_analyses/outputs/test_int/diam_nb_carspp_int.rds', ###*** change here
                     file_refit = 'on_change')
mod <- diam_nb #generic model name

# Model results
summary(mod)

# Model fit
ps <- powerscale_sensitivity(mod)
unique(ps$sensitivity$diagnosis)

plot(mod)
pp_check(mod, ndraws = 100)
pairs(mod)

ppc_stat(y = dat$mxdiam_mm,
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4)
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$mxdiam_mm,
                    posterior_predict(mod), lw = w)




####################################################################################################

# # PLANT PERCENT COVER # #
# Family: Beta
# Fixed effects: dist * altitude
# Random effects: (1|trans.pair)

####################################################################################################

## Model for PLANT PERCENT COVER

dat <- quad %>% #rename DF
  filter_at(vars(perc.cov, dist, altitude), all_vars(!is.na(.))) %>% 
 distinct(id, .keep_all = T)

cover_beta <- brms::brm(perc.cov ~ dist*altitude + (1|trans.pair), data = dat, seed = 050523,
                        family = Beta(link = "logit", link_phi = "log"), init = '0',
                        chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                        file = 'trampling_analyses/outputs/test_int/perc-cov_beta_int.rds', ###*** change here
                        file_refit = 'on_change')
mod <- cover_beta

# Model results
summary(mod)

# Model fit
ps <- powerscale_sensitivity(mod)
unique(ps$sensitivity$diagnosis)

plot(mod)
pp_check(mod, ndraws = 100)
pairs(mod)

ppc_stat(y = dat$perc.cov,
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4)
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$perc.cov,
                    posterior_predict(mod), lw = w)



