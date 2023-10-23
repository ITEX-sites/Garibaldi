# Aims: manuscript figures
# 1. Bayesian model results for disturbance only
# 2. Bayesian model results for disturbance + elevation
# 3. Reproductive density

# Author: Philippa Stone, Nathalie Chardon
# Date created: 17 May 2023
# Date updated: 27 June 2023 (PS)

rm(list=ls()) 

# # LIBRARIES # #
library(ggplot2)
library(tidybayes)
library(brms)
library(tidyverse)
library(gridExtra)
library(scales) #for percentage conversion on Y axis
library(ggtext) #for italics in plot titles
library(lmerTest)
library(dplyr)


# # INPUT FILES # #

# Data [quadrat dataframe]
load("trampling_analyses/compiled_data/quad.RData") ##updated with reproductive metric & plant area (repro.R)
dat <- quad

# # [rds model files for each species' trait]
phyemp_height_nb <- readRDS("trampling_analyses/outputs/test_int/phyemp_height_nb_int.rds")
height_nb_vacova <- readRDS("trampling_analyses/outputs/test_int/height_nb_vacova_int.rds")
height_nb_casmer <- readRDS("trampling_analyses/outputs/test_int/height_nb_casmer_int.rds")
height_nb_carspp <- readRDS("trampling_analyses/outputs/test_int/height_nb_carspp_int.rds")

phyemp_diam_nb <- readRDS("trampling_analyses/outputs/test_int/phyemp_diam_nb_int.rds")
diam_nb_casmer <- readRDS("trampling_analyses/outputs/test_int/diam_nb_casmer_int.rds")
diam_nb_carspp <- readRDS("trampling_analyses/outputs/test_int/diam_nb_carspp_int.rds")
diam_nb_vacova <- readRDS("trampling_analyses/outputs/test_int/diam_nb_vacova_int.rds")

repro_beta_vacova <- readRDS("trampling_analyses/outputs/test_int/repro_beta_vacova_int.rds")
repro_beta_casmer <- readRDS("trampling_analyses/outputs/test_int/repro_beta_casmer_int.rds")
phyemp_repro_beta <- readRDS("trampling_analyses/outputs/test_int/phyemp_repro_beta_int.rds")

percentCover <- readRDS("trampling_analyses/outputs/test_int/perc-cov_beta_int.rds")


# # OUTPUT FILES # #
# [PDFs for plots of models for each species' trait]




####################################################################################################

# # Fig. 1: disturbance only # # (updated with new models 26.6.2023, NC)

####################################################################################################

# # THEME # #
mytheme <-   theme_classic() +
  theme(axis.text.x = element_text(colour = 'black', size = 46, angle = 30, hjust = 1), #x axis text size
        axis.text.y = element_text(colour = 'black', size = 46), #y axis text size
        axis.title.x=element_blank(), #x axis label size
        axis.title.y = element_text(size = 50), #x axis label size
        plot.title = element_text(size = 51, #title size
                                  hjust = 0.5), #align title to center
        plot.tag = element_text(size = 50), plot.tag.position = c(0.08, 0.98), #plot tags for labelling within figure
        plot.subtitle = element_text(size = 60, hjust = 1), #subtitle is being used to show sig asterisk
        legend.title = element_text(size = 46), legend.text = element_text(size = 40), legend.key.height = unit(2, 'cm'), legend.key.width = unit(1, 'cm'), #legend text size
        plot.margin = margin(1.5,1.5,1.5,0.5, "cm"))  #increased plot margins on right so axis text seen in full

theme_set(mytheme)


# # HEIGHT # #

#phyemp
cond <- conditional_effects(phyemp_height_nb, effect = 'dist', re_formula = NULL)
est <- as.data.frame(cond[[1]])

#with random effects considered - estimates are slightly different 
# summary(phyemp_height_nb)
# condr <- conditional_effects(phyemp_height_nb, effect = 'dist', re_formula = NULL) #NULL=include all random effects
# estr <- as.data.frame(condr[[1]])

phyemp_height_plot <- ggplot(est, aes(dist, height_mm, color = dist)) +
    labs(title = "Phyllodoce empetriformis", tag = "(a)") +
    ylab("Plant height (mm)") +
    geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 2.75) +
    geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 2.75) +
    geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 2.75) +
    scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
    scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
    scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
    theme(legend.title = element_blank(), legend.position = c(0.78, 0.88), plot.title = element_text(face = "italic"), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#casmer
cond <- conditional_effects(height_nb_casmer, effect = 'dist', re_formula = NULL)
est <- as.data.frame(cond[[1]])

casmer_height_plot <- ggplot(est, aes(dist, height_mm, color = dist)) +
    labs(title = "Cassiope mertensiana", tag = "(b)") +
    ylab("Plant height (mm)") +
    xlab("Disturbance") +
    geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 2.75) +
    geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 2.75) +
    geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 2.75) +
    scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
    scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
    scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
    theme(legend.position = "none", plot.title = element_text(face = "italic"), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#vacova
cond <- conditional_effects(height_nb_vacova, effect = 'dist', re_formula = NULL)
est <- as.data.frame(cond[[1]])

vacova_height_plot <- ggplot(est, aes(dist, height_mm, color = dist)) +
    labs(title = "Vaccinium ovalifolium", tag = "(c)", subtitle = "*") +
    ylab("Plant height (mm)") +
    xlab("Disturbance") +
    geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 2.75) +
    geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 2.75) +
    geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 2.75) +
    scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
    scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
    scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
    theme(legend.position = "none", plot.title = element_text(face = "italic"), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#carspp
cond <- conditional_effects(height_nb_carspp, effect = 'dist', re_formula = NULL)
est <- as.data.frame(cond[[1]])

carspp_height_plot <- ggplot(est, aes(dist, height_mm, color = dist)) +
  ggtitle(expression(paste(italic("Carex "), "spp."))) +
  labs(tag = "(d)", subtitle = "*") +
  ylab("Plant height (mm)") +
  xlab("Disturbance") +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 2.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 2.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 2.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank())


# # DIAMETER # #

#phyemp
cond <- conditional_effects(phyemp_diam_nb, effect = 'dist', re_formula = NULL)
est <- as.data.frame(cond[[1]])

phyemp_diam_plot <- ggplot(est, aes(dist, mxdiam_mm, color = dist)) +
  labs(title = "Phyllodoce empetriformis", tag = "(e)", subtitle = "*") +
  ylab("Plant diameter (mm)") +
  xlab("Disturbance") +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 2.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 2.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 2.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#casmer
cond <- conditional_effects(diam_nb_casmer, effect = 'dist', re_formula = NULL)
est <- as.data.frame(cond[[1]])

casmer_diam_plot <- ggplot(est, aes(dist, mxdiam_mm, color = dist)) +
  labs(title = "Cassiope mertensiana", tag = "(f)", subtitle = " ") +
  ylab("Plant diameter (mm)") +
  xlab("Disturbance") +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 2.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 2.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 2.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#vacova
cond <- conditional_effects(diam_nb_vacova, effect = 'dist', re_formula = NULL)
est <- as.data.frame(cond[[1]])

vacova_diam_plot <- ggplot(est, aes(dist, mxdiam_mm, color = dist)) +
  labs(title = "Vaccinium ovalifolium", tag = "(g)", subtitle = "*") +
  ylab("Plant diameter (mm)") +
  xlab("Disturbance") +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 2.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 2.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 2.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#carspp
cond <- conditional_effects(diam_nb_carspp, effect = 'dist', re_formula = NULL)
est <- as.data.frame(cond[[1]])

carspp_diam_plot <- ggplot(est, aes(dist, mxdiam_mm, color = dist)) +
  ggtitle(expression(paste(italic("Carex "), "spp."))) +
  labs(tag = "(h)", subtitle = " ") +
  ylab("Plant diameter (mm)") +
  xlab("Disturbance") +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 2.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 2.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 2.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank())


# # REPRODUCTIVE OUTPUT # #

#phyemp
cond <- conditional_effects(phyemp_repro_beta, effect = 'dist', re_formula = NULL)
est <- as.data.frame(cond[[1]])

phyemp_repro_plot <- ggplot(est, aes(dist, rel_repro, color = dist)) +
  labs(title = "Phyllodoce empetriformis", tag = "(i)", subtitle = " ") +
  labs(x = expression(paste("Disturbance")),
       y = expression(paste("Relative reproductive output"))) +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 2.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 2.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 2.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#casmer
cond <- conditional_effects(repro_beta_casmer, effect = 'dist', re_formula = NULL)
est <- as.data.frame(cond[[1]])

casmer_repro_plot <- ggplot(est, aes(dist, rel_repro, color = dist)) +
  labs(title = "Cassiope mertensiana", tag = "(j)", subtitle = "*") +
  labs(x = expression(paste("Disturbance")),
       y = expression(paste("Relative reproductive output"))) +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 2.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 2.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 2.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#vacova
cond <- conditional_effects(repro_beta_vacova, effect = 'dist', re_formula = NULL)
est <- as.data.frame(cond[[1]])

vacova_repro_plot <- ggplot(est, aes(dist, rel_repro, color = dist)) +
  labs(title = "Vaccinium ovalifolium", tag = "(k)", subtitle = " ") +
  labs(x = expression(paste("Disturbance")),
       y = expression(paste("Relative reproductive output"))) +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 2.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 2.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 2.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"), axis.text.x=element_blank(), axis.ticks.x=element_blank())


# # PERCENT COVER # #

cond <- conditional_effects(percentCover, effect = 'dist', re_formula = NULL)
est <- as.data.frame(cond[[1]])

perccover_plot <- ggplot(est, aes(dist, perc.cov, color = dist)) +
  labs(title = "All species", tag = "(l)", subtitle = "*") +
  ylab("Percent cover") +
  xlab("Disturbance") +
  scale_y_continuous(labels = percent_format()) +
  geom_linerange(aes(ymin = lower__, ymax = upper__), linewidth = 2.75) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.5, linewidth = 2.75) +
  geom_pointrange(aes(y = estimate__, ymin = lower__, ymax = upper__), size = 2.75) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_x_discrete(labels=c("0" = "Undisturbed", "1" = "Disturbed")) +
  theme(legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank())



# # PANEL FIGURES # #

allTraitsPanelPlot <- grid.arrange(phyemp_height_plot, casmer_height_plot, vacova_height_plot, carspp_height_plot, phyemp_diam_plot, casmer_diam_plot, vacova_diam_plot, carspp_diam_plot, phyemp_repro_plot, casmer_repro_plot, vacova_repro_plot, perccover_plot, nrow=3)
ggsave(allTraitsPanelPlot, file = 'trampling_analyses/outputs/ms_figs/allTraitsPanelPlot_int.jpeg', width = 40, height = 36)




####################################################################################################

# # Fig. S1: disturbance * elevation # # (updated with new models 27.6.2023, PS)

####################################################################################################

load("trampling_analyses/compiled_data/quad.RData") 
dat <- quad


# # THEME # #
mytheme <-   theme_classic() +
  theme(axis.text.x = element_text(colour = 'black', size = 46, angle = 30, hjust = 1), #x axis text size
        axis.text.y = element_text(colour = 'black', size = 46), #y axis text size
        axis.title.x= element_text(size = 50), #x axis label size
        axis.title.y = element_text(size = 50), #x axis label size
        plot.title = element_text(size = 51, #title size
                                  hjust = 0.5), #align title to center
        plot.tag = element_text(size = 50), plot.tag.position = c(0.08, 0.98), #plot tags for labelling within figure
        plot.subtitle = element_text(size = 60, hjust = 1), #subtitle is being used to show sig asterisk
        legend.title = element_text(size = 46), legend.text = element_text(size = 35), #legend text size
        plot.margin = margin(1,1.5,1,0.5, "cm"))  #increased plot margins on right so axis text seen in full

theme_set(mytheme)

# # HEIGHT # #
dat <- dat %>% mutate(estimate__ = height_mm) #need to make column of plant height called estimate__ so it can be plotted with line and ribbon plot of height estimates from model outputs


#phyemp
cond <- conditional_effects(phyemp_height_nb)[[3]]
est <- as.data.frame(cond)

phyemp_height_elev_plot <- ggplot(est, aes(altitude, estimate__, color = dist)) +
  labs(title = "Phyllodoce empetriformis", tag = "(a)") +
  geom_point(data = dat %>% filter(species == "phyemp"), size = 2.75) +
  ylab("Plant height (mm)") +
  xlab("Elevation (m)") +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = dist), alpha = 0.4) +
  geom_line(size = 1.5) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  theme(legend.title = element_blank(), legend.position = c(0.78, 0.78), plot.title = element_text(face = "italic"))

# ggsave(phyemp_height_elev_plot, file = 'trampling_analyses/outputs/ms_figs/PhyempHeightModPlotEdits.pdf', width = 10, height = 8)

#casmer
cond <- conditional_effects(height_nb_casmer)[[3]]
est <- as.data.frame(cond)

casmer_height_elev_plot <- ggplot(est, aes(altitude, estimate__, color = dist)) +
  labs(title = "Cassiope mertensiana", tag = "(b)") +
  geom_point(data = dat %>% filter(species == "casmer"), size = 2.75) +
  ylab("Plant height (mm)") +
  xlab("Elevation (m)") +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = dist), alpha = 0.4) +
  geom_line(size = 1.5) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))


#vacova
cond <- conditional_effects(height_nb_vacova)[[3]]
est <- as.data.frame(cond)

vacova_height_elev_plot <- ggplot(est, aes(altitude, estimate__, color = dist)) +
  labs(title = "Vaccinium ovalifolium", tag = "(c)") +
  geom_point(data = dat %>% filter(species == "vacova"), size = 2.75) +
  ylab("Plant height (mm)") +
  xlab("Elevation (m)") +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = dist), alpha = 0.4) +
  geom_line(size = 1.5) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))


#carspp
cond <- conditional_effects(height_nb_carspp)[[3]]
est <- as.data.frame(cond)

carspp_height_elev_plot <- ggplot(est, aes(altitude, estimate__, color = dist)) +
  labs(tag = "(d)") +
  ggtitle(expression(paste(italic("Carex "), "spp."))) +
  geom_point(data = dat %>% filter(species == "carspp"), size = 2.75) +
  ylab("Plant height (mm)") +
  xlab("Elevation (m)") +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = dist), alpha = 0.4) +
  geom_line(size = 1.5) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  theme(legend.position = "none")




# # Diameter # #
dat = subset(dat, select = -estimate__)
dat <- dat %>% mutate(estimate__ = mxdiam_mm) #need to make column of plant diamter called estimate__ so it can be plotted with line and ribbon plot of diameter estimates from model outputs

#phyemp
cond <- conditional_effects(phyemp_diam_nb)[[3]]
est <- as.data.frame(cond)

phyemp_diam_elev_plot <- ggplot(est, aes(altitude, estimate__, color = dist)) +
  labs(title = "Phyllodoce empetriformis", tag = "(e)") +
  geom_point(data = dat %>% filter(species == "phyemp"), size = 2.75) +
  ylab("Plant diameter (mm)") +
  xlab("Elevation (m)") +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = dist), alpha = 0.4) +
  geom_line(size = 1.5) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))

#ggsave(phyemp_diam_elev_plot, file = 'trampling_analyses/outputs/ms_figs/PhyempDiamModPlot.pdf', width = 10, height = 8)

#casmer
cond <- conditional_effects(diam_nb_casmer)[[3]]
est <- as.data.frame(cond)

casmer_diam_elev_plot <- ggplot(est, aes(altitude, estimate__, color = dist)) +
  labs(title = "Cassiope mertensiana", tag = "(f)") +
  geom_point(data = dat %>% filter(species == "casmer"), size = 2.75) +
  ylab("Plant diameter (mm)") +
  xlab("Elevation (m)") +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = dist), alpha = 0.4) +
  geom_line(size = 1.5) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))


#vacova
cond <- conditional_effects(diam_nb_vacova)[[3]]
est <- as.data.frame(cond)

vacova_diam_elev_plot <- ggplot(est, aes(altitude, estimate__, color = dist)) +
  labs(title = "Vaccinium ovalifolium", tag = "(g)") +
  geom_point(data = dat %>% filter(species == "vacova"), size = 2.75) +
  ylab("Plant diameter (mm)") +
  xlab("Elevation (m)") +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = dist), alpha = 0.4) +
  geom_line(size = 1.5) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))


#carspp
cond <- conditional_effects(diam_nb_carspp)[[3]]
est <- as.data.frame(cond)

carspp_diam_elev_plot <- ggplot(est, aes(altitude, estimate__, color = dist)) +
  labs(tag = "(h)") +
  ggtitle(expression(paste(italic("Carex "), "spp."))) +
  geom_point(data = dat %>% filter(species == "carspp"), size = 2.75) +
  ylab("Plant diameter (mm)") +
  xlab("Elevation (m)") +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = dist), alpha = 0.4) +
  geom_line(size = 1.5) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  theme(legend.position = "none")



# # Reproductive output # #
dat = subset(dat, select = -estimate__)
dat <- dat %>% mutate(estimate__ = rel_repro) #need to make column of plant diamter called estimate__ so it can be plotted with line and ribbon plot of diameter estimates from model outputs

#phyemp
cond <- conditional_effects(phyemp_repro_beta)[[3]]
est <- as.data.frame(cond)

phyemp_repro_elev_plot <- ggplot(est, aes(altitude, estimate__, color = dist)) +
  labs(title = "Phyllodoce empetriformis", tag = "(i)") +
  geom_point(data = dat %>% filter(species == "phyemp"), size = 2.75) +
  labs(x = expression(paste("Elevation (m)")),
       y = expression(paste("Relative reproductive output"))) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = dist), alpha = 0.4) +
  geom_line(size = 1.5) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))

#casmer
cond <- conditional_effects(repro_beta_casmer)[[3]]
est <- as.data.frame(cond)

casmer_repro_elev_plot <- ggplot(est, aes(altitude, estimate__, color = dist)) +
  labs(title = "Cassiope mertensiana", tag = "(j)") +
  geom_point(data = dat %>% filter(species == "casmer"), size = 2.75) +
  labs(x = expression(paste("Elevation (m)")),
       y = expression(paste("Relative reproductive output"))) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = dist), alpha = 0.4) +
  geom_line(size = 1.5) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))


#vacova
cond <- conditional_effects(repro_beta_vacova)[[3]]
est <- as.data.frame(cond)

vacova_repro_elev_plot <- ggplot(est, aes(altitude, estimate__, color = dist)) +
  labs(title = "Vaccinium ovalifolium", tag = "(k)") +
  geom_point(data = dat %>% filter(species == "vacova"), size = 2.75) +
  labs(x = expression(paste("Elevation (m)")),
       y = expression(paste("Relative reproductive output"))) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = dist), alpha = 0.4) +
  geom_line(size = 1.5) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"))



# #  PERCENT COVER  # #
dat = subset(dat, select = -estimate__)
dat <- dat %>% mutate(estimate__ = perc.cov) #need to make column of plant diamter called estimate__ so it can be plotted with line and ribbon plot of diameter estimates from model outputs

cond <- conditional_effects(percentCover)[[3]]
est <- as.data.frame(cond)

percCov_elev_plot <- ggplot(est, aes(altitude, estimate__, color = dist)) +
  labs(title = "All species", tag = "(l)") +
  geom_point(data = dat, size = 2.75) +
  ylab("Percent cover") +
  xlab("Elevation (m)") +
  labs(tag = "(l)") +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = dist), alpha = 0.4) +
  geom_line(size = 1.5) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  scale_fill_manual("Dist", breaks = c(0,1), values=c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed")) +
  theme(legend.position = "none")



# # PANEL FIGURES # #

heightAltitudePanelPlot <- grid.arrange(phyemp_height_elev_plot, casmer_height_elev_plot, vacova_height_elev_plot, carspp_height_elev_plot, nrow=1)
ggsave(heightAltitudePanelPlot, file = 'trampling_analyses/outputs/ms_figs/heightAltitudePanelPlot.pdf', width = 40, height = 14)

diamAltitudePanelPlot <- grid.arrange(phyemp_diam_elev_plot, casmer_diam_elev_plot, vacova_diam_elev_plot, carspp_diam_elev_plot, nrow=1)
ggsave(diamAltitudePanelPlot, file = 'trampling_analyses/outputs/ms_figs/diamAltitudePanelPlot.pdf', width = 40, height = 14)

reproPerccoverAltitudePanelPlot <- grid.arrange(phyemp_repro_elev_plot, casmer_repro_elev_plot, vacova_repro_elev_plot, percCov_elev_plot, nrow =1)
ggsave(reproPerccoverAltitudePanelPlot, file = 'trampling_analyses/outputs/ms_figs/reproPerccoverAltitudePanelPlot.pdf', width = 40, height = 14)

allTraitsAltitudePanelPlot <- grid.arrange(phyemp_height_elev_plot, casmer_height_elev_plot, vacova_height_elev_plot, carspp_height_elev_plot, phyemp_diam_elev_plot, casmer_diam_elev_plot, vacova_diam_elev_plot, carspp_diam_elev_plot, phyemp_repro_elev_plot, casmer_repro_elev_plot, vacova_repro_elev_plot, percCov_elev_plot, nrow=3)
ggsave(allTraitsAltitudePanelPlot, file = 'trampling_analyses/outputs/ms_figs/allTraitsAltitudePanelPlot.jpeg', width = 40, height = 42)






####################################################################################################

# # Fig. S2: reproductive structure density # # 

####################################################################################################

# # PLOT THEME # #
mytheme <- theme_classic() +
  theme(axis.text.x = element_text(colour = 'black', size = 25), #x axis text size
        axis.text.y = element_text(colour = 'black', size = 25), #y axis text size
        axis.title.x = element_text(size = 28), #x axis label size
        axis.title.y = element_text(size = 28), #x axis label size
        plot.title = element_text(size = 32, #title size
                                  hjust = 0.5), #align title to center
        plot.tag = element_text(size = 32), plot.tag.position = c(0.08, 0.99), #plot tags for labelling within figure
        legend.title = element_text(size = 24), legend.text = element_text(size = 22),
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        panel.background = element_blank(), #remove panel background
        axis.line = element_line(colour = "black"), #set axis line color
        legend.key.size = unit(2, "line"), #increase size of legend key
        legend.spacing.y = unit(0.5, "cm"), #increase vertical spacing between legend items
        strip.background = element_rect(fill = "white"), #set background color for facet labels
        strip.text.x = element_text(face = "italic", size = 20), #set size for x-axis facet labels
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) 

theme_set(mytheme)

#load data file
load("trampling_analyses/compiled_data/quad.RData") ##updated with reproductive metric & plant area (repro.R)

# Remove Carex spp.
dat <- quad %>% 
  filter(species != "carspp")

#remove Phyllodoce outlier causing problems with visualisation for plot
dat <- filter(dat, repro <20)

# full species names
species_names <- c('carspp'= "Carex spp.", 'casmer' = "Cassiope mertensiana", 'phyemp' = "Phyllodoce empetriformis", 'phygla' = "Phyllodoce glanduliflora", 'vacova' = "Vaccinium ovalifolium")

# # ALL SPECIES PLOTTED SEPARATELY

# Plot size vs number of buds, flowers, and fruits/(height * diameter) by disturbed/undisturbed

(phyempPlotAreaBySpecies <- ggplot(dat %>% filter(species == "phyemp"), aes(x= plantArea_cm2, y= repro, color = dist)) +
    geom_point(data = dat %>% filter(species == "phyemp")) +
    labs(title = "Phyllodoce empetriformis") +
    stat_smooth(aes(fill = dist), method = "lm", formula = y ~ x, geom = "ribbon", alpha = 0.3, size = 0) +
    stat_smooth(aes(color = dist), method = "lm", formula = y ~ x, geom = "line", size=1) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    coord_trans(y = "log1p") + #log10 y axis 
    labs(x = expression(paste("Plant size (", cm^2, ")")),
         y = expression(paste("Density of reproductive structures (counts/", cm^2, ")")),
         tag = "(a)") +
    theme(legend.position = "none", plot.title = element_text(face = "italic")))

(casmerPlotAreaBySpecies <- ggplot(dat %>% filter(species == "casmer"), aes(x= plantArea_cm2, y= repro, color = dist)) +
    geom_point(data = dat %>% filter(species == "casmer")) +
    labs(title = "Cassiope mertensiana") +
    stat_smooth(aes(fill = dist), method = "lm", formula = y ~ x, geom = "ribbon", alpha = 0.3, size = 0) +
    stat_smooth(aes(color = dist), method = "lm", formula = y ~ x, geom = "line", size=1) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    coord_trans(y = "log1p") + #log10 y axis 
    labs(x = expression(paste("Plant size (", cm^2, ")")),
         y = expression(paste("Density of reproductive structures (counts/", cm^2, ")")),
        tag = "(b)") +
    theme(legend.position = c(0.75, 0.75), plot.title = element_text(face = "italic")))

(vacovaPlotAreaBySpecies <- ggplot(dat %>% filter(species == "vacova"), aes(x= plantArea_cm2, y= repro, color = dist)) +
    geom_point(data = dat %>% filter(species == "vacova")) +
    labs(title = "Vaccinium ovalifolium") +
    stat_smooth(aes(fill = dist), method = "lm", formula = y ~ x, geom = "ribbon", alpha = 0.3, size = 0) +
    stat_smooth(aes(color = dist), method = "lm", formula = y ~ x, geom = "line", size=1) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    coord_trans(y = "log1p") + #log10 y axis 
    labs(x = expression(paste("Plant size (", cm^2, ")")),
         y = expression(paste("Density of reproductive structures (counts/", cm^2, ")")),
         tag = "(c)") +
    theme(legend.position = "none", plot.title = element_text(face = "italic")))

allSpeciesReproPlot <- grid.arrange(phyempPlotAreaBySpecies, casmerPlotAreaBySpecies, vacovaPlotAreaBySpecies, nrow=1)

ggsave(allSpeciesReproPlot, file = 'trampling_analyses/outputs/ms_figs/allSpeciesReproPlot.jpeg', width = 20, height = 10)


####################################################################################################

# # TEST FOR RELATIONSHIP BETWEEN PLANT AREA AND REPRO STRUCTURES BY SPECIES (Nathalie) # # 
# package: lmerTest
# Fixed effects: plantArea_cm2 + dist
# Random effects: (1|trans.pair)

####################################################################################################

# # Data
load("trampling_analyses/compiled_data/quad.RData") ##updated with reproductive metric & plant area (repro.R)

totalReproStruct <- quad$flws + quad$frts + quad$buds #total repro structures
quad <- cbind(quad, totalReproStruct)
totalReproStructByArea <- quad$totalReproStruct / quad$plantArea_cm2
quad <- cbind(quad, totalReproStructByArea)

str(quad) #check that categorical explanatory variables are factors, others numeric

## PHYEMP ##

# Filter by species 
dat <- quad %>%
  filter(species == 'phyemp')

hist(dat$totalReproStructByArea, breaks = 100)

# # Fit Model 
mod <- lmer(totalReproStructByArea ~ plantArea_cm2 * dist + (1|trans.pair), data = dat)

summary(mod)


## CASMER ##

# Filter by species 
dat <- quad %>%
  filter(species == 'casmer')

hist(dat$totalReproStructByArea, breaks = 100)

# # Fit Model 
mod <- lmer(totalReproStructByArea ~ plantArea_cm2 * dist + (1|trans.pair), data = dat)

summary(mod)


## VACOVA ##

# Filter by species 
dat <- quad %>%
  filter(species == 'vacova')

hist(dat$totalReproStructByArea, breaks = 100)

# # Fit Model 
mod <- lmer(totalReproStructByArea ~ plantArea_cm2 * dist + (1|trans.pair), data = dat)

summary(mod)

