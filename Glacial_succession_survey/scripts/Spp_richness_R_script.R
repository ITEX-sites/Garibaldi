# point frame R script

install.packages("plyr")
install.packages("readr")
install.packages("tidyverse")
install.packages("stringr")
install.packages("tidyr")
install.packages("chron")
install.packages("timeDate")
install.packages("dplyr")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("scales")
install.packages("lubridate")
install.packages("vegan")
install.packages("BiodiversityR")
install.packages("car")
install.packages("ggplot2")
install.packages("tcltk")
install.packages("permute")
install.packages("lattice")
install.packages("ggthemes")

#--------------------------
library(plyr)
library(readr)
library(tidyverse)
library(stringr)
library(tidyr)
library(chron)
library(timeDate)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(lubridate)
library(BiodiversityR)
library(vegan)
library(car)
library(ggplot2)
library(ggthemes)

# first change path to where you want the figures output to
#setwd("~/GitHub/Garibaldi/Glacial_succession_survey")

# read in spp and site matrices
species.data0 <- read.csv(file = "./data/Garibaldi_plant_data_Genus.csv")
site.data <- read.csv(file = "./data/waypoints_with_model_val.csv")

#---------------------------
# edit the site data
Bay_date <- str_split_fixed(site.data$Date,  "_", 2)

site.data$GlacialDate <- Bay_date[,2]
site.data$Bay <- Bay_date[,1]

#----------------------
# Edit the species data
# remove transect column
Genus_spp <- species.data0$Genus_spp
Genus <- species.data0$Genus
species.data <- species.data0[,-c(1:2,43)]

species.data <- t(species.data)

#####################################
#
# Diversity indices
#
#####################################

#create dataframe of site and species data
df<- cbind(site.data, species.data)

# calculate species diversity
diversity(species.data, index = "shannon")#this is the Shannon-Wiener index
diversity(species.data, index = "simpson")#this is the Simpson index
#fisher.alpha(species.data) #this is Fisher's alpha from the log-series distribution, fairly independent of sample size

site.data$shannon<-(diversity(species.data, index = "shannon"))#makes a new column in site data with the shannon values
site.data$simpson<-(diversity(species.data, index = "simpson"))
#site.data$fisher<-fisher.alpha(species.data)

# effects of Bay on shannon diversity
model1<-lm(shannon~Bay, data =site.data)
#summary(lm(shannon~Bay, data =site.data))
anova(model1)

# effects of Date on shannon diversity
model2<-lm(shannon~GlacialDate, data =site.data)
#summary(lm(shannon~GlacialDate, data =site.data))
anova(model2)

# effects of Date on simpson diversity
model5<-lm(shannon~Date, data =site.data)
#summary(lm(simpson~Date, data =site.data))
anova(model5)

#------
# effects of Bay on simpson diversity
model1<-lm(simpson~Bay, data =site.data)
#summary(lm(simpson~Bay, data =site.data))
anova(model1)

# effects of Date on simpson diversity
model2<-lm(simpson~GlacialDate, data =site.data)
#summary(lm(simpson~GlacialDate, data =site.data))
anova(model2)

# effects of Date on simpson diversity
model5<-lm(simpson~Date, data =site.data)
#summary(lm(simpson~Date, data =site.data))
anova(model5)

#---------------------------------------------
# create graph of species diversity separated by X1 and coloured by X2
#plot(site.data$shannon ~ site.data$GlacialDate,  main= "Shannon Diversity Index changes with GlacialDate", xlab="GlacialDate", ylab="Shannon Diversity", pch=20)
#abline(model1) #adds the trend line

# ggplot(data=site.data, aes(x=Bay, y=shannon, colour=GlacialDate)) + geom_point(size=3)+
#   stat_smooth(method = "lm")#add the line

# example of how to save a png image in R
png("./figures/Shannon_bay.png", width = 856, height = 540)
ggplot(data=site.data, aes(x=Bay, y=shannon, colour=GlacialDate)) + geom_point(size=3)+
  stat_smooth(method = "lm")#add the line
dev.off()

png("./figures/Shannon_Date.png", width = 856, height = 540)
ggplot(data=site.data, aes(x=GlacialDate, y=shannon, colour=Bay)) + geom_point(size=3)+
  stat_smooth(method = "lm")#add the line
dev.off()


#####################################
#
# Evenness index {vegan}
#
#####################################

PielouJ <- diversity(species.data, index = "shannon")/log(specnumber(species.data)) # Pielou's J
site.data <- cbind(site.data, PielouJ)

model5<-lm(PielouJ~Bay, data =site.data)
anova(model5)
model5<-lm(PielouJ~GlacialDate, data =site.data)
anova(model5)

png("./figures/Shannon_Bay_boxplot.png", width = 856, height = 540)
boxplot(shannon~Bay, data=site.data, col="light blue", xlab="Bay", ylab="Shannon Diversity Index", main="Shannon Diversity")
dev.off()

png("./figures/Shannon_GlacialDate_boxplot.png", width = 856, height = 540)
boxplot(shannon~GlacialDate, data=site.data, las=2, col="light blue", xlab="GlacialDate", ylab="Shannon Diversity Index", main="Shannon Diversity")
dev.off()

png("./figures/Shannon_GlacialDate_Bay_boxplot.png", width = 856, height = 540)
boxplot(shannon~Date, data=site.data, las=2, col="light blue", xlab="GlacialDate_Bay", ylab="Shannon Diversity Index", main="Shannon Diversity")
dev.off()

png("./figures/Pielou_GlacialDate_boxplot.png", width = 856, height = 540)
boxplot(PielouJ~GlacialDate, data=site.data, col="light blue", xlab="GlacialDate", ylab="Pielou's J Evenness", main="Pielou's J evenness")
dev.off()

png("./figures/Pielou_Bay_boxplot.png", width = 856, height = 540)
boxplot(PielouJ~Bay, data=site.data, col="light blue", xlab="Bay", ylab="Pielou's J Evenness", main="Pielou's J evenness")
dev.off()

#####################################
#
# Rarefaction {vegan}
#
#####################################

#min(rowSums(species.data))
#rarefy(species.data, 15) #if only 15 individuals had been sampled in each treatment, what would the diversity have been?

#####################################
#
# Species - Frequency plots, traditional for log series {vegan}
#
#####################################

#dev.off()
#par(mfrow=c(1,1))
#plot(fisherfit(colSums(species.data))) #here is a plot of the number of species for each "bin" of abundances, abundances are summed over all sites

#####################################
#
# Rank- abundance plots, with log normal and other fits overlain {vegan}
#
#####################################
plot(rad.lognormal(colSums(species.data))) #sum abundances across all sites with colSums, fit log normal, then plot

radlattice(radfit(colSums(species.data))) #other functions for rank-abundance, the lowest AIC is the best if >2 units lower than any other

#####################################
#
# Rank- abundance plots, the raw values linked with a line
# Requires BiodiversityR package
#
#####################################
colnames(species.data)<- Genus
RankAbun.1 <- rankabundance(species.data)
RankAbun.1 # a dataframe of the rank of each species

png("./figures/Rank_abundance_total.png", width = 856, height = 540)
rankabunplot(RankAbun.1,scale='abundance', addit=FALSE, specnames=c(1:31), srt = 45, xlim = c(1,32), ylim = c(0,200)) #rank abundance plot, labelling the most common 3 species
dev.off()

site.data$GlacialDate <- as.factor(site.data$GlacialDate)
site.data$Bay <- as.factor(site.data$Bay)
site.data$Date <- as.factor(site.data$Date)

#rankabuncomp(species.data, y=site.data, factor=c('GlacialDate'),scale='proportion', legend=TRUE) #click on where on plot you want to have the legend

# png("./figures/Rank_abundance_treatment.png", width = 856, height = 540)
# rankabuncomp(species.data, y=site.data, factor=c('GlacialDate'),scale='proportion', legend=FALSE) #click on where on plot you want to have the legend
# dev.off()
#
# rankabuncomp(species.data, y=site.data, factor=c('Bay'),scale='proportion', legend=TRUE, specnames=c(1:3)) #click on where on plot you want to have the legend
# rankabuncomp(species.data, y=site.data, factor=c('Date'),scale='proportion', legend=TRUE) #click on where on plot you want to have the legend
#
# #------------------------------------
# # what species differ between bays
# RankAbun.Sphinx <- rankabundance(species.data[which(site.data$Bay=="Sphinx"),])
# RankAbun.Sphinx # a dataframe of the rank of each species
#
# png("./figures/Rank_abundance_Sphinx.png", width = 856, height = 540)
# rankabunplot(RankAbun.Sphinx,addit=FALSE, specnames=c(1:31), srt = 45, xlim = c(1,32), ylim = c(0,100)) #rank abudnance plot, labelling the most common 3 species
# dev.off()
#
# RankAbun.Sentinel <- rankabundance(species.data[which(site.data$Bay=="Sentinel"),])
# RankAbun.Sentinel # a dataframe of the rank of each species
#
# png("./figures/Rank_abundance_Sentinel.png", width = 856, height = 540)
# rankabunplot(RankAbun.Sentinel,scale='abundance', addit=FALSE, specnames=c(1:31), srt = 45, xlim = c(1,32), ylim = c(0,100)) #rank abudnance plot, labelling the most common 3 species
# dev.off()
#
# #------------------------------------
# # What species change with time
# unique(site.data$GlacialDate)
# summary(site.data$GlacialDate)
#
# RankAbun.1720 <- rankabundance(species.data[which(site.data$GlacialDate=="1720"),])
# RankAbun.1720 # a dataframe of the rank of each species
#
# png("./figures/Rank_abundance_1720.png", width = 856, height = 540)
# rankabunplot(RankAbun.1720,addit=FALSE, specnames=c(1:31), srt = 45, xlim = c(1,32), ylim = c(0,100)) #rank abudnance plot, labelling the most common 3 species
# dev.off()
#
# RankAbun.younger <- rankabundance(species.data[which(site.data$GlacialDate=="1977"|site.data$GlacialDate=="1949"|site.data$GlacialDate=="1928"),])
# RankAbun.younger # a dataframe of the rank of each species
#
# png("./figures/Rank_abundance_younger.png", width = 856, height = 540)
# rankabunplot(RankAbun.younger,scale='abundance', addit=FALSE, specnames=c(1:31), srt = 45, xlim = c(1,32), ylim = c(0,100)) #rank abudnance plot, labelling the most common 3 species
# dev.off()
#
#
# ########################
# # Look at which species had the largest change in abundance between groups
# sppyounger <- as.data.frame(cbind(rownames(RankAbun.younger), RankAbun.younger[,8]))
# spp1720 <- as.data.frame(cbind(rownames(RankAbun.1720), RankAbun.1720[,8]))
#
# pre_post_1720 <- left_join(spp1720, sppyounger, by="V1")
# colnames(pre_post_1720) <- c("Spp", "Older", "Younger")
#
# pre_post_1720$Diff <-  as.numeric(pre_post_1720$Older) - as.numeric(pre_post_1720$Younger)
#
# pre_post_1720$Rank <- rank(pre_post_1720$Diff, na.last = TRUE,ties.method = c("average"))
#
# png("./figures/Rank_abundance_pre_post_1720_diff.png", width = 3000, height = 2000)
# plot(pre_post_1720$Rank, pre_post_1720$Diff)
# text(pre_post_1720$Rank, pre_post_1720$Diff, pre_post_1720$Spp)
# dev.off()
# #####################################
#
# Similarity matrices (vegan package required)
#
#####################################

dissim.mat<-vegdist(species.data, method="bray", binary=FALSE, na.rm = TRUE)
#dissim.mat

#?vegdist #link to other dissimilarity metrics you could use in place of "bray"
#these include: "manhattan", "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao" or "mahalanobis".

dissim.mat<-vegdist(species.data, method="jaccard", binary=TRUE, na.rm = TRUE)
#dissim.mat

#####################################

# dissimilarity matrix based on CONTINUOUS site variables - none in point framing dataset
#site.mat<-vegdist(site.data$Temperature, method="euclidean") #need to add in column for actual temperature/soil moisture which is continuous
#site.mat

# do a Mantel test, which asks if the dissimilarity between pairs of sites in their species composition is related to that in their environment
#since any given site occurs in many pairs of sites, significance in Mantel tests is assessed via randomization (automatic)
#mantel(dissim.mat, site.mat, method="pearson", permutations=9999)

#####################################
#
# Cluster analysis (no special package required)
#
#####################################

#cluster dendrogram showing how each of the 24 species are clustered
fit <- hclust(dissim.mat, method="average")

png("./figures/Dendrogram_Genus.png", width = 856, height = 540)
plot(fit)
dev.off()

png("./figures/Dendrogram_clustered_Genus_0.7.png", width = 856, height = 540)
plot(fit); rect.hclust(fit, h=0.7, border="blue") # emphasize clusters <0.5 different
dev.off()


#####################################
#
# Ordination: nMDS (requires vegan package)
#
##############################

# make NAs zero
species.data[is.na(species.data)] <- 0

# remove all rows and cols with sum of 0
#summed_rows <- apply(species.data, 1, sum)
#species.data0 <- species.data[-which(summed_rows==0),]
summed_cols <- apply(species.data, 2, sum)
species.data1 <- species.data[,-which(summed_cols==0)]

site.data1<- site.data
#-----------------------------------

site.data1 <- site.data
species.data1 <- species.data
myNMDS<-metaMDS(species.data1,k=10)
myNMDS #most important: is the stress low?
stressplot(myNMDS) #low stress means that the observed dissimilarity between site pairs matches that on the 2-D plot fairly well (points hug the line)

plot(myNMDS)#sites are open circles and species are red +'s

png("./figures/NMDS_spp_sites.png", width = 2000, height = 2000)
#the following commands create layers on a plot, and should be run sequentially
ordiplot(myNMDS,type="n") #this clears the symbols from the plot
orditorp(myNMDS,display="species",col="red",air=0.01) #this adds red species names
orditorp(myNMDS,display="sites",cex=0.75,air=0.01) #this adds black site labels, cex is the font size
#ordispider(myNMDS,groups=site.data1$Ecotone,spiders="centroid",col="black",label=F)
dev.off()

png("./figures/NMDS_Pixel_model.png", width = 856, height = 540)
# connect sites in the same treatment with a polygon use "ordihull"
ordiplot(myNMDS,type="n")
ordihull(myNMDS,groups=site.data1$Model_clas, draw="polygon",col="grey90",label=F)
#orditorp(myNMDS,display="species",col="red",air=0.01)
#orditorp(myNMDS,display="sites",cex=0.75,air=0.01)
dev.off()

# TO DO: need to add labels
# but for this we need all the WP with single categories removed

# try grouping single categories
# glacier with scree
# till_water with water

# remove equisetum and alder



png("./figures/NMDS_Pixel_model.png", width = 856, height = 540)
# connect sites in the same treatment with a polygon use "ordihull"
ordiplot(myNMDS,type="n")
ordihull(myNMDS,groups=site.data1$Model_clas, draw="polygon",col="grey90",label=F)
text(mod, labels = Management, select = Management == "BF", display = "sites"))
dev.off()


#--------------------------
png("./figures/NMDS_GlacialDate.png", width = 856, height = 540)
# connect sites in the same treatment with a polygon use "ordihull"
ordiplot(myNMDS,type="n")
ordihull(myNMDS,groups=site.data1$GlacialDate,draw="polygon",col="grey90",label=T)
#orditorp(myNMDS,display="species",col="red",air=0.01)
#orditorp(myNMDS,display="sites",cex=0.75,air=0.01)
dev.off()

# link the sites within a treatment by lines
ordiplot(myNMDS,type="n")
ordispider(myNMDS,groups=site.data1$Bay,spiders="centroid",col="black",label=F)
orditorp(myNMDS,display="species",col="red",air=0.01)
orditorp(myNMDS,display="sites",cex=0.75,air=0.01)

#other plots
png("./figures/NMDS_site.png", width = 856, height = 540)
ordiplot(myNMDS,type="n")
ordihull(myNMDS,groups=site.data1$Bay,draw="polygon",col='blue',label=T)
dev.off()

png("./figures/NMDS_trampling.png", width = 856, height = 540)
ordiplot(myNMDS,type="n")
ordihull(myNMDS,groups=site.data1$GlacialDate,draw="polygon",col='blue',label=T)
dev.off()

ordiplot(myNMDS,type="n")
ordihull(myNMDS,groups=site.data1$Observer,draw="polygon",col='blue',label=T)

ordiplot(myNMDS,type="n")
ordihull(myNMDS,groups=site.data1$Bay,draw="polygon",col='blue',label=T)
orditorp(myNMDS,display="species",col="red",air=0.01)
orditorp(myNMDS,display="sites",cex=0.75,air=0.01)

#####################################
#
# Permutational analysis of variance for multivariate data {vegan}
#
#####################################

adonis(species.data1 ~ Bay*GlacialDate, data=site.data1, permutations=9999)
adonis(species.data1 ~ PLOT*OBSERVER*DATE, data=site.data1, permutations=9999)

#####################################
#
# Recommended online resources
#  https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf
#  https://cran.r-project.org/web/packages/vegan/vignettes/intro-vegan.pdf
#  http://ecology.msu.montana.edu/labdsv/R/labs/lab13/lab13.html
#
#####################################

# from point framing
####ADDITIONAL GRAPHS, ORDIPLOTS WITHOUT X, OTHER, LITTER, SOIL, ROCK

myNMDSrevised<-metaMDS(species.data,k=2)
myNMDSrevised #most important: is the stress low?
stressplot(myNMDSrevised) #low stress means that the observed dissimilarity between site pairs matches that on the 2-D plot fairly well (points hug the line)

plot(myNMDSrevised)#sites are open circles and species are red +'s

#the following commands create layers on a plot, and should be run sequentially
ordiplot(myNMDSrevised,type="n") #this clears the symbols from the plot
orditorp(myNMDSrevised,display="species",col="red",air=0.01) #this adds red species names
orditorp(myNMDSrevised,display="sites",cex=0.75,air=0.01) #this adds black site labels, cex is the font size

# connect sites in the same treatment with a polygon use "ordihull"
ordiplot(myNMDSrevised,type="n")
ordihull(myNMDSrevised,groups=site.data$Bay,draw="polygon",col="grey90",label=T)
orditorp(myNMDSrevised,display="species",col="red",air=0.01)
orditorp(myNMDSrevised,display="sites",cex=0.75,air=0.01)

# link the sites within a treatment by lines
ordiplot(myNMDSrevised,type="n")
ordispider(myNMDSrevised,groups=site.data$Date,spiders="centroid",col="black",label=F)
orditorp(myNMDSrevised,display="species",col="red",air=0.01)
orditorp(myNMDSrevised,display="sites",cex=0.75,air=0.01)

#other plots
ordiplot(myNMDSrevised,type="n")
ordihull(myNMDSrevised,groups=site.data$Bay,draw="polygon",col='blue',label=T)

ordiplot(myNMDSrevised,type="n")
ordihull(myNMDSrevised,groups=site.data$Observer,draw="polygon",col='blue',label=T)

ordiplot(myNMDSrevised,type="n")
ordispider(myNMDSrevised,groups=site.data$Bay,spiders="centroid",col="black",label=T)

ordiplot(myNMDSrevised,type="n")
ordihull(myNMDSrevised,groups=site.data$Bay,draw="polygon",col='blue',label=T)
