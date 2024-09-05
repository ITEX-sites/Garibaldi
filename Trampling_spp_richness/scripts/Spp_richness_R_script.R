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
#setwd("~/GitHub/Garibaldi/Trampling_spp_richness")
getwd()
# read in spp and site matrices
species.data0 <- read.csv(file = "./data/processed_data/garibaldi_trampling_species_matrix_2024_data_t_vs_ut_cover.csv")
site.data <- read.csv(file = "./data/processed_data/garibaldi_trampling_site_matrix.csv")

# remove transect column
transect <- species.data0$transect
species.data <- species.data0[,-c(1:2)]
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
fisher.alpha(species.data) #this is Fisher's alpha from the log-series distribution, fairly independent of sample size

site.data$shannon<-(diversity(species.data, index = "shannon"))#makes a new column in site data with the shannon values
site.data$simpson<-(diversity(species.data, index = "simpson"))
site.data$fisher<-fisher.alpha(species.data)

# effects of OBSERVER on shannon diversity
model1<-lm(shannon~OBSERVER, data =site.data)
#summary(lm(shannon~OBSERVER, data =site.data))
anova(model1)

# effects of Site on shannon diversity
model2<-lm(shannon~SITE, data =site.data)
#summary(lm(shannon~SITE, data =site.data))
anova(model2)

# effects of TRTMT on shannon diversity
model3<-lm(shannon~TRTMT, data =site.data)
#summary(lm(shannon~TRTMT, data =site.data))
anova(model3)

# effects of aspect on shannon diversity
model3<-lm(shannon~aspect, data =site.data)
#summary(lm(shannon~aspect, data =site.data))
anova(model3)

# effects of slope on shannon diversity
model3<-lm(shannon~slope, data =site.data)
#summary(lm(shannon~slope, data =site.data))
anova(model3)

# effects of TRTMT on fisher alpha
model4<-lm(fisher~TRTMT, data =site.data)
#summary(lm(fisher~TRTMT, data =site.data))
anova(model4)

# effects of TRTMT on simpson diversity
model5<-lm(simpson~TRTMT, data =site.data)
#summary(lm(simpson~TRTMT, data =site.data))
anova(model5)


#---------------------------------------------
# create graph of species diversity separated by X1 and coloured by X2
#plot(site.data$shannon ~ site.data$TRTMT,  main= "Shannon Diversity Index changes with TRTMT", xlab="TRTMT", ylab="Shannon Diversity", pch=20)
#abline(model1) #adds the trend line

# ggplot(data=site.data, aes(x=SITE, y=shannon, colour=TRTMT)) + geom_point(size=3)+
#   stat_smooth(method = "lm")#add the line

ggplot(data=site.data, aes(x=SITE, y=shannon, colour=TRTMT)) + geom_point(size=3)+
  stat_smooth(method = "lm")#add the line

ggplot(data=site.data, aes(x=SITE, y=shannon, colour=OBSERVER)) + geom_point(size=3)+
  stat_smooth(method = "lm")#add the line

ggplot(data=site.data, aes(x=TRTMT, y=shannon, colour=SITE)) + geom_point(size=3)+
  geom_smooth(method = "lm") +
  scale_colour_manual(values = c("#283b00", "#afc249", "#ea9173")) +
  labs(x = 'Treatment', y = 'Shannon Diversity Index') +
  theme_bw() #add the line

# example of how to save a png image in R
ggsave(file = "./figures/Shannon_trampling_site_dotplot.jpg", plot = Shannondot, dpi = 800, units = "mm", width = 150, height = 100)
ggplot(data=site.data, aes(x=TRTMT, y=shannon, colour=SITE)) + geom_point(size=3)+
  scale_colour_manual(values = c("#283b00", "#afc249", "#ea9173")) +
  labs(x = 'Treatment', y = 'Shannon Diversity Index') +
  theme_bw() -> Shannondot
dev.off()


#####################################
#
# Evenness index {vegan}
#
#####################################

PielouJ <- diversity(species.data, index = "shannon")/log(specnumber(species.data)) # Pielou's J
site.data <- cbind(site.data, PielouJ)

model5<-lm(PielouJ~SITE, data =site.data)
anova(model5)
model5<-lm(PielouJ~TRTMT, data =site.data)
anova(model5)

boxplot(shannon~SITE, data=site.data, col="#afc249", xlab="SITE", ylab="Shannon Diversity Index", main="Shannon Diversity")

#Revised Shannon vs TRTMT graph
ggplot(site.data, aes(x=TRTMT, y= shannon)) +
  geom_boxplot(color = "#283b00", fill = "#afc249") +
  labs(x = 'TRTMT', y = 'Shannon Diversity Index', title = 'Shannon Diversity') + theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

boxplot(shannon~TRTMT, data=site.data, las=2, col="light blue", xlab="TRTMT", ylab="Shannon Diversity Index", main="Shannon Diversity")

#------------------------------------
# make SITE_TRMT combined character and plot

site.data$SITE.TRTMT <- paste0(site.data$TRTMT, "_", site.data$SITE)

ggsave(file = "./figures/Shannon_trampling_site_boxplot.jpg", plot = Shannonbox, dpi = 800, units = "mm", width = 150, height = 100)
#Revised Shannon vs Site.TRTMT graph
ggplot(site.data, aes(x=SITE.TRTMT, y= shannon)) +
  geom_boxplot(color = "#283b00", fill = "#afc249") +
  labs(x = 'Site/Treatment', y = 'Shannon Diversity Index') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) -> Shannonbox

boxplot(shannon~SITE.TRTMT, data=site.data, las=2, col="light blue", xlab="Site/TRTMT", ylab="Shannon Diversity Index", main="Shannon Diversity")
dev.off()
dev.new()
#Revised PielouJ vs Site/TRTMT graph
ggplot(site.data, aes(x=TRTMT, y=PielouJ)) +
  geom_boxplot(color = "black", aes(fill = SITE)) +
  theme_bw() +
  labs(x = 'Site/Treatment', y = 'Evenness') +
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#283b00", "#afc249", "#ea9173"))-> PielouJbox


ggsave(file = "./figures/PielouJ_trampling_site_boxplot.jpg", plot = PielouJbox, dpi = 800, units = "mm", width = 150, height = 100)
# add png code here to export plots (see above for Shannon)
boxplot(PielouJ~SITE.TRTMT, data=site.data, col="light blue", xlab="TRTMT", ylab="Pielou's J Evenness", main="Pielou's J evenness")

#####################################
#
# Rarefaction {vegan}
#
#####################################

min(rowSums(species.data))
rarefy(species.data, 15) #if only 15 individuals had been sampled in each treatment, what would the diversity have been?

#####################################
#
# Species - Frequency plots, traditional for log series {vegan}
#
#####################################

dev.off()
dev.new()
par(mfrow=c(1,1))
plot(fisherfit(colSums(species.data))) #here is a plot of the number of species for each "bin" of abundances, abundances are summed over all sites

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


RankAbun.1 <- rankabundance(species.data)
RankAbun.1 # a dataframe of the rank of each species

png("./figures/Rank_abundance_total.jpg", width = 856, height = 540)
rankabunplot(RankAbun.1,scale='abundance', addit=FALSE, specnames=c(1:31), srt = 45, xlim = c(1,32), ylim = c(0,1000)) #rank abundance plot, labelling the most common 3 species
dev.off()

site.data$TRTMT <- as.factor(site.data$TRTMT)
site.data$SITE <- as.factor(site.data$SITE)
site.data$Observer <- as.factor(site.data$OBSERVER)
site.data$SITE.TRTMT <- as.factor(site.data$SITE.TRTMT)

rankabuncomp(species.data, y=site.data, factor=c('TRTMT'),scale='abundance', srt = 45, xlim = c(1,35), ylim = c(0,500))

rankabuncomp(species.data, y=site.data, factor=c('TRTMT'),scale='proportion', legend=TRUE)->rankabuntut #click on where on plot you want to have the legend
dev.off()
png("./figures/Rank_abundance_treatment.jpg",res=800, width = 15, height = 10, units = "cm")
rankabuncomp(species.data, y=site.data, factor=c('TRTMT'),scale='abundance', srt = 45, xlim = c(1,35), ylim = c(0,500))
 #click on where on plot you want to have the legend
dev.off()

rankabuncomp(species.data, y=site.data, factor=c('SITE'),scale='proportion', legend=TRUE, specnames=c(1:3)) #click on where on plot you want to have the legend
rankabuncomp(species.data, y=site.data, factor=c('SITE.TRTMT'),scale='proportion', legend=TRUE) #click on where on plot you want to have the legend

#------------------------------------
RankAbun.trampled <- rankabundance(species.data[which(site.data$TRTMT=="trampled"),])
RankAbun.trampled # a dataframe of the rank of each species

png("./figures/Rank_abundance_trampled.jpg", width = 856, height = 540)
rankabunplot(RankAbun.trampled,scale='abundance', addit=FALSE, specnames=c(1:50), ylim=c(0,500),srt = 45) #rank abudnance plot, labelling the most common 3 species
dev.off()

RankAbun.untrampled <- rankabundance(species.data[which(site.data$TRTMT=="untrampled"),])
RankAbun.untrampled # a dataframe of the rank of each species

png("./figures/Rank_abundance_untrampled.jpg", width = 856, height = 540)
rankabunplot(RankAbun.untrampled,scale='abundance', addit=FALSE, specnames=c(1:50), ylim=c(0,500), srt = 45) #rank abudnance plot, labelling the most common 3 species
dev.off()

# Erigeron glacialis looks like it is relatively less with trampling

#####################################
#
# Similarity matrices (vegan package required)
#
#####################################

dissim.mat<-vegdist(species.data, method="bray", binary=FALSE)
#dissim.mat

#?vegdist #link to other dissimilarity metrics you could use in place of "bray"
#these include: "manhattan", "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao" or "mahalanobis".

dissim.mat<-vegdist(species.data, method="jaccard", binary=TRUE)
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
plot(fit)

plot(fit); rect.hclust(fit, h=0.5, border="red") # emphasize clusters <0.5 different

#####################################
#
# Ordination: nMDS (requires vegan package)
#
#####################################
# convert NA's to 0
species.data[is.na(species.data)] <- 0

# remove all rows and cols with sum of 0
summed_rows <- apply(species.data, 1, sum)
species.data0 <- species.data[-which(summed_rows==0),]
summed_cols <- apply(species.data, 2, sum)
species.data1 <- species.data0[,-which(summed_cols==0)]
site.data1 <- site.data[-which(summed_rows==0),]
#-----------------------------------
myNMDS<-metaMDS(species.data1,k=4)
myNMDS #most important: is the stress low?
stressplot(myNMDS) #low stress means that the observed dissimilarity between site pairs matches that on the 2-D plot fairly well (points hug the line)

plot(myNMDS)#sites are open circles and species are red +'s

#the following commands create layers on a plot, and should be run sequentially
ordiplot(myNMDS,type="n") #this clears the symbols from the plot
orditorp(myNMDS,display="species",col="red",air=0.01) #this adds red species names
orditorp(myNMDS,display="sites",cex=0.75,air=0.01) #this adds black site labels, cex is the font size

png("./figures/NMDS_spp.jpg", width = 856, height = 540)
# connect sites in the same treatment with a polygon use "ordihull"
ordiplot(myNMDS,type="n")
ordihull(myNMDS,groups=site.data1$TRTMT,draw="polygon",col="grey90",label=T)
orditorp(myNMDS,display="species",col="red",air=0.01)
orditorp(myNMDS,display="sites",cex=0.75,air=0.01)
dev.off()

# link the sites within a treatment by lines
ordiplot(myNMDS,type="n")
ordispider(myNMDS,groups=site.data1$SITE.TRTMT,spiders="centroid",col="black",label=F)
orditorp(myNMDS,display="species",col="red",air=0.01)
orditorp(myNMDS,display="sites",cex=0.75,air=0.01)

#other plots
png("./figures/NMDS_site.jpg", width = 856, height = 540)
ordiplot(myNMDS,type="n")
ordihull(myNMDS,groups=site.data1$SITE,draw="polygon",col='blue',label=T)
dev.off()

png("./figures/NMDS_trampling.jpg", width = 856, height = 540)
ordiplot(myNMDS,type="n")
ordihull(myNMDS,groups=site.data1$TRTMT,draw="polygon",col='blue',label=T)
dev.off()

ordiplot(myNMDS,type="n")
ordihull(myNMDS,groups=site.data1$Observer,draw="polygon",col='blue',label=T)

ordiplot(myNMDS,type="n")
ordispider(myNMDS,groups=site.data1$SITE,spiders="centroid",col="black",label=T)

#####################################
#
# Permutational analysis of variance for multivariate data {vegan}
#
#####################################

adonis(species.data1 ~ SITE*TRTMT, data=site.data1, permutations=9999)
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
ordihull(myNMDSrevised,groups=site.data$SITE,draw="polygon",col="grey90",label=T)
orditorp(myNMDSrevised,display="species",col="red",air=0.01)
orditorp(myNMDSrevised,display="sites",cex=0.75,air=0.01)

# link the sites within a treatment by lines
ordiplot(myNMDSrevised,type="n")
ordispider(myNMDSrevised,groups=site.data$SITE.TRTMT,spiders="centroid",col="black",label=F)
orditorp(myNMDSrevised,display="species",col="red",air=0.01)
orditorp(myNMDSrevised,display="sites",cex=0.75,air=0.01)

#other plots
ordiplot(myNMDSrevised,type="n")
ordihull(myNMDSrevised,groups=site.data$SITE,draw="polygon",col='blue',label=T)

ordiplot(myNMDSrevised,type="n")
ordihull(myNMDSrevised,groups=site.data$Observer,draw="polygon",col='blue',label=T)

ordiplot(myNMDSrevised,type="n")
ordispider(myNMDSrevised,groups=site.data$SITE,spiders="centroid",col="black",label=T)

ordiplot(myNMDSrevised,type="n")
ordihull(myNMDSrevised,groups=site.data$SITE,draw="polygon",col='blue',label=T)
