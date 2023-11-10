######################
# Compaction data

#########################

# load data
compaction.data <- read.csv(file = "./data/raw_data/data_2023 - Compaction475.csv")
site.data <- read.csv(file = "./data/raw_data/data_2023 - Site_data.csv")

# make a transect column in site
site.data$transect <- paste0(site.data$SITE, "-", site.data$PLOT)

# join site and compaction data
compaction_site <- dplyr::left_join(compaction.data, site.data, by='transect')

# convert types
compaction_site$TRTMT <- as.factor(compaction_site$TRTMT)
compaction_site$compact_B <- as.numeric(compaction_site$compact_B)

# plot the data
boxplot(compact_B~TRTMT, data=compaction_site, las=2, col="light blue", xlab="TRTMT", ylab="CompactionB")
ggplot2::ggplot(data=compaction_site, ggplot2::aes(x=TRTMT, y=compact_B, colour=SITE)) + ggplot2::geom_point(size=3)



############################
# Make data long
# https://garrettgman.github.io/tidying/

# gather data by brushing
library(tidyr)

compaction_site_brushing <- gather(compaction_site, "Brushing", "Compaction", 4:5)

# Columns: Site, Trampled_trmt, Brush_trmt, Compaction value

mydata <- compaction_site_brushing[,c(2, 8,9,20,21)]

ggplot2::ggplot(data=mydata , ggplot2::aes(x=TRTMT, y=Compaction, colour=Brushing)) + ggplot2::geom_point(size=3)

# jitter

mydata$TRTMT_Brushing <- as.factor(paste0(mydata$TRTMT, "-", mydata$Brushing))
mydata$Compaction <- as.numeric(mydata$Compaction)
boxplot(Compaction~TRTMT_Brushing, data=mydata, las=1, col="light blue", xlab="TRTMT_Brushing", ylab="Compaction")

#-----------------------------
# ANoVA

# Tests the effects of trampling on soil compaction
model1<-lm(Compaction~TRTMT+SITE, data =mydata)
summary(model1)
anova(model1)

#--------------------------------
#write file to csv
write.csv(mydata, file = "./data/processed_data/garibaldi_trampling_compaction.csv")

#--------------------------
# Example of how to extract trampled vs untrampled
# species.data[which(site.data$TRTMT=="trampled"),])
