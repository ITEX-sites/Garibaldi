library(tidyverse)
pf<-read.csv("Point_frame_analysis/Point_frame_data/cleaned_Garibaldi_point_frame_data.csv")
spp<-read.csv("Point_frame_analysis/Point_frame_data/Species_list.csv")
unique(pf$SPP)
spp$SPP<-spp$SPP_code
pf<-left_join(pf, spp)

pfx<-group_by(pf, SITE, PLOT, SPP, Species.name)%>%summarise(plot_total=n())%>%ungroup(.)%>%group_by(SITE, SPP, Species.name)%>%mutate(site_total=sum(plot_total))
pfx<-subset(pfx, SPP!="x"& Species.name!="Litter"& Species.name!="Soil"& Species.name!="Other"& Species.name!="Rock")
pfx<-group_by(pfx, SITE)%>%mutate(rich= n_distinct(SPP))
