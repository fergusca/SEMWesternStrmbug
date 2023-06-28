## DATA EXPLORATION OF PROCESSED DATA FOR WEST PATH ANALYSES

library(dplyr)

# LOAD DATA from R package- a .rda file
load(file="data/nrsa_proc.rda")


##########
## DETERMINE THE MEAN MACROINVERTEBRATE VARIABLE VALUES WITHIN REFERENCE SITE
# WEST OE standard deviation for reference sites (n=170)
west_ref<-dat_proc%>%
  filter(AG_ECO3=="WMTNS")%>%
  filter(PROTOCOL=="WADEABLE")%>%
  filter(RT_MASTER=="R")
mean(west_ref$OE_SCORE) #0.9882746
sd(west_ref$OE_SCORE) #0.2327895

wmt_xer_ref<-dat_proc%>%
  filter(AG_ECO9=="WMT"|AG_ECO9=="XER")%>%
  filter(PROTOCOL=="WADEABLE")%>%
  filter(RT_MASTER=="R")%>%
  group_by(AG_ECO9)%>%
  #filter(!is.na(OE_SCORE))%>%
  summarize(meanMMI=mean(MMI_BENT),
            sdMMI=sd(MMI_BENT))
