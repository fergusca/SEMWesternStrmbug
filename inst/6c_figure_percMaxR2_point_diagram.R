########################
## CALCULATE THE PROPORTION OF VARIANCE EXPLAINED OF THE MAX POSSIBLE R2 IN THE MODEL
##  TO CREATE POINT GRAPH SHOWING SCALED R2 BY RESPONSE VARIABLE AND BY ECOREGION ACROSS MODELS
#########################

remove(list=ls())

library(tidyverse)
library(ggplot2)

devtools::load_all()
library(SEMWesternStrmbug)

#########################
# Read final path model R2 values
# WMT v15
WMT_R2_OE <-read.csv("inst/Routput/WMTw_m15_OE_R2.csv")
WMT_R2_MMI <-read.csv("inst/Routput/WMTw_m15_MMI_R2.csv")
WMT_R2_EPT <-read.csv("inst/Routput/WMTw_m15_EPT_R2.csv")

# XER v15
XER_R2_OE <-read.csv("inst/Routput/XERw_m15_OE_R2.csv")
XER_R2_MMI <-read.csv("inst/Routput/XERw_m15_MMI_R2.csv")
XER_R2_EPT <-read.csv("inst/Routput/XERw_m15_EPT_R2.csv")

# Read maximum R2 estimates from S:N code
WMTmaxR2.df <-read.csv("inst/Routput/WMTw_estMaxR2.csv")
XERmaxR2.df <-read.csv("inst/Routput/XERw_estMaxR2.csv")

# MERGE observed R2 with maximum R2 for each dataset separately
WMT_R2_OE_proc <- left_join(WMT_R2_OE, WMTmaxR2.df, by=c("Variable"="METRIC"))%>%
  mutate(Model = "OE")%>%
  mutate(Ecoregion="WMT")

WMT_R2_MMI_proc <- left_join(WMT_R2_MMI, WMTmaxR2.df, by=c("Variable"="METRIC"))%>%
  mutate(Model = "MMI")%>%
  mutate(Ecoregion="WMT")

WMT_R2_EPT_proc <- left_join(WMT_R2_EPT, WMTmaxR2.df, by=c("Variable"="METRIC"))%>%
  mutate(Model = "EPT")%>%
  mutate(Ecoregion="WMT")

# XERIC
XER_R2_OE_proc <- left_join(XER_R2_OE, XERmaxR2.df, by=c("Variable"="METRIC"))%>%
  mutate(Model = "OE")%>%
  mutate(Ecoregion="XER")

XER_R2_MMI_proc <- left_join(XER_R2_MMI, XERmaxR2.df, by=c("Variable"="METRIC"))%>%
  mutate(Model = "MMI")%>%
  mutate(Ecoregion="XER")

XER_R2_EPT_proc <- left_join(XER_R2_EPT, XERmaxR2.df, by=c("Variable"="METRIC"))%>%
  mutate(Model = "EPT")%>%
  mutate(Ecoregion="XER")

###########
# Rowbind datasets together
All_R2 <- rbind(WMT_R2_OE_proc,WMT_R2_MMI_proc,WMT_R2_EPT_proc,
                XER_R2_OE_proc,XER_R2_MMI_proc,XER_R2_EPT_proc)

##############
## APPLY % max R2 function
All_R2_proc<- R2_calc(All_R2)
All_R2_proc

###########
## CLEAN UP Processed R2 combined dataset
#All_R2_proc2 <-All_R2_proc%>%
#  rename(Response=Variable,Obs_R2=R2)%>%
#  select(Category,Response,Model,Ecoregion,Obs_R2,MaxR2,R2_perc)

#names(All_R2_proc2)

############
## WRITE PROCESSED R2 dataframe
write.csv(All_R2_proc, "inst/Routput/R2_processed_m15.csv",
          row.names=FALSE)


#####################
## POINT PLOT of %R2 explained in the model
#####################
# Set font
windowsFonts(AR=windowsFont("Arial"))
# Color palette for response categories
vid <- c("#35b779","#31688e","#90d743","#fde725","#440154")#"#21918c",

# Order response variables for plot
All_R2_proc$Variable <- ordered(All_R2_proc$Variable, levels=c("Site-riparian cover index","Specific stream power",
                                                               "Summer flow/km2","Bankfull flow/km2","Evaporation indicator",
                                                               "Relative bed stability","TN","Sulfate",
                                                               "Biotic response"))
# Order models for plot
All_R2_proc$Model <- ordered(All_R2_proc$Model, levels=c("OE","MMI","EPT"))

# % of Maximum R2 accounted for in the models
point_percmaxr2<-ggplot(All_R2_proc, aes(x=R2_perc, y=Variable))+ #, group=Ecoregion
  geom_point(aes(shape=Ecoregion, color=Category, size=Ecoregion))+
  scale_color_manual(values=vid, drop=TRUE)+
  scale_size_manual(values=c(2.5,2.5))+
  scale_x_continuous(breaks=seq(10,100,20))+
  facet_wrap(~Model,ncol=3)+
  xlim(10,80)+
  #facet_wrap(~model, ncol=1)+
  theme_bw(base_size=10)+
  theme(axis.text.x=element_text(family="AR", size=10),
        axis.title.x=element_text(family="AR", size=10),
        axis.text.y=element_text(family="AR", size=10),
        axis.title.y=element_text(family="AR", size=10),
        legend.position= "right",
        legend.title=element_text(family="AR", size=10),
        legend.text=element_text(family="AR", size=10))+
  xlab(expression("%Max"~R^2))+
  ylab(NULL)

point_percmaxr2

# Observed R2 in the models
point_r2<-ggplot(All_R2_proc, aes(x=R2, y=Variable))+ #, group=Ecoregion
  geom_point(aes(shape=Ecoregion, color=Category,size=Ecoregion))+
  scale_color_manual(values=vid, drop=TRUE)+
  scale_size_manual(values=c(2.5,2.5))+
  scale_x_continuous(breaks=seq(0.10,0.70,0.1))+
  xlim(0.10,0.50)+
  guides(size="none")+
  facet_wrap(~Model,ncol=3)+
  #xlim()+
  #facet_wrap(~model, ncol=1)+
  theme_bw(base_size=10)+
  theme(axis.text.x=element_text(family="AR", size=10),
        axis.title.x=element_text(family="AR", size=10),
        axis.text.y=element_text(family="AR", size=10),
        axis.title.y=element_text(family="AR", size=10),
        legend.position= "right",
        legend.title=element_text(family="AR", size=10),#element_blank(),
        legend.text=element_text(family="AR", size=10))+
  xlab(expression("Observed"~R^2))+
  ylab(NULL)

point_r2


###################
# PRINT R2 PLOTS
tiff(filename="inst/Routput/Figures/Ptplot_m15_Max_percR2.tiff",
     width=6.5, height = 4, units="in", res=300)
point_percmaxr2
dev.off()

tiff(filename="inst/Routput/Figures/Ptplot_m15_ObsR2.tiff",
     width=6.5, height = 4, units="in", res=300)
point_r2
dev.off()
