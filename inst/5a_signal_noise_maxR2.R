#########################
## CALCULATING SIGNAL:NOISE OF RESPONSE VARIABLES
## BY ECOREGION
## Using script from Karen Blocksom email 2/10/23
##
#########################

remove(list=ls())

require(lme4)
require(Hmisc)
require(plyr)
require(dplyr)
require(gtools)
require(ggplot2)
require(tidyr)

library(devtools)
devtools::load_all()
library(SEMWesternStrmbug)

##########
## READ PROCESSED DATA
# COMPILED NRSA SURVEYS WITH SUBSET OF VARIABLES
#  INCLUDES ALL RESAMPLED SITES AND VISITS 1 & 2

# COMPLETE DATASET WITH ALL THREE SURVEYS COMBINED
#  VISITS 1 and 2 n = 6674
# Load from R package- a .rda file
load(file="data/nrsa_oe_all.rda")

# KEEP BOTH VISITS
dat_proc<- nrsa_oe_all

###############
## PROCESS DATA DROPPING MISSING PROTOCOL
table(dat_proc$AG_ECO9)
dat_proc$PROTOCOL<-as.factor(dat_proc$PROTOCOL)
summary(dat_proc$PROTOCOL)

# n = 6644
dat_proc<- dat_proc%>%
  drop_na(PROTOCOL)%>%
  filter(PROTOCOL=="BOATABLE"|PROTOCOL=="WADEABLE")

# DROP NOPHAB class from REALM
dat_proc$PROTOCOL<-droplevels(dat_proc$PROTOCOL)
table(dat_proc$PROTOCOL)
#BOATABLE WADEABLE
#    2662     3982

#########################
## SUBSET BY AGGREGATED 9- ECOREGION

# WMT
wmt<-dat_proc%>%
  filter(AG_ECO9=="WMT")%>%
  drop_na(LOE_QLow_cl)

summary(wmt$LOE_Qbkf_cl)

# SCALE CUMULATIVE PRECIPITATION in wmt
wmt$PSUMPY_SY_WS_sc<-scale(wmt$PSUMPY_SY_WS)
summary(wmt$PSUMPY_SY_WS_sc)

# WADEABLE n = 508 obs; sites = 324
wmt_w <- wmt %>%
  filter(PROTOCOL=="WADEABLE")
length(unique(wmt_w$SITE_ID))

# BOATABLE n = 268
wmt_b <-wmt %>%
  filter(PROTOCOL=="BOATABLE")

table(wmt_w$VISIT_NO)
#  1   2
#477  31

table(wmt_w$YEAR,wmt_w$VISIT_NO)
#     1  2
#2008  58   6
#2009  82   1
#2013  77   4
#2014 104   7
#2018  73   8
#2019  83   5

##############
## XER
xer<-dat_proc%>%
  filter(AG_ECO9=="XER")%>%
  drop_na(LOE_QLow_cl)

summary(xer$LOE_Qbkf_cl)

# SCALE CUMULATIVE PRECIPITATION in xer
xer$PSUMPY_SY_WS_sc<-scale(xer$PSUMPY_SY_WS)
summary(xer$PSUMPY_SY_WS_sc)

# WADEABLE n = 385
xer_w <- xer %>%
  filter(PROTOCOL=="WADEABLE")

# BOATABLE n = 238
xer_b <-xer %>%
  filter(PROTOCOL=="BOATABLE")

########################
## SINGAL:NOISE OF BENTHIC INDICES AND METRICS
#########
# WMTw Subset data
wmtw_red<-wmt_w%>%
  select(UID, UNIQUE_ID, YEAR, OE_SCORE, MMI_BENT, EPT_RICH)

#wmtw_red<-wmt_w%>%
#  select(UID, UNIQUE_ID, YEAR, OE_SCORE, MMI_BENT, EPT_RICH,LRBS_use, Lpt01_XCMGW,LQLow_kmcl,LQbkf_kmcl,L_NTL,L_SULF,NTL_RESULT,SULFATE_RESULT)

# CALL S:N function
wmt_SN<-snTest(wmtw_red,idVars.samp="UID",idVars.site="UNIQUE_ID",year='YEAR')

#[1] "Number of revisits:" "175"
#METRIC SIGNAL NOISE SN_RATIO COM
#1 OE_SCORE   0.04  0.02     1.74  NA
#2 MMI_BENT 350.69 83.92     4.18  NA
#3 EPT_RICH  29.37  7.88     3.73  NA

############
# XERw Subset data
xerw_red<-xer_w%>%
  select(UID, UNIQUE_ID, YEAR, OE_SCORE, MMI_BENT, EPT_RICH)

# CALL S:N function
xer_SN<-snTest(xerw_red,idVars.samp="UID",idVars.site="UNIQUE_ID",year='YEAR')
#[1] "Number of revisits:" "107"
#METRIC SIGNAL  NOISE SN_RATIO COM
#1 OE_SCORE   0.07   0.02     2.93  NA
#2 MMI_BENT 293.08 118.81     2.47  NA
#3 EPT_RICH  23.84   5.88     4.05  NA

########################
## SINGAL:NOISE OF PREDICTORS
#########
# WMTw Subset data
wmtw_red<-wmt_w%>%
  select(UID, UNIQUE_ID, YEAR, OE_SCORE,MMI_BENT_sc, EPT_RICH_sc, LRBS_use, L_STRM_POWER,
         Lpt01_XCMGW, LQLow_kmcl, LQbkf_kmcl,  evap_index_sc,#d.excess,evap_index,
         L_NTL, L_SULF)#, NTL_RESULT, SULFATE_RESULT)

# CALL S:N function
wmt_SN<-snTest(wmtw_red,idVars.samp="UID",idVars.site="UNIQUE_ID",year='YEAR')
wmt_SN

#METRIC SIGNAL NOISE SN_RATIO COM
#1   LRBS_use   0.53  0.04    12.85  NA
#2Lpt01_XCMGW   0.13  0.01     9.00  NA
#5L_STRM_POWER     0.35  0.01    38.90  NA
#3 LQLow_kmcl   0.70  0.07     9.63  NA
#4 LQbkf_kmcl   0.81  0.06    14.50  NA
#5      L_NTL   0.13  0.02     6.25  NA
#6     L_SULF   0.54  0.00   145.67  NA


# XERw Subset data
xerw_red<-xer_w%>%
  select(UID, UNIQUE_ID, YEAR, OE_SCORE, MMI_BENT_sc, EPT_RICH_sc, LRBS_use, L_STRM_POWER,
         Lpt01_XCMGW, LQLow_kmcl, LQbkf_kmcl,  evap_index_sc,#d.excess,evap_index, evap_index_sc,
         L_NTL, L_SULF)#, NTL_RESULT, SULFATE_RESULT)

# CALL S:N function
xer_SN<-snTest(xerw_red,idVars.samp="UID",idVars.site="UNIQUE_ID",year='YEAR')

xer_SN
#       METRIC SIGNAL NOISE SN_RATIO COM
#1    LRBS_use   0.74  0.20     3.70  NA
#2 Lpt01_XCMGW   0.21  0.01    17.50  NA
#5L_STRM_POWER   0.50  0.08     6.29  NA
#3  LQLow_kmcl   1.17  0.12     9.87  NA
#4  LQbkf_kmcl   1.07  0.13     8.49  NA
#5       L_NTL   0.17  0.02     7.12  NA
#6      L_SULF   0.87  0.01    88.81  NA

###################
## CALCULATE MAX r-squared using SITE_ID as random effect (Lester Yuan's approach)
##  From code Ryan shared 2/15/2023
# MaxR2 = SN/(SN+1)
##################
# WMT wadeable - OE, MMI, EPT
# List of Unique Metric names
unqMetric <- unique(wmt_SN$METRIC)

# Create empty dataframe to populate
WMTmaxR2.df <- data.frame()

for(i in length(unqMetric)){
  maxR2 <- wmt_SN$SN_RATIO/(wmt_SN$SN_RATIO+1)
  WMTmaxR2.df <- rbind(WMTmaxR2.df,
                       data.frame(METRIC=unqMetric, MaxR2=maxR2))
}

WMTmaxR2.df
#############
# WRITE WMT R2 TABLE
write.csv(WMTmaxR2.df, "inst/Routput/WMTw_estMaxR2.csv",
          row.names=FALSE)

################
## XER wadeable - OE, MMI, EPT
# List of Unique Metric names
unqMetric <- unique(xer_SN$METRIC)

# Create empty dataframe to populate
XERmaxR2.df <- data.frame()

for(i in length(unqMetric)){
  maxR2 <- xer_SN$SN_RATIO/(xer_SN$SN_RATIO+1)
  XERmaxR2.df <- rbind(XERmaxR2.df,
                       data.frame(METRIC=unqMetric, MaxR2=maxR2))
}

XERmaxR2.df

#############
# WRITE XER R2 TABLE
write.csv(XERmaxR2.df, "inst/Routput/XERw_estMaxR2.csv",
          row.names=FALSE)


########################
## CALCULATE THE PROPORTION OF VARIANCE EXPLAINED OF THE MAX POSSIBLE R2 IN THE MODEL

# Read final path model R2 values
# WMT v15
WMT_R2_OE <-read.csv("inst/Routput/WMTw_m15_OE_R2.csv")
WMT_R2_MMI <-read.csv("inst/Routput/WMTw_m15_MMI_R2.csv")
WMT_R2_EPT <-read.csv("inst/Routput/WMTw_m15_EPT_R2.csv")

# XER v15
XER_R2_OE <-read.csv("inst/Routput/XERw_m15_OE_R2.csv")
XER_R2_MMI <-read.csv("inst/Routput/XERw_m15_MMI_R2.csv")
XER_R2_EPT <-read.csv("inst/Routput/XERw_m15_EPT_R2.csv")

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
# Need to treat SEMNRSA like a package to be able to run function relabeling predictors
devtools::load_all()
library(SEMWestStrmbug)

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
All_R2_proc$Variable <- ordered(All_R2_proc$Variable, levels=c("Site-riparian index","Specific stream power",
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


#############
## COMPARE S:N FOR SPECIFIC NRSA SURVEY PERIODS TO COMPARE WITH RENEE'S RESULTS
## NRSA 2013-14
test<-nrsa_oe_all%>%
  filter(YEAR=="2013"|YEAR=="2014")#%>%
  #filter(AG_ECO9=="WMT")

# S:N for NRSA 2013-14
test_red<-test%>%
  select(UID, UNIQUE_ID, YEAR, OE_SCORE,MMI_BENT_sc, EPT_RICH_sc, LRBS_use, L_STRM_POWER,
         Lpt01_XCMGW, LQLow_kmcl, LQbkf_kmcl,  evap_index_sc,d.excess,evap_index,
         L_NTL, L_SULF)#, NTL_RESULT, SULFATE_RESULT)

# CALL S:N function
test_SN<-snTest(test_red,idVars.samp="UID",idVars.site="UNIQUE_ID",year='YEAR')
test_SN

## NRSA 2018-19
test2<-nrsa_oe_all%>%
  filter(YEAR=="2018"|YEAR=="2019")

# S:N for NRSA 2013-14
test2_red<-test2%>%
  select(UID, UNIQUE_ID, YEAR, OE_SCORE,MMI_BENT_sc, EPT_RICH_sc, LRBS_use, L_STRM_POWER,
         Lpt01_XCMGW, LQLow_kmcl, LQbkf_kmcl,  evap_index_sc,d.excess,evap_index,
         L_NTL, L_SULF)#, NTL_RESULT, SULFATE_RESULT)

# CALL S:N function
test2_SN<-snTest(test2_red,idVars.samp="UID",idVars.site="UNIQUE_ID",year='YEAR')
test2_SN
