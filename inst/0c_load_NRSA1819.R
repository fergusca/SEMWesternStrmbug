################
## LOAD NRSA 2018-19 DATA
###########
# ORIGINAL NRSA 2018-19 DATA downloaded from website 1/21/2022

rm(list=ls())

# Libraries
library(dplyr)
library(tidyr)

library(devtools)
devtools::load_all("./") # Loads current directory
library(SEMWesternStrmbug)

# LOAD data stored in Rpackage - a .rda file
#load(file="data/nrsa.rda")

###############
# On OneDrive
site_org <- read_csv("data/NRSA1819/nrsa-1819-site-information-data-updated.csv")
chem_org <- read_csv("data/NRSA1819/nrsa_1819_water_chemistry_chla_-_data.csv")
phab_org <- read_csv("data/NRSA1819/nrsa_1819_physical_habitat_larger_set_of_metrics_-_data.csv")
strcat_org <-read_csv("data/NRSA1819/FINAL_TABLE_NRSA_1819.csv")
strcat_nlcd<-read.csv("data/NLCD_NRSA081319.csv")

# Load other NRSA data compiled across surveys
benthic_oe_org<-read_csv("data/NRSA2008-2019_OE_Scores.csv")
isotope_org <- read_csv("data/NRSA1819/NRSA 2018 2019 Water Isotope.csv")
phab_oe_org<-read_csv("data/NRSA1819/NRSA_PHab_Discharge_OE1819.csv")
wqii_org<-read_csv("data/wqii_nrsa.csv")

# FORMAT DATE IN SITE
site_org$DATE_COL<-as.Date(site_org$DATE_COL, format="%m/%d/%Y")
str(site_org$DATE_COL)
site_org$YEAR <-format(site_org$DATE_COL,"%Y")
table(site_org$YEAR)
# 2018 2019
# 1044 1068
# Remove UID - this may be old? - will replace with UID from benthic O/E dataset Karen B shared
site_org<-site_org%>%
  select(!UID)

# Reduce datasets to merge together
chem <- chem_org%>%
  select(c("SITE_ID","VISIT_NO",
           "AMMONIA_N_RESULT",'AMMONIA_N_UNITS',
           "ANC_RESULT","ANC_UNITS",
           "CALCIUM_RESULT","CALCIUM_UNITS",
           "CHLORIDE_RESULT","CHLORIDE_UNITS",
           "COLOR_RESULT", "COLOR_UNITS",
           "COND_RESULT","COND_UNITS",
           "DOC_RESULT","DOC_UNITS",
           "MAGNESIUM_RESULT","MAGNESIUM_UNITS",
           "NITRATE_N_RESULT","NITRATE_N_UNITS",
           "NITRATE_NITRITE_N_RESULT","NITRATE_NITRITE_N_UNITS",
           "NITRITE_N_RESULT","NITRITE_N_UNITS",
           "NTL_RESULT", "NTL_UNITS",
           "PH_RESULT","PH_UNITS",
           "POTASSIUM_RESULT","POTASSIUM_UNITS",
           "PTL_RESULT","PTL_UNITS",
           "SILICA_RESULT","SILICA_UNITS",
           "SODIUM_RESULT", "SODIUM_UNITS",
           "SULFATE_RESULT","SULFATE_UNITS",
           "TSS_RESULT","TSS_UNITS",
           "TURB_RESULT", "TURB_UNITS"))

phab <-phab_org%>%
  select(c("SITE_ID","VISIT_NO","PROTOCOL","XDEPTH_CM","SDDEPTH_CM",'XWIDTH',"XWXD","RP100","RPGT50","XBKF_W","XBKF_H","XINC_H","SINU","XSLOPE","REACHLEN",
           "LSUB_DMM","XEMBED","PCT_FN","PCT_SA","PCT_SAFN","PCT_SFGF",
           "LDCBF_G08","LDMB_BW5","LRBS_BW5","LRBS_G08",
           "PCT_FAST","PCT_POOL","PCT_SLOW",
           "RpRat","RPMXDEP_CM","RPXDEP_CM",
           "PCT_BDRK",
           "XFC_ALG","XFC_AQM","XFC_LWD","XFC_BRS", "XFC_OHV","XFC_BIG","XFC_NAT","XFC_UCB","V1W_MSQ",
           "XCDENBK","XCDENMID","XCL","XGB","XGW","XMW","XC","XCS","XCL","XCM","XCMGW","XPCAN","XPCM","XPCMG",
           "QR1","QRVeg1","RDIST1",
           "W1_HALL","W1H_WALL","W1H_LOG","W1_HNOAG","W1_HAG"))

# NOT IN 2018-19 "RB3","REYP3","RRPW3","RPRAT","QRVEG1",


strcat_red <-strcat_org%>%
  select("UNIQUE_ID",
         "CatAreaSqKm","WsAreaSqKm","CatAreaSqKmRp100","WsAreaSqKmRp100",
         "HydrlCondWs","OMHWs",
         "RdDensWs","RdDensWsRp100","RdDens_WS_PctFull","RdDens_RipBuf100_WS_PctFullRp100",
         "DamDensWs","DamNIDStorWs","DamNrmStorWs",
         "NABD_DensWs","NABD_NIDStorWs","NABD_NrmStorWs",
         "MineDensWs","MineDensWsRp100",
         "AgKffactWs","FertWs","ManureWs","NPDESDensWs","NPDESDensWsRp100",
         "SuperfundDensWs",
         "HUDen2010Ws","HUDen2010WsRp100","PopDen2010Ws","PopDen2010WsRp100",
         "PctOw2019Ws","PctIce2019Ws","PctUrbOp2019Ws","PctUrbLo2019Ws","PctUrbMd2019Ws","PctUrbHi2019Ws",
         "PctDecid2019Ws","PctConif2019Ws","PctMxFst2019Ws",
         "PctShrb2019Ws","PctGrs2019Ws",
         "PctHay2019Ws","PctCrop2019Ws",
         "PctWdWet2019Ws","PctHbWet2019Ws",
         "PctOw2019_WsRp100","PctIce2019_WsRp100","PctUrbOp2019_WsRp100","PctUrbLo2019_WsRp100","PctUrbMd2019_WsRp100","PctUrbHi2019_WsRp100",
         "PctDecid2019_WsRp100","PctConif2019_WsRp100","PctMxFst2019_WsRp100",
         "PctShrb2019_WsRp100","PctGrs2019_WsRp100",
         "PctHay2019_WsRp100","PctCrop2019_WsRp100",
         "PctWdWet2019_WsRp100","PctHbWet2019_WsRp100",
         "PctFrstLoss2013Ws","PctFrstLoss2013WsRp100",
         "PctImp2019Ws","PctImp2019_RipBuf100WsRp100",
         "BFIWs","BFI_WS_PctFull",
         "PctNOT_PRINCIPLEWs","PctSANDSTONEWs","PctSEMICONSOL_SANDWs","PctVOLCANICWs",
         "PctSANDSTONE_CARBONATEWs","PctUNCONSOL_SAND_GRAVELWs","PctCARBONATEWs",
         "ClayWs",'PctCarbResidWs',"PctNonCarbResidWs","PctAlkIntruVolWs","PctSilicicWs",
         "PctExtruVolWs","PctColluvSedWs",
         "PctGlacTilClayWs","PctGlacTilLoamWs","PctGlacTilCrsWs","PctGlacLakeCrsWs","PctGlacLakeFineWs",
         "PctHydricWs","PctEolCrsWs","PctEolFineWs","PctSalLakeWs","PctAlluvCoastWs","PctCoastCrsWs","PctWaterWs",
         "ElevWs","PermWs","KffactWs","Kffact_PT","LSTFRZWs","LSTFRZ_PT","RDHWs","RDH_PT","RHMEANWs","RHMEAN_PT",
         "RunoffWs","WDMAX_PT","WDMIN_PT","WDSUM_PT",
         "PMAXWs","PMINWs","Precip8110Ws","Tmax8110Ws","Tmean8110Ws","Tmin8110Ws",
         "TMEAN_S_2018Ws","TMEAN_S_2018_PT","TMEAN_S_2019Ws","TMEAN_S_2019_PT","TMEAN_W_2018Ws","TMEAN_W_2018_PT",
         "TMEAN_W_2019Ws","TMEAN_W_2019_PT","TMEAN_PW_2018Ws","TMEAN_PW_2018_PT","TMEAN_PW_2019Ws","TMEAN_PW_2019_PT",
         "TMEAN_SY_2018Ws","TMEAN_SY_2018_PT","TMEAN_SY_2019Ws","TMEAN_SY_2019_PT",
         "PSUMPY_2018Ws","PSUMPY_2018_PT","PSUMPY_2019Ws","PSUMPY_2019_PT",
         "MAST_2018","MAST_2019","MSST_2018","MSST_2019","MWST_2018","MWST_2019")


###################
# STREAMCAT CATCHMENT SCALE - Compiled using the StreamCat Tool R package 8/15/23
strcat_nlcd_red<-strcat_nlcd%>%
  select("UNIQUE_ID","COMID",
         "PCTOW2019CAT","PCTICE2019CAT","PCTURBOP2019CAT","PCTURBLO2019CAT","PCTURBMD2019CAT","PCTURBHI2019CAT",
         "PCTDECID2019CAT","PCTCONIF2019CAT","PCTMXFST2019CAT",
         "PCTSHRB2019CAT","PCTGRS2019CAT",
         "PCTHAY2019CAT","PCTCROP2019CAT",
         "PCTWDWET2019CAT","PCTHBWET2019CAT",
         "PCTOW2019CATRP100","PCTICE2019CATRP100",
         "PCTURBOP2019CATRP100","PCTURBLO2019CATRP100","PCTURBMD2019CATRP100","PCTURBHI2019CATRP100",
         "PCTDECID2019CATRP100","PCTCONIF2019CATRP100","PCTMXFST2019CATRP100",
         "PCTSHRB2019CATRP100","PCTGRS2019CATRP100",
         "PCTHAY2019CATRP100","PCTCROP2019CATRP100",
         "PCTWDWET2019CATRP100","PCTHBWET2019CATRP100",
         "PCTIMP2019CAT","PCTIMP2019CATRP100","PCTAGDRAINAGEWS","PCTAGDRAINAGECAT")

# MERGE STREAMCAT SUBSETS
strcat_tot<-left_join(strcat_red,strcat_nlcd_red,by="UNIQUE_ID")
summary(strcat_tot$PCTAGDRAINAGECAT)

# Updated benthic O/E
# PROCESS DATA - SELECT YEAR 2018-19
# DATE_COL from a character to a date
benthic_oe_org$DATE_COL<-as.Date(benthic_oe_org$DATE_COL, format="%m/%d/%Y")
benthic_oe_org$YEAR <-format(benthic_oe_org$DATE_COL,"%Y")
benthic_oe <-benthic_oe_org%>%
  filter(YEAR==2018|YEAR==2019)%>%
  select(c("UID","SITE_ID","VISIT_NO","OE_SCORE")) #n=2105

#PHab O/E
phab_oe <- phab_oe_org%>%
  select(c("SITE_ID","VISIT_NO","REALM","W1H_CROP",
           "XSLOPE_use","XWIDTH_use","Lpt01_XCMGW","Lpt01_XFC_NAT",
           "LRBS_use","RDIST_COND","LRBS_Cond_use","LOE_RBS_use",
           "LXCMGW_Cond_use","LOE_XCMGW_use","LXFC_NAT_Cond_use","LOE_XFC_NAT_use",
           "Qbkf_cl","LQbkf_cl","Qbkf_kmcl","LQbkf_kmcl","QLow_cl","LQLow_cl",
           "QLow_cl","LQLow_cl","QLow_kmcl","LQLow_kmcl","LOE_QLow_cl","LOE_Qbkf_cl"
  ))

length(unique(phab_oe$SITE_ID))#n=1919

# Check for duplicate obs n = 4, 2each: SITE_ID=NRS18_MN_RF002 and NRS18_MN_RF001
test<-phab_oe%>%
  group_by(SITE_ID,VISIT_NO)%>%
  filter(n()>1)

# WQII
names(wqii_org)
wqii<- wqii_org%>%
  filter(YEAR==2018|YEAR==2019)%>%
  select(c("SITE_ID","VISIT_NO","RT_WQI",
           "CL_pt","SO4_pt","PTL_pt","NTL_pt",
           "TURB_pt","ENTERO_PT",
           "DO_PT","PH_PT","NUMVARN","WQII"))

length(unique(wqii$SITE_ID)) #n=1919 out of 2112 observations


# Water isotopes
isotope_org$DATE_COL<-as.Date(isotope_org$DATE_COL, format="%m/%d/%Y")
isotope_org$YEAR <-format(isotope_org$DATE_COL,"%Y")
table(isotope_org$YEAR)
#2018 2019
# 960  993
isotope <- isotope_org%>%
  select(c("SITE_ID","VISIT_NO","SAMPLE_ID","H2O_dD","H2O_d18O","d-excess"))
length(unique(isotope$SITE_ID))#1768

# Check for duplicates n=4 (2 each but have different isotope values)
# NRS18_MN_RF001,NRS18_MN_RF002
test<-isotope%>%
  group_by(SITE_ID,VISIT_NO)%>%
  filter(n()>1)

# WRITE REDUCED DATASETS TO NEW FOLDER TO MERGE
write_csv(site_org,"data_to_merge_1819/a_site.csv")
write_csv(benthic_oe,"data_to_merge_1819/b_benth_oe.csv")
write_csv(chem,"data_to_merge_1819/c_chem_.csv")
write_csv(phab,"data_to_merge_1819/e_phab.csv")

write_csv(phab_oe,"data_to_merge_1819/f_phab_oe.csv")
write_csv(wqii,"data_to_merge_1819/g_wqii.csv")
write_csv(isotope,"data_to_merge_1819/h_isotope.csv")

# Save in processed data folder
write_csv(strcat_tot,"data_processed/subset_streamcat1819.csv")


#########
## Use multi-merge function to bring data together
# Merging based on Site ID

nrsa<- multimerge("data_to_merge_1819")
names(nrsa)
#nrsa<-nrsa %>%
#  select(-c("X.x","X.y"))
length(unique(nrsa$SITE_ID)) #n=1919 out of 2364

# AGGREGATE ECOREGIONS
nrsa$AG_ECO5<-nrsa$AG_ECO9
nrsa<-nrsa %>%
  mutate(AG_ECO5 = case_when (
    (AG_ECO9 %in% c('WMT','XER'))~'West',
    (AG_ECO9 %in% c('NPL','SPL'))~'Great Plains',
    (AG_ECO9 %in% c('UMW','TPL'))~'Midwest',
    (AG_ECO9 %in% c('NAP','SAP'))~'Appalachians',
    (AG_ECO9 %in% c('CPL'))~'Coastal Plains'))
table(nrsa$AG_ECO5)

nrsa$AG_ECO5<-ordered(nrsa$AG_ECO5, levels=c("West", "Great Plains","Midwest",
                                             "Appalachians","Coastal Plains"))

####################
# Drop duplicate observations and retain first observation (even though isotope values are different)
# https://www.codecademy.com/courses/learn-dplyr/lessons/r-data-cleaning/exercises/duplicates#:~:text=To%20check%20for%20duplicates%2C%20we,which%20rows%20are%20duplicate%20rows.&text=We%20can%20see%20that%20the,calories%2C%20is%20a%20duplicate%20row.
# Retains first observation n = 2110 compared to n=2364
nrsa_proc= nrsa%>%
  distinct(SITE_ID,VISIT_NO, .keep_all=TRUE)

##################
# Merge processed NRSA and StreamCat datasets together using left_join
nrsa_strmcatv1<-left_join(nrsa_proc,strcat_tot,
                          by="UNIQUE_ID")

#############
## DATA PROCESSING
# Need to match survey climate vars to year of survey (originally in land_org dataset)

# SUBSET DATA BY YEAR
first<-nrsa_strmcatv1%>%
  filter(YEAR=="2018")

second<-nrsa_strmcatv1%>%
  filter(YEAR=="2019")

# Create column of survey specific climate and water temp vars that can be joined together
# 2018 n = 1043
first<-first%>%
  mutate(
    MAST_SY=MAST_2018,
    MSST_SY=MSST_2018,
    MWST_SY=MWST_2018,
    PSUMPY_SY_PT=PSUMPY_2018_PT,
    PSUMPY_SY_WS=PSUMPY_2018Ws,
    TMEAN_PWSY_PT=TMEAN_PW_2018_PT,
    TMEAN_PWSY_WS=TMEAN_PW_2018Ws,
    TMEAN_WSY_PT=TMEAN_W_2018_PT,
    TMEAN_WSY_WS=TMEAN_W_2018Ws,
    TMEAN_SSY_PT=TMEAN_S_2018_PT,
    TMEAN_SSY_WS=TMEAN_S_2018Ws,
    TMEAN_SY_PT=TMEAN_SY_2018_PT,
    TMEAN_SY_WS=TMEAN_SY_2018Ws)%>%
  select(-c(MAST_2018,MSST_2018,MWST_2018,PSUMPY_2018_PT,PSUMPY_2018Ws,
            TMEAN_PW_2018_PT,TMEAN_PW_2018Ws,TMEAN_W_2018_PT,TMEAN_W_2018Ws,
            TMEAN_S_2018_PT,TMEAN_S_2018Ws,TMEAN_SY_2018_PT,TMEAN_SY_2018Ws,
            MAST_2019,MSST_2019,MWST_2019,PSUMPY_2019_PT,PSUMPY_2019Ws,
            TMEAN_PW_2019_PT,TMEAN_PW_2019Ws,TMEAN_W_2019_PT,TMEAN_W_2019Ws,
            TMEAN_S_2019_PT,TMEAN_S_2019Ws,TMEAN_SY_2019_PT,TMEAN_SY_2019Ws))
names(first)


# 2019 n = 1067
second<-second%>%
  mutate(
    MAST_SY=MAST_2019,
    MSST_SY=MSST_2019,
    MWST_SY=MWST_2019,
    PSUMPY_SY_PT=PSUMPY_2019_PT,
    PSUMPY_SY_WS=PSUMPY_2019Ws,
    TMEAN_PWSY_PT=TMEAN_PW_2019_PT,
    TMEAN_PWSY_WS=TMEAN_PW_2019Ws,
    TMEAN_WSY_PT=TMEAN_W_2019_PT,
    TMEAN_WSY_WS=TMEAN_W_2019Ws,
    TMEAN_SSY_PT=TMEAN_S_2019_PT,
    TMEAN_SSY_WS=TMEAN_S_2019Ws,
    TMEAN_SY_PT=TMEAN_SY_2019_PT,
    TMEAN_SY_WS=TMEAN_SY_2019Ws)%>%
  select(-c(MAST_2018,MSST_2018,MWST_2018,PSUMPY_2018_PT,PSUMPY_2018Ws,
            TMEAN_PW_2018_PT,TMEAN_PW_2018Ws,TMEAN_W_2018_PT,TMEAN_W_2018Ws,
            TMEAN_S_2018_PT,TMEAN_S_2018Ws,TMEAN_SY_2018_PT,TMEAN_SY_2018Ws,
            MAST_2019,MSST_2019,MWST_2019,PSUMPY_2019_PT,PSUMPY_2019Ws,
            TMEAN_PW_2019_PT,TMEAN_PW_2019Ws,TMEAN_W_2019_PT,TMEAN_W_2019Ws,
            TMEAN_S_2019_PT,TMEAN_S_2019Ws,TMEAN_SY_2019_PT,TMEAN_SY_2019Ws))
names(second)

## COMBINE SUBSETS TOGETHER USING ROWBIND
nrsa_strmcat_proc<-bind_rows(first,second)
#n = 2110 w/345 variables
table(nrsa_strmcat_proc$VISIT_NO)
# 1    2    R
#1919  189    2

# EXPORE DATA AS .csv files
write_csv(nrsa_strmcat_proc,"data_processed/nrsa1819/nrsa1819_strmcat_all.csv")


################################
## PROCESS DATA TO BE ABLE TO MERGE WITH OTHER SURVEYS
## READ PROCESSED DATA n = 2110
nrsa_strmcat_proc<-read_csv("data_processed/nrsa1819/nrsa1819_strmcat_all.csv")

## # LANDCOVER/USE CLASSES
nrsa_strmcat_proc <-nrsa_strmcat_proc %>%
  mutate(PCTOW_WS=PctOw2019Ws,
         PCTICE_WS=PctIce2019Ws,
         PCTURBOP_WS=PctUrbOp2019Ws,
         PCTURBLO_WS=PctUrbLo2019Ws,
         PCTURBMD_WS=PctUrbMd2019Ws,
         PCTURBHI_WS=PctUrbHi2019Ws,
         PCTDECID_WS=PctDecid2019Ws,
         PCTCONIF_WS=PctConif2019Ws,
         PCTMXFST_WS=PctMxFst2019Ws,
         PCTSHRB_WS=PctShrb2019Ws,
         PCTGRS_WS=PctGrs2019Ws,
         PCTHAY_WS=PctHay2019Ws,
         PCTCROP_WS=PctCrop2019Ws,
         PCTWDWET_WS=PctWdWet2019Ws,
         PCTHBWET_WS=PctHbWet2019Ws,
         PCTOW_WsRp100=PctOw2019_WsRp100,
         PCTICE_WsRp100=PctIce2019_WsRp100,
         PCTURBOP_WsRp100=PctUrbOp2019_WsRp100,
         PCTURBLO_WsRp100=PctUrbLo2019_WsRp100,
         PCTURBMD_WsRp100=PctUrbMd2019_WsRp100,
         PCTURBHI_WsRp100=PctUrbHi2019_WsRp100,
         PCTDECID_WsRp100=PctDecid2019_WsRp100,
         PCTCONIF_WsRp100=PctConif2019_WsRp100,
         PCTMXFST_WsRp100=PctMxFst2019_WsRp100,
         PCTSHRB_WsRp100=PctShrb2019_WsRp100,
         PCTGRS_WsRp100=PctGrs2019_WsRp100,
         PCTHAY_WsRp100=PctHay2019_WsRp100,
         PCTCROP_WsRp100=PctCrop2019_WsRp100,
         PCTWDWET_WsRp100=PctWdWet2019_WsRp100,
         PCTHBWET_WsRp100=PctHbWet2019_WsRp100,
         PCTIMP_WS=PctImp2019Ws,
         PCTIMP_WsRp100=PctImp2019_RipBuf100WsRp100,
         PCTOW_CAT=PCTOW2019CAT,
         PCTICE_CAT=PCTICE2019CAT,
         PCTURBOP_CAT=PCTURBOP2019CAT,
         PCTURBLO_CAT=PCTURBLO2019CAT,
         PCTURBMD_CAT=PCTURBMD2019CAT,
         PCTURBHI_CAT=PCTURBHI2019CAT,
         PCTDECID_CAT=PCTDECID2019CAT,
         PCTCONIF_CAT=PCTCONIF2019CAT,
         PCTMXFST_CAT=PCTMXFST2019CAT,
         PCTSHRB_CAT=PCTSHRB2019CAT,
         PCTGRS_CAT=PCTGRS2019CAT,
         PCTHAY_CAT=PCTHAY2019CAT,
         PCTCROP_CAT=PCTCROP2019CAT,
         PCTWDWET_CAT=PCTWDWET2019CAT,
         PCTHBWET_CAT=PCTHBWET2019CAT,
         PCTOW_CATRP100=PCTOW2019CATRP100,
         PCTICE_CATRP100=PCTICE2019CATRP100,
         PCTURBOP_CATRP100=PCTURBOP2019CATRP100,
         PCTURBLO_CATRP100=PCTURBLO2019CATRP100,
         PCTURBMD_CATRP100=PCTURBMD2019CATRP100,
         PCTURBHI_CATRP100=PCTURBHI2019CATRP100,
         PCTDECID_CATRP100=PCTDECID2019CATRP100,
         PCTCONIF_CATRP100=PCTCONIF2019CATRP100,
         PCTMXFST_CATRP100=PCTMXFST2019CATRP100,
         PCTSHRB_CATRP100=PCTSHRB2019CATRP100,
         PCTGRS_CATRP100=PCTGRS2019CATRP100,
         PCTHAY_CATRP100=PCTHAY2019CATRP100,
         PCTCROP_CATRP100=PCTCROP2019CATRP100,
         PCTWDWET_CATRP100=PCTWDWET2019CATRP100,
         PCTHBWET_CATRP100=PCTHBWET2019CATRP100,
         PCTIMP_CAT=PCTIMP2019CAT,
         PCTIMP_CATRP100=PCTIMP2019CATRP100)%>%
  select(-c(PctOw2019Ws, PctIce2019Ws, PctUrbOp2019Ws, PctUrbLo2019Ws, PctUrbMd2019Ws, PctUrbHi2019Ws,
            PctDecid2019Ws, PctConif2019Ws, PctMxFst2019Ws, PctShrb2019Ws, PctGrs2019Ws,
            PctHay2019Ws, PctCrop2019Ws, PctWdWet2019Ws, PctHbWet2019Ws,
            PctOw2019_WsRp100, PctIce2019_WsRp100, PctUrbOp2019_WsRp100,
            PctUrbLo2019_WsRp100, PctUrbMd2019_WsRp100, PctUrbHi2019_WsRp100,
            PctDecid2019_WsRp100, PctConif2019_WsRp100, PctMxFst2019_WsRp100,
            PctShrb2019_WsRp100, PctGrs2019_WsRp100, PctHay2019_WsRp100, PctCrop2019_WsRp100,
            PctWdWet2019_WsRp100, PctHbWet2019_WsRp100, PctImp2019Ws,
            PctImp2019_RipBuf100WsRp100,
            PCTOW2019CAT,PCTICE2019CAT,PCTURBOP2019CAT,PCTURBLO2019CAT,PCTURBMD2019CAT,PCTURBHI2019CAT,
            PCTDECID2019CAT,PCTCONIF2019CAT,PCTMXFST2019CAT,PCTSHRB2019CAT,PCTGRS2019CAT,
            PCTHAY2019CAT,PCTCROP2019CAT,PCTWDWET2019CAT,PCTHBWET2019CAT,
            PCTOW2019CATRP100,PCTICE2019CATRP100,PCTURBOP2019CATRP100,PCTURBLO2019CATRP100,PCTURBMD2019CATRP100,PCTURBHI2019CATRP100,
            PCTDECID2019CATRP100,PCTCONIF2019CATRP100,PCTMXFST2019CATRP100,
            PCTSHRB2019CATRP100,PCTGRS2019CATRP100,
            PCTHAY2019CATRP100,PCTCROP2019CATRP100,
            PCTWDWET2019CATRP100,PCTHBWET2019CATRP100,
            PCTIMP2019CAT,PCTIMP2019CATRP100))


nrsa1819<-nrsa_strmcat_proc%>%
  select(c("UID","SITE_ID","VISIT_NO","DATE_COL","YEAR","SITETYPE","STATE_NM","AG_ECO3","AG_ECO9","AG_ECO5",
           "US_L3CODE","US_L4CODE","HUC8",
           "LAT_DD83","LON_DD83","PROTOCOL","REALM","STRAH_ORD",
           "OE_SCORE",
           "AMMONIA_N_RESULT","ANC_RESULT","CHLORIDE_RESULT","COLOR_RESULT","COND_RESULT","DOC_RESULT",
           "MAGNESIUM_RESULT","SODIUM_RESULT","POTASSIUM_RESULT","NITRATE_N_RESULT","NITRITE_N_RESULT",
           "NTL_RESULT","PTL_RESULT","SULFATE_RESULT","TSS_RESULT","TURB_RESULT",
           "RT_WQI","CL_pt","SO4_pt","PTL_pt","NTL_pt","TURB_pt","ENTERO_PT", "DO_PT","PH_PT","WQII",
           "H2O_dD","H2O_d18O","d.excess","MAST_SY","MSST_SY","MWST_SY",
           "LDCBF_G08",
           "XDEPTH_CM","SDDEPTH_CM","XWXD","RP100","XBKF_W","XBKF_H","XINC_H","SINU","REACHLEN",
           "LSUB_DMM","XEMBED","PCT_FN","PCT_SAFN","PCT_SFGF","LDMB_BW5","LRBS_BW5","LRBS_G08","PCT_FAST","PCT_SLOW",
           "RpRat","PCT_BDRK","XFC_ALG","XFC_AQM","XFC_LWD","XFC_NAT","V1W_MSQ","XCDENBK","XCDENMID",
           "XCL","XGB","XGW","XMW","XC","XCMGW","QR1","QRVeg1","RDIST1","W1_HALL","W1H_WALL","W1_HNOAG","W1_HAG",
           "W1H_CROP","XSLOPE_use","XWIDTH_use",
           "Lpt01_XCMGW","Lpt01_XFC_NAT","LRBS_use",
           "RDIST_COND","LRBS_Cond_use","LOE_RBS_use",
           "LXCMGW_Cond_use","LOE_XCMGW_use","LXFC_NAT_Cond_use","LOE_XFC_NAT_use",
           "Qbkf_cl","LQbkf_cl","Qbkf_kmcl","LQbkf_kmcl",
           "QLow_cl","LQLow_cl","QLow_kmcl","LQLow_kmcl",
           "LOE_QLow_cl","LOE_Qbkf_cl",
           "CatAreaSqKm","WsAreaSqKm","CatAreaSqKmRp100","WsAreaSqKmRp100",
           "PSUMPY_SY_PT","PSUMPY_SY_WS","TMEAN_PWSY_PT","TMEAN_PWSY_WS",
           "TMEAN_WSY_PT","TMEAN_WSY_WS","TMEAN_SSY_PT","TMEAN_SSY_WS",
           "TMEAN_SY_PT","TMEAN_SY_WS","PMAXWs","PMINWs","Precip8110Ws","Tmax8110Ws","Tmean8110Ws","Tmin8110Ws",
           "BFIWs","ElevWs","PermWs","RunoffWs","HydrlCondWs","WDMAX_PT","WDMIN_PT","WDSUM_PT",
           "PCTOW_WS", "PCTICE_WS", "PCTURBOP_WS", "PCTURBLO_WS", "PCTURBMD_WS", "PCTURBHI_WS",
           "PCTDECID_WS", "PCTCONIF_WS", "PCTMXFST_WS", "PCTSHRB_WS", "PCTGRS_WS", "PCTHAY_WS", "PCTCROP_WS",
           "PCTWDWET_WS", "PCTHBWET_WS", "PCTOW_WsRp100", "PCTICE_WsRp100", "PCTURBOP_WsRp100", "PCTURBLO_WsRp100",
           "PCTURBMD_WsRp100","PCTURBHI_WsRp100", "PCTDECID_WsRp100", "PCTCONIF_WsRp100", "PCTMXFST_WsRp100",
           "PCTSHRB_WsRp100", "PCTGRS_WsRp100", "PCTHAY_WsRp100", "PCTCROP_WsRp100",
           "PCTWDWET_WsRp100", "PCTHBWET_WsRp100", "PCTIMP_WS", "PCTIMP_WsRp100",
           "PCTAGDRAINAGEWS","PCTAGDRAINAGECAT",
           "PCTOW_CAT","PCTICE_CAT","PCTURBOP_CAT","PCTURBLO_CAT","PCTURBMD_CAT","PCTURBHI_CAT",
           "PCTDECID_CAT","PCTCONIF_CAT","PCTMXFST_CAT","PCTSHRB_CAT","PCTGRS_CAT",
           "PCTHAY_CAT","PCTCROP_CAT","PCTWDWET_CAT","PCTHBWET_CAT",
           "PCTOW_CATRP100","PCTICE_CATRP100","PCTURBOP_CATRP100","PCTURBLO_CATRP100","PCTURBMD_CATRP100","PCTURBHI_CATRP100",
           "PCTDECID_CATRP100","PCTCONIF_CATRP100","PCTMXFST_CATRP100","PCTSHRB_CATRP100","PCTGRS_CATRP100",
           "PCTHAY_CATRP100","PCTCROP_CATRP100","PCTWDWET_CATRP100","PCTHBWET_CATRP100",
           "PCTIMP_CAT","PCTIMP_CATRP100",
           "NABD_DensWs","NABD_NIDStorWs","NABD_NrmStorWs",
           "RdDensWs","RdDensWsRp100",
           "PopDen2010Ws","PopDen2010WsRp100",
           "AgKffactWs","FertWs","ManureWs","NPDESDensWs","NPDESDensWsRp100"))
#n = 2110 (visits 1 & 2) with 222 variables

#"RPRAT","QRVEG1","MMI_BENT","OE_SCORE_OLD"

#######################
#GET COLUMN NAMES TO CHECK
dat_names_share<-data.frame(colnames(nrsa1819))
write_csv(dat_names_share,"data_processed/nrsa1819/column_varsToCompile_1819.csv")

## WRITE TO CSV
write_csv(nrsa1819,"data_processed/nrsa1819/nrsa1819_to_merge.csv")

