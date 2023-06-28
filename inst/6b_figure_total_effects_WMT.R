##################
## BARCHARTS OF TOTAL EFFECTS OF PREDICTORS
## RESPONSE WMT & XER ECO9: OE, MMI, EPT
## WADEABLE SITES
## Model v15
## Automated method taking R output and getting total effects rather than manipulating data in excel
###################

remove(list=ls())

library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(GGally)
library(ggpubr)
#library(tidyr)
library(stringr)

library(devtools)
devtools::load_all()
library(SEMWesternStrmbug)

###################
## PLOTTING SPECIFICATIONS
#####################
## Set font for plot
#windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman
#windowsFonts(CAL=windowsFont("Calibri"))
windowsFonts(AR=windowsFont("Arial"))

# SET COLOR PALATE FOR BY DRIVER CATEGORY

# Viridis Color palette
# https://waldyrious.net/viridis-palette-generator/
vid <- c("#440154","#443983","#31688e","#440154","#35b779","#90d743","#fde725")

#############
## READ IN MODEL OUTPUT MODEL 15 - WMT
# SEM Standardized OUTPUT
## OE
oe<-read.csv("inst/Routput/WMTw_m15_OE_CI.csv")

## MMI
mmi<- read.csv("inst/Routput/WMTw_m15_MMI_CI.csv")

## EPT
ept<- read.csv("inst/Routput/WMTw_m15_EPT_CI.csv")

####################
# Call function sem_eff_tab to process the raw SEM output
# Selects Total, Direct, and Indirect Effects on OE
# And relabels predictor variables to more useful names
# O/E
oe_proc<- sem_eff_tab(oe)%>%
  mutate(Model="OE")
names(oe_proc)

# MMI
mmi_proc<-sem_eff_tab(mmi)%>%
  mutate(Model="MMI")

# EPT
ept_proc<-sem_eff_tab(ept)%>%
  mutate(Model="EPT")

#############################
# Combine datasets
teff<- bind_rows(oe_proc,mmi_proc,ept_proc)

names(teff)
table(teff$Predictor,teff$Model)

# ORDER PREDICTOR VARIABLES
teff$Predictor <- ordered(teff$Predictor, levels=c("Agriculture Ws","Developed Ws","Dam","Precipitation","Max Temp","Drought index",
                                                   "Bankfull flow/km2","Summer flow/km2","Evaporation indicator","Slope*depth","Specific stream power",
                                                   "Ag index Rp-S","Non-ag index Rp-S",
                                                   "Forest Rp","Forest/Grass Rp-W","Wetland Rp-W","Site-riparian cover index","Instream cover",
                                                   "Relative bed stability","TP","TN","Sulfate","Turbidity")) #"Instream cover","agr","SO4"
table(teff$Predictor)


# CREATE CATEGORIES
teff$Category <-teff$Predictor
teff <- teff %>%
  mutate(Category = recode_factor(Category,
                                  "Agriculture Ws"="Land use","Developed Ws"="Land use","Dam"="Land use",
                                  "Max Temp"="Climate","Precipitation"="Climate","Drought index"="Climate",
                                  "Slope*depth"="Morphometry","Specific stream power"="Hydromorphology",
                                  "Forest Rp"="Riparian cover",  "Wetland Rp-W"="Riparian cover","Forest/Grass Rp-W"="Riparian cover",
                                  "Bankfull flow/km2"="Hydromorphology","Summer flow/km2"="Hydromorphology",
                                  "Evaporation indicator"="Hydromorphology", "Site-riparian cover index"="Riparian cover", "Relative bed stability"="Substrate","Instream cover"="Substrate",
                                  "Ag index Rp-S" = "Riparian land use", "Non-ag index Rp-S" = "Riparian land use",
                                  "TN"="Chemistry","TP"="Chemistry","Sulfate"="Chemistry","Turbidity"="Chemistry")) #

teff$Category <- ordered(teff$Category, levels=c("Land use","Climate","Hydromorphology","Riparian land use","Riparian cover","Substrate","Chemistry")) #"Hydrology","Morphometry"
table(teff$Category)


# ORDER EFFECT
teff$Effects <- ordered(teff$Effects, levels=c("Total","Direct","Indirect"))
table(teff$Effects)

# ORDER BY RESPONSE AND DRIVER CLASS
teff$model<-ordered(teff$Model, levels=c("OE","MMI","EPT"))


######################
## FIGURE OF PROPORTION OF TOTAL EFFECTS ONLY
## SUBSET DATA TO HAVE ONLY TOTAL EFFECTS
teff_total <- teff %>%
  filter(Effects == "Total")

#####################
# Process to be able to create background guidelines in plot
teff_total2 <- teff_total%>%
  mutate(Predictor=factor(Predictor,exclude="Agriculture Ws"))#%>%
#  mutate(Predictor=recode_factor(Predictor,Turbidity = NA_character_))
table(teff_total2$Predictor)

############################
## FIGURE WITH THREE PANELS: OE, MMI, EPT
#########################
## TOTAL EFFECTS OE
total<-ggplot(teff_total2,aes(Predictor,est.std,fill=Category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid, drop=TRUE)+
  #scale_fill_manual(values=cbPalette, drop=TRUE)+
  geom_rect(aes(xmin=as.integer(Predictor)-0.5,
                xmax=as.integer(Predictor)+0.5,
                ymin=-Inf,ymax=Inf),# ...,color=Category),linewidth=0.3,fill=NA,#fill=alpha("grey",0),
            alpha=0.2)+
  #geom_vline(xintercept=c())
  ylim(-0.55,0.55)+
  facet_wrap(~model, ncol=1)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "AR",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "AR", angle=45, hjust=1,size=12,
                                   colour=c(rep("#440154",2),rep("#443983",2), rep("#31688e",4),rep("#440154",1), #teal rep("#21918c",4),
                                            rep("#35b779",3),rep("#90d743",1),rep("#fde725",2))),#, colour=c(rep("#8c510a",3),rep("#bf812d",1),rep("#dfc27d",3),rep("#c7eae5",2),rep("#80cdc1",1),rep("#c7eae5",1),rep("#35978f",2),rep("#01665e",1))),
        axis.text.y = element_text(family = "AR", size=12),
        axis.title.y = element_text(family="AR"), #element_blank(),#
        strip.text.x = element_text(family="AR", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="AR", size=11))+
  ylab("Total effects")+
  xlab(NULL)+
  ggtitle("WMT")


total

###################
# PRINT TOTAL EFFECTS
tiff(filename="inst/Routput/Figures/Tot_eff_ECO9_WMTw_m15_OE_MMI_EPT_rev.tiff",
     width=6, height = 8, units="in", res=300)
total
dev.off()

#################
## WRITE PROCESSED TOTAL EFFECTS DF
# WMT
write.csv(teff_total2,"inst/Routput/a_WMT_toteffects_m15.csv",
          row.names=FALSE)

############################
####################
## XERIC TOTAL EFFECTS
#####################
vid_x <- c("#440154","#443983","#31688e","#440154","#35b779","#90d743","#fde725")#"#31688e",
#############
## READ IN MODEL OUTPUT MODEL 15 - XER
# SEM Standardized OUTPUT
## OE
oe_x<-read.csv("inst/Routput/XERw_m15_OE_CI.csv")

## MMI
mmi_x<- read.csv("inst/Routput/XERw_m15_MMI_CI.csv")

## EPT
ept_x<- read.csv("inst/Routput/XERw_m15_EPT_CI.csv")

##################
# Call function sem_eff_tab to process the raw SEM output
# Selects Total, Direct, and Indirect Effects on OE
# And relabels predictor variables to more useful names
# O/E
oe_proc_x<- sem_eff_tab(oe_x)%>%
  mutate(Model="OE")
names(oe_proc_x)

# MMI
mmi_proc_x<-sem_eff_tab(mmi_x)%>%
  mutate(Model="MMI")

# EPT
ept_proc_x<-sem_eff_tab(ept_x)%>%
  mutate(Model="EPT")

#############################
# Combine datasets
teff_x<- bind_rows(oe_proc_x,mmi_proc_x,ept_proc_x)

names(teff_x)
table(teff_x$Predictor,teff_x$Model)

# ORDER PREDICTOR VARIABLES
teff_x$Predictor <- ordered(teff_x$Predictor, levels=c("Agriculture Ws","Developed Ws","Dam","Precipitation","Max Temp","Drought index",
                                                       "Bankfull flow/km2","Summer flow/km2","Evaporation indicator","Slope*depth","Specific stream power",
                                                       "Ag index Rp-S","Non-ag index Rp-S",
                                                       "Forest Rp","Forest/Grass Rp-W","Wetland Rp-W","Site-riparian cover index","Instream cover",
                                                       "Relative bed stability","TP","TN","Sulfate","Turbidity")) #"Instream cover","agr","SO4"
table(teff_x$Predictor)


# CREATE CATEGORIES
teff_x$Category <-teff_x$Predictor
teff_x <- teff_x %>%
  mutate(Category = recode_factor(Category,
                                  "Agriculture Ws"="Land use","Developed Ws"="Land use","Dam"="Land use",
                                  "Max Temp"="Climate","Precipitation"="Climate","Drought index"="Climate",
                                  "Slope*depth"="Morphometry","Specific stream power"="Hydromorphology",
                                  "Forest Rp"="Riparian cover",  "Wetland Rp-W"="Riparian cover","Forest/Grass Rp-W"="Riparian cover",
                                  "Bankfull flow/km2"="Hydromorphology","Summer flow/km2"="Hydromorphology",
                                  "Evaporation indicator"="Hydromorphology", "Site-riparian cover index"="Riparian cover", "Relative bed stability"="Substrate","Instream cover"="Substrate",
                                  "Ag index Rp-S" = "Riparian land use", "Non-ag index Rp-S" = "Riparian land use",
                                  "TN"="Chemistry","TP"="Chemistry","Sulfate"="Chemistry","Turbidity"="Chemistry")) #

teff_x$Category <- ordered(teff_x$Category, levels=c("Land use","Climate","Hydromorphology","Riparian land use","Riparian cover","Substrate","Chemistry"))
table(teff_x$Category)


# ORDER EFFECT
teff_x$Effects <- ordered(teff_x$Effects, levels=c("Total","Direct","Indirect"))
table(teff_x$Effects)

# ORDER BY RESPONSE AND DRIVER CLASS
teff_x$model<-ordered(teff_x$Model, levels=c("OE","MMI","EPT"))


######################
## FIGURE OF PROPORTION OF TOTAL EFFECTS ONLY
## SUBSET DATA TO HAVE ONLY TOTAL EFFECTS
teff_total_x <- teff_x %>%
  filter(Effects == "Total")

#####################
# Process to be able to create background guidelines in plot
#https://community.rstudio.com/t/geom-rect-with-factor-variable/100850/2
#https://stackoverflow.com/questions/50339909/shade-background-of-a-ggplot-chart-using-geom-rect-with-categorical-variables
#https://stackoverflow.com/questions/68961237/how-to-remove-a-specific-factor-level-to-missing-value-in-r
teff_total_x2 <- teff_total_x%>%
  mutate(Predictor=factor(Predictor,exclude="Max Temp"))#%>%
#mutate(Predictor=recode_factor(Predictor,Turbidity = NA_character_))
table(teff_total_x2$Predictor)

head(teff_total_x2)

############################
## FIGURE WITH THREE PANELS: OE, MMI, EPT
#########################
## TOTAL EFFECTS
total_x<-ggplot(teff_total_x2,aes(Predictor,est.std,fill=Category)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+
  scale_fill_manual(values=vid_x, drop=TRUE)+
  geom_rect(aes(xmin=as.integer(Predictor)-0.5,
                xmax=as.integer(Predictor)+0.5,
                ymin=-Inf,ymax=Inf),
            alpha=0.2)+
  ylim(-0.55,0.55)+
  facet_wrap(~model, ncol=1)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "AR",face="plain",size=14, hjust=0.5),
        axis.text.x = element_text(family = "AR", angle=45, hjust=1,size=12,
                                   colour= c(rep("#440154",3), rep("#443983",2),rep("#31688e",4),rep("#440154",2), # teal rep("#21918c",1),
                                             rep("#35b779",3),rep("#90d743",1),rep("#fde725",2))),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),#element_text(family="AR"), #
        strip.text.x = element_text(family="AR", size=12),
        panel.grid.major =  element_line(colour = NA),
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "none",#"bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="AR", size=11))+
  ylab("Total effects")+
  xlab(NULL)+
  ggtitle("XER")


total_x

#################
## WRITE PROCESSED TOTAL EFFECTS DF
# XER
#################
## WRITE PROCESSED TOTAL EFFECTS DF
write.csv(teff_total_x2,"inst/Routput/a_XER_toteffects_m15.csv",
          row.names=FALSE)


#########################
## COMBINE WMT AND XER - run XER Fig code
# GET LEGEND
legend <- get_legend(total)

# REMOVE LEGEND
total <- total + theme(legend.position="none")

# ARRANGE MULTIPLE GRAPHS AND LEGEND
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
tiff(filename="inst/Routput/Figures/Tot_eff_ECO9_WMTXERw_m15_OE_MMI_EPT_rev.tiff",
     width=8, height=8, units="in", res=600)
grid.arrange(arrangeGrob(total,
                         total_x,
                         ncol=2,widths=c(4,3.5)),
             legend,nrow=2,heights=c(8, .5))
dev.off()
