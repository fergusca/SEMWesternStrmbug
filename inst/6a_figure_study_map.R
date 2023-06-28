##########################
## STUDY EXTENT MAPS NRSA SEM Ecosphere Manuscript
##
## RESOURCES
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
###############

remove(list=ls())

library(ggplot2)
library(ggmap)
library(tidyverse)
#library(tidyr)

library(maps)
library(scales)
library(maptools)
library(ggspatial)

library(sf)
library(readr)

library("rnaturalearth")
library("rnaturalearthdata")

library(devtools)
devtools::load_all()
library(SEMWesternStrmbug)

###########
## Plot specifications
###########
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman"))
windowsFonts(AR=windowsFont("Arial"))

################
## LOAD DATA
# PROCESSED - all 0809 and only new sites from later surveys
#  VISITS 1 and 2 n = 4578
# Or load from R package- a .rda file
load(file="data/nrsa_proc.rda")

# PROCESSED DATA VISIT_NO=1 ONLY n = 4389
dat_proc<-nrsa_proc%>%
  filter(VISIT_NO==1) %>%
  filter(PROTOCOL=="BOATABLE"|PROTOCOL=="WADEABLE")

# SUBSET NRSA SITES: WMT + XER WADEABLE
nrsa_wmtxerw <- dat_proc %>%
  filter(AG_ECO9=="WMT"|AG_ECO9=="XER",PROTOCOL=="WADEABLE")

## READ SHAPEFILE -
#Aggregated Ecoregion 9
eco <-read_sf("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/GIS/ECOREG_9/ecoreg9_2015.shp")

eco<- st_transform(eco, 4269)

# Order ecoregions for gray color assignment
eco$WSA9<- ordered(eco$WSA9, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

# SUBSET WMT AND XER ECOREGIONS
eco_reduced <-eco %>%
  filter(WSA9 == "WMT"|WSA9=="XER") #filter(ECO_5 == "West"|ECO_5 == "Midwest")

##################
## GRAB WORLD DATA
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

############
# MAKE NRSA SITES SPATIAL OBJECT WMT + XER WADEABLE n = 626
sites_wmtxer <-st_as_sf(nrsa_wmtxerw, coords = c("LON_DD83","LAT_DD83"),
                        crs=4269)

#####################
## MAP WMT + XER WADEABLE
#####################

map_wmt_xer <-ggplot(data=world, color="gray90")+
  geom_sf(data=eco,fill="gray78", color="grey94")+ # fill=NA  aes(fill=factor(eco$WSA9))) +
  geom_sf(data=eco_reduced, aes(fill=factor(eco_reduced$WSA9))) +
  scale_fill_manual(values=c("#8c510a","#bf812d"))+
  geom_sf(data=sites_wmtxer, color="black",size =0.7)+ #geom_sf(data=sites, aes(colour=sites$HydrAP_f),size = .05)+
  coord_sf(xlim=c(-129, -65), ylim = c(24, 51), expand = FALSE)+
  theme_bw()+
  theme(plot.title = element_text(family = "AR",face="plain",size=14, hjust=0.5),
        #axis.ticks=element_blank(),
        axis.text.x = element_text(family = "AR", size=10),#element_blank(), # remove lat long values
        axis.text.y = element_text(family = "AR", size=10),#element_blank(),
        legend.text = element_text(family = "AR", size=12),
        legend.title= element_blank(),
        #panel.border=element_blank(),#
        #panel.grid=element_blank(),
        axis.title=element_blank(),
        legend.background = element_rect(fill = "gray90"), #lightgray
        legend.key = element_rect(fill = "gray90", color = NA),
        legend.position = "bottom")+
  ggtitle("")

# Adding north arrow and scale bar
# https://stackoverflow.com/questions/61809382/how-can-i-put-a-scalebar-and-a-north-arrow-on-the-map-ggplot
map_ms<-map_wmt_xer +
  ggspatial::annotation_scale(
    location= "bl",
    bar_cols = c("grey60","white"),
    text_family = "AR")+
  ggspatial::annotation_north_arrow(
    location="br", which_north="true",
    #pad_x = unit(0.4, "in"), pad_y = unit(0.4,"in"),
    style = ggspatial::north_arrow_minimal(
      text_family = "AR"
    )
  )

###########
## PRINT MAP: WMT + XER
tiff(filename="inst/Routput/Figures/Map_WMT_XER_north.tiff",
     width=7.5, height=5, units="in", res=600)
map_ms#map_wmt_xer
dev.off()
