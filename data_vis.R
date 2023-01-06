library(tidyverse)
library(lubridate)
library(dplyr)
library(glatos) #R package for the Great Lakes Acoustic Telemetry
library(tibble)
library(readr)
library(etn)

con <- connect_to_etn(Sys.getenv("userid"), Sys.getenv("pwd"))

setwd("~/lifewatch_speciescooccurrence/")
plot_loc <- "~/lifewatch_speciescooccurrence/plots/"

df <- read_csv("csv/DPH_final.csv")
detections_merge <- read_csv("csv/DPH_cod_sb.csv")
df_dol <- read_csv("csv/DPH_dolphins.csv")
df_porpoise2 <- read_csv("csv/DPH_porpoise.csv")

detect <- read_csv("csv/detect_cpod.csv")

#filter study period 3 Dec 2021
detect <- detect %>% filter("2021-12-03" >= date_time)

#recode station names
detect$station_name <- recode(detect$station_name,"bpns-Cpowerreefballs-CPOD"="Cpowerreefballs",
                                                  "bpns-Nauticaena" = "Nauticaena", "bpns-Grafton"="Grafton", "bpns-G-88" ="G-88",
                                                  "bpns-Faulbaums" = "Faulbaums", "bpns-Westhinder" = "Westhinder", "bpns-Buitenratel" = "Buitenratel",
                                                  "bpns-Fairplay" = "Fairplay", "bpns-Gardencity" = "Gardencity", "Belwindreefballs-CPOD"= "Belwindreefballs",
                                                  "bpns-Birkenfels" = "Birkenfels")

#########
#DOT PLOT
#########

df_plot = df %>% 
  select(
    location_col,
    Diurnal,
    Season,
    day_time,
    por_DPH,
    sb_DPH,
    dol_DPH,
    cod_DPH) 

#convert df from wide to long
df_long <- gather(as.data.frame(df_plot), species, dph,por_DPH:cod_DPH)
df_long$day_time <- as.Date(df_long$day_time)

#rename
df_long$Season <- recode(df_long$Season, "December Solstice" = "Winter", "September Equinox" = "Autumn", "March Equinox"="Spring", "June Solstice"="Summer")
df_long$location_col <- recode(df_long$location_col,"bpns-Belwindreefballs-CPOD"="bpns-Belwindreefballs","bpns-Cpowerreefballs-CPOD"="bpns-Cpowerreefballs")

df_long %>% filter(dph==1) %>% ggplot(aes(x = day_time, y = location_col, color = Season))+
  geom_point()+theme_bw()+theme(axis.title=element_blank(),strip.text.x = element_text(size = 15),axis.text.x=element_text(size =7, angle=20))+
  scale_color_manual(values = c("Winter" = "steelblue",
                                "Autumn"="deeppink",
                                "Summer"="darkorange",
                                "Spring" = "green"))+
  facet_wrap(~species,labeller = labeller(species = c("por_DPH" = "Harbour Porpoise (PAM)",
                                                      "sb_DPH" = "European Seabass (AT)",
                                                      "cod_DPH" = "Atlantic Cod (AT)",
                                                      "dol_DPH" = "Dolphins (PAM)")))+
  scale_x_date(date_labels = "%b-%Y",date_breaks = "3 months")

ggsave(paste0(plot_loc,"dph_year.png"), device='png', dpi=500, width=13, height=7)

#############
#MAP STATIONS
#############

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(rgdal)
library(broom)
library(maptools)
library(ggspatial)

world <- ne_countries(scale = "medium", returnclass = "sf")
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

stn_list <- read_csv("csv/stn_list.csv")
stn_list$location_col <- recode(stn_list$location_col,"bpns-Belwindreefballs-CPOD"="bpns-Belwindreefballs","bpns-Cpowerreefballs-CPOD"="bpns-Cpowerreefballs")

bpns <- readOGR( 
  dsn= "~/lifewatch_network_analysis/shp/belgium_eez/", 
  layer="eez",
  verbose=FALSE)
bpns_fortified <- tidy(bpns, region = "geoname")

ggplot(data=world) + geom_sf()+
  geom_polygon(data = bpns_fortified, aes(x = long, y = lat, group = group), fill="lightblue", alpha=0.75)+
  geom_point(data=stn_list, aes(x=longitude, y=latitude), size = 2, color="darkblue")+
  geom_text_repel(data=stn_list, aes(x=longitude, y=latitude, label=location_col), size=2.5, color = "darkblue")+
  coord_sf(xlim = c(2.1, 3.5), ylim = c(51.05,51.9), expand = FALSE)+theme_bw()+theme(axis.title = element_blank())+
  #geom_text(data= world_points,aes(x=X, y=Y, label=name),color = "darkblue", fontface = "bold", check_overlap = FALSE)+
  annotate(geom = "text", x = c(3, 4.3, 2.46), y = c(51.15, 51.75, 51.03), label = c("Be", "NL","FR"), size = 3) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave("plots/cpod_stn_map.png", device='png', dpi =300)

###################
#PLOT DATA ACTIVITY
###################

#---visualize data availability
df$location_col[df$location_col=="bpns-Cpowerreefballs-CPOD"] <- "bpns-Cpowerreefballs"
df$location_col[df$location_col=="bpns-Belwindreefballs-CPOD"] <- "bpns-Belwindreefballs"

df$location_col <- fct_relevel(df$location_col, rev)

df_activity <- df %>% group_by(location_col,day_time) %>% summarise(PAM = if_else((!is.na(por_DPH)|!is.na(dol_DPH)), 1,0),
                                                  AT = if_else((!is.na(sb_DPH)|!is.na(cod_DPH)), 1,0))

#convert to long
df_activity <- gather(df_activity, technique, activity, PAM:AT, factor_key=TRUE)

df_activity %>% 
  ggplot(aes(as.Date(day_time), location_col, colour = technique)) +
  geom_point(size = 0.7)  + scale_x_date(date_labels = "%Y")+ 
  ggtitle("AT & PAM Data Availability") +theme(axis.text.y=element_text(size=12), axis.text.x=element_text(size=9), axis.title=element_blank())

ggplot(df_activity, aes(x = as.Date(day_time), y = location_col)) +
  geom_point(
    aes(color = technique),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))

ggsave("plots/ATdata_availability.png", device='png', dpi =300)

#############################
#HEAT MAP of species detected
#############################

detect$date_hour <- format(detect$date_time,format='%Y-%m-%d %H')

unique_fish <- detect %>% group_by(station_name,scientific_name) %>% summarise(no_individuals = length(unique(animal_id)), DPH = length(unique(date_hour))) %>% 
  filter(scientific_name!="Built-in", station_name!="Fairplay") %>% rename("species"="scientific_name","station"= "station_name") %>% as.data.frame()

#add PAM data

load("cpod_df_20180701_20220801_week_hourly.Rdata")
cpod1_df <- as.data.frame(cpod1_df)
cpod1_df[,2] <- as.POSIXct(cpod1_df[,2], format = "%Y-%m-%d %H:%M:%S", tz="UTC")

cpod1_df$station <- recode(cpod1_df$station,"bpns-Reefballs Belwind"="Belwindreefballs","bpns-Reefballs-cpower"="Cpowerreefballs",
                           "bpns-Cpowerreefballs-CPOD"="Cpowerreefballs", "AP_bpns-Grafton" = "Grafton", 
                           "bpns-Nautica Ena" = "Nauticaena", "bpns-Grafton"="Grafton", "bpns-G88" ="G-88",
                           "bpns-Faulbaums" = "Faulbaums", "bpns-Westhinder" = "Westhinder", "bpns-Buitenratel" = "Buitenratel",
                           "bpns-Gardencity" = "Gardencity", "Belwindreefballs-CPOD"= "Belwindreefballs",
                           "bpns-Birkenfels" = "Birkenfels", "AP-bpns-Birkenfels"="Birkenfels", "AP-bpns-Belwind" = "Belwind", "AP-bpns-Cpower"="Cpowerreefballs")

unique_cpod <- cpod1_df %>% group_by(station,species)  %>% summarise(no_individuals = NA, DPH = sum(dph)) %>% filter(species!="sonar") %>% as.data.frame()
unique_cpod$species <- recode(unique_cpod$species, "NBHF"="Phocoena phocoena", "Dolphins" = "Delphinidae")

write_csv(unique_cet_cpod,"C:/Workspace_2MA1/Thesis/Fish/csv/unique_cet_BPAN.csv")

#organise labels
unique_fish$species <- fct_relevel(unique_fish$species, rev)

#merge 2 datasets
unique_animals <- rbind(unique_fish, unique_cpod)

ggplot(unique_animals, aes(station, species, fill= DPH)) + 
  geom_tile() + scale_fill_gradient(low="yellow", high="blue", trans="log1p", breaks = c(1000000, 100000,10000,1000,100,10)) +
  geom_text(aes(label = no_individuals))+
  theme_linedraw() + theme(axis.text.x=element_text(size = 9, angle = 90),axis.text.y = element_text(face="italic"),axis.title = element_blank())  

ggsave("plots/species_stations_heatmap.png", device='png', dpi = 300, width=13, height=7)
