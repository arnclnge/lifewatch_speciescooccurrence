library(tidyverse)
library(lubridate)
library(dplyr)
library(glatos) #R package for the Great Lakes Acoustic Telemetry
library(tibble)
library(readr)

setwd("~/lifewatch_speciescooccurrence/")
plot_loc <- "~/lifewatch_speciescooccurrence/plots/"

detections_merge <- read_csv("csv/DPH_cod_sb.csv")
df_dol <- read_csv("csv/DPH_dolphins.csv")
df_porpoise2 <- read_csv("csv/DPH_porpoise.csv")

#rename time column
colnames(df_dol)[2] <- "detection_timestamp_utc"

#convert time to POSIXct
df_dol <- as.data.frame(df_dol)
df_dol[,2] <- as.POSIXct(df_dol[,2], format = "%Y-%m-%d %H:%M:%S", tz="UTC")

#ABACUS PLOT
abacusPlot(df_dol[df_dol$dol_DPH >0,], detColNames = list(locationCol = "location_col",timestampCol = "detection_timestamp_utc"), 
           col="blue",plotTitle = "DPH per station per species",
           outFile =paste0(plot_loc,"dol_detections.png"),
           x_res = "year",x_format = "%b-%Y", Ylab="")

#DOT PLOT
df = read_csv("csv/DPH_final.csv")

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

#MAP STATIONS

stn_list %>% 
  ggplot(aes(x=longitude, y=latitude)) +
  geom_polygon(data = bpns_fortified, aes(x = long, y = lat, group = group), fill="grey", alpha=3) +
  geom_point(aes(x=longitude, y=latitude), size = 3)+
  geom_text_repel(data=stn_list, aes(x=longitude, y=latitude, label=location_col), size=3.5)

ggsave(paste0(plot_loc,"DPH_cod_summer2.png"), device='png')

#PLOT DATA ACTIVITY
#---visualize data availability
df_merged$location_col[df_merged$location_col=="bpns-Cpowerreefballs-CPOD"] <- "bpns-Cpowerreefballs"
df_merged$location_col[df_merged$location_col=="bpns-Belwindreefballs-CPOD"] <- "bpns-Belwindreefballs"

df_merged %>% as.data.frame() %>% group_by(location_col,day_time) %>% summarise(detections=n()) %>% 
  ggplot(aes(as.Date(day_time), location_col)) +
  geom_point(size = 0.4)  + scale_x_date(date_labels = "%b-%Y")+
  ggtitle("C-POD Network AT Data Availability") +  theme_bw()+theme(axis.text.y=element_text(size=12), axis.text.x=element_text(size=9), axis.title=element_blank())

