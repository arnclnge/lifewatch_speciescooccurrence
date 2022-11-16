library(tidyverse)
library(lubridate)
library(dplyr)
library(glatos) #R package for the Great Lakes Acoustic Telemetry
library(tibble)
library(readr)

setwd("~/Ari/CoOccur/")
plot_loc <- "~/Ari/CoOccur/plots/"

detections_merge <- read_csv("DPH_cod_sb.csv")
df_dol <- read_csv("DPH_dolphins.csv")
df_porpoise2 <- read_csv("DPH_porpoise.csv")

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
df = read_csv("DPH_final.csv")

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

#rename
df_long$Season <- recode(df_long$Season, "December Solstice" = "Winter", "September Equinox" = "Autumn", "March Equinox"="Spring", "June Solstice"="Summer")
df_long$location_col <- recode(df_long$location_col,"bpns-Belwindreefballs-CPOD"="bpns-Belwindreefballs","bpns-Cpowerreefballs-CPOD"="bpns-Cpowerreefballs")

df_long %>% filter(dph==1) %>% ggplot(aes(x = day_time, y = location_col, color = Season))+
  geom_point()+theme_bw()+theme(axis.title=element_blank(),strip.text.x = element_text(size = 15))+
  scale_color_manual(values = c("Winter" = "steelblue",
                                "Autumn"="deeppink",
                                "Summer"="darkorange",
                                "Spring" = "green"))+
  facet_wrap(~species,labeller = labeller(species = c("por_DPH" = "Harbour Porpoise (PAM)",
                                                      "sb_DPH" = "European Seabass (AT)",
                                                      "cod_DPH" = "Atlantic Cod (AT)",
                                                      "dol_DPH" = "Dolphins (PAM)")))
ggsave(paste0(plot_loc,"dph_year.png"), device='png', dpi=500, width=13, height=7)
