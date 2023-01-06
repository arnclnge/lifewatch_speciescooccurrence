#Pairwise Species Monthly Occupancy

install.packages("EcoSimR")

library("cooccur")
library("textshape")
library("EcoSimR")
library("tidyverse")

df_merged = read_csv("DPH_final.csv")

#filtered for at least 10 hours per station
#run code for each month
df_cooccur <- df_merged %>%   filter(month == 12) %>% group_by(location_col) %>%
                              summarise(Seabass = if_else(sum(na.omit(sb_DPH)) >= 10, 1,0),
                                        Cod = if_else(sum(na.omit(cod_DPH)) >= 10, 1,0),
                                        HarbourPorpoise = if_else(sum(na.omit(por_DPH)) >= 10, 1,0),
                                        Dolphins = if_else(sum(na.omit(dol_DPH)) >= 10, 1,0))

df_cooccur <- column_to_rownames(df_cooccur, "location_col")
df_cooccur <- t(df_cooccur)

#---co-occur

cooccur_df <- cooccur(mat = df_cooccur, type = "spp_site", thresh = TRUE, spp_names = TRUE)
prob.table(cooccur_df)

summary(cooccur_df)
plot(cooccur_df)

data("finches")
cooccur.finches <- cooccur(mat = finches, type = "spp_site", thresh = TRUE, spp_names = TRUE)
plot(cooccur.finches)

#---ecosim

df_ecosim <- cooc_null_model(df_cooccur,suppressProg=TRUE)
summary(df_ecosim)

skewCScore <- c_score_skew(m=df_cooccur)
skewCScore <- c_score_skew(m = matrix(rbinom(100, 1, 0.5), nrow = 10))

plot(df_ecosim,type="cooc")

#---graph
library(tidyverse)
library(hrbrthemes)

cooccur_table <- read.csv("csv/cooccur_10hrfilter.csv", sep =";")

cooccur_table <- gather(cooccur_table, Month, Probability,JAN:DEC, factor_key=TRUE)
  
  # Viz
  ggplot(cooccur_table, aes(Month, Species_Pair, fill= Probability)) + 
  geom_tile() + scale_fill_gradient(low="yellow", high="blue") +
  theme_minimal() + theme(axis.title = element_blank())
  
  ggsave("plots/cooccur.png", device='png', dpi = 300, width=8, height=3)
  