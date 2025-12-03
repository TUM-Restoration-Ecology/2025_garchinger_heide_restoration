#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Garchinger Heide
# Check mowing ####
# Show figure ?
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sina Appeltauer, Markus Bauer
# 2025-11-27



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(here)
library(tidyverse)
library(ggplot2)


### Start ###
rm(list = ls())

### Functions ###
theme_mb <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 9, color = "black"),
    strip.text = element_text(size = 10),
    axis.text = element_text(angle = 0, hjust = 0.5, size = 9,
                             color = "black"),
    axis.title = element_text(angle = 0, hjust = 0.5, size = 9,
                              color = "black"),
    axis.line = element_line(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    legend.text = element_text(size = 10),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

#### * Load data sites ####

sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types =
    cols(
      .default = "?"
    )) %>% 
  arrange(id) %>%
  filter(treatment %in% c("cut_summer", "cut_autumn"))

sites$mowing_start_decades <- ifelse(sites$mowing_date_start < 2015, 
                                     "> 10 years", "< 10 years")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Boxplots ###################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



ggplot(sites, aes(x = treatment, y = richness_total, fill = mowing_start_decades))+
  geom_boxplot(alpha = .5) +
  geom_jitter(
    aes(col = mowing_start_decades), position = position_jitterdodge()
  ) +
  theme_mb()#

ggplot(sites, aes(x = treatment, y = richness_R1A, fill = mowing_start_decades))+
  geom_boxplot(alpha = .5) +
  geom_jitter(
    aes(col = mowing_start_decades), position = position_jitterdodge()
    ) +
  theme_mb()#

ggplot(sites, aes(x = treatment, y = richness_R22, fill = mowing_start_decades))+
  geom_boxplot(alpha = .5) +
  geom_jitter(
    aes(col = mowing_start_decades), position = position_jitterdodge()
  ) +
  theme_mb()#

ggplot(sites, aes(x = treatment, y = rlg, fill = mowing_start_decades))+
  geom_boxplot(alpha = .5) +
  geom_jitter(aes(col = mowing_start_decades), width = 0.1) +
  theme_mb()

ggplot(sites, aes(x = treatment, y = CWM_Height, fill = mowing_start_decades))+
  geom_boxplot(alpha = .5) +
  geom_jitter(
    aes(col = mowing_start_decades), position = position_jitterdodge()
  ) +
  theme_mb()

ggplot(sites, aes(x = treatment, y = CWM_Seed, fill = mowing_start_decades))+
  geom_boxplot(alpha = .5) +
  geom_jitter(
    aes(col = mowing_start_decades), position = position_jitterdodge()
  ) +
  theme_mb()

ggplot(sites, aes(x = treatment, y = CWM_SLA, fill = mowing_start_decades))+
  geom_boxplot(alpha = .5) +
  geom_jitter(
    aes(col = mowing_start_decades), position = position_jitterdodge()
  ) +
  theme_mb()
