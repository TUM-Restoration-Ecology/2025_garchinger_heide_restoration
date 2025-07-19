#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Management Garchinger Heide restoration sites
# Species richness ####
# Model building
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sina Appeltauer, Markus Bauer
# 2025-07-15



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(patchwork)
library(DHARMa)

### Start ###
rm(list = ls())

### Load data ###
sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types =
    cols(
      .default = "?",
      treatment = col_factor(
        levels = c("control_2003", "control_2018", "control_2021", "cut_summer",
                   "cut_autumn", "topsoil_removal")
      )
    )
  ) %>%
  rename(y = richness_total)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics #################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Data exploration ##########################################################


### a Graphs of raw data -------------------------------------------------------

ggplot(sites, aes(y = y, x = treatment)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent")
ggplot(sites, aes(y = y, x = height_vegetation)) +
  geom_quasirandom(color = "grey") + geom_smooth(method = "lm")
ggplot(sites, aes(y = y, x = cover_vegetation)) +
  geom_quasirandom(color = "grey") + geom_smooth(method = "lm")
ggplot(sites, aes(y = y, x = grass_cover)) +
  geom_quasirandom(color = "grey") + geom_smooth(method = "lm")
ggplot(sites, aes(y = y, x = graminoid_cover)) +
  geom_quasirandom(color = "grey") + geom_smooth(method = "lm")
sites %>%
  filter(!(is.na(block))) %>%
  ggplot(aes(y = y, x = block)) +
  geom_quasirandom(color = "grey") + geom_boxplot()
sites %>%
  filter(!(is.na(hay_transfer) | hay_transfer == "no")) %>%
  ggplot(aes(y = y, x = as.numeric(hay_transfer), color = mowing_date)) +
  geom_point()
sites %>%
  filter(!(is.na(hay_transfer) | hay_transfer == "no")) %>%
  ggplot(aes(y = y, x = mowing_date_start, color = mowing_date)) +
  geom_point()


### b Outliers, zero-inflation, transformations? -------------------------------

sites %>% group_by(treatment) %>% count(treatment)
ggplot(sites, aes(x = treatment, y = y)) + geom_quasirandom()
ggplot(sites, aes(x = y)) + geom_histogram(binwidth = 1)
ggplot(sites, aes(x = y)) + geom_density()


### c Check collinearity ------------------------------------------------------

sites %>%
  select(
    cover_vegetation, height_vegetation, grass_cover, graminoid_cover, mem1,
    mem2
    ) %>%
  GGally::ggpairs(lower = list(continuous = "smooth_loess")) +
  theme(strip.text = element_text(size = 7))
#--> exclude r > 0.7
# Dormann et al. 2013 Ecography
# https://doi.org/10.1111/j.1600-0587.2012.07348.x


## 2 Model building ###########################################################


m1 <- lm(y ~ treatment, data = sites)
simulateResiduals(m1, plot = TRUE)

m2 <- lm(y ~ treatment + mem2, data = sites)
simulateResiduals(m2, plot = TRUE)


### Save ####


save(m1, file = here("outputs", "models", "model_species_richness_1.Rdata"))
save(m2, file = here("outputs", "models", "model_species_richness_2.Rdata"))
