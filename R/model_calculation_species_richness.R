#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Management Garchinger Heide restoration sites
# Species richness ####
# Model building
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sina Appeltauer, Markus Bauer
# 2025-12-03



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
      year_hay_transfer = "f",
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
ggplot(sites, aes(y = y, x = treatment_age)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent")
ggplot(sites, aes(y = y, x = as_factor(year_hay_transfer))) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent")
ggplot(sites, aes(y = y, x = as.factor(mowing_date_start))) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent")


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


### a Full dataset ------------------------------------------------------------

m1 <- lm(y ~ treatment_age, data = sites)
simulateResiduals(m1, plot = TRUE)

m2 <- lm(y ~ treatment_age + mem2, data = sites)
simulateResiduals(m2, plot = TRUE)


### Save ####

save(m1, file = here("outputs", "models", "model_species_richness_1.Rdata"))
save(m2, file = here("outputs", "models", "model_species_richness_2.Rdata"))


### b Subset of data -----------------------------------------------------------

subset <- sites %>%
  mutate(
    hay_and_mowing_date = str_c(
      year_hay_transfer, mowing_date, sep = "_"
    )
  ) %>%
  filter(
    is.na(hay_and_mowing_date) | !(hay_and_mowing_date %in% c("1993_summer")),
    is.na(year_topsoil_removal) | year_topsoil_removal != "1996/2003"
  )

m1sub <- lm(y ~ treatment, data = subset)
simulateResiduals(m1, plot = TRUE)

m2sub <- lm(y ~ treatment + mem2, data = subset)
simulateResiduals(m2, plot = TRUE)


### Save ####

save(m1sub, file = here("outputs", "models", "model_species_richness_1sub.Rdata"))
save(m2sub, file = here("outputs", "models", "model_species_richness_2sub.Rdata"))
