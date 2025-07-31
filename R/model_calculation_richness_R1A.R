#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Management Garchinger Heide restoration sites
# Species richness EUNIS habitat type R1A ####
# Model building
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sina Appeltauer, Markus Bauer
# 2025-07-30



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
      treatment = "f"
    )
) %>%
  rename(y = richness_R1A)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics #################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Data exploration ##########################################################


### a Graphs of raw data -------------------------------------------------------

ggplot(sites, aes(y = y, x = treatment)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent")
ggplot(sites, aes(y = y, x = as_factor(year_hay_transfer))) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent")
ggplot(sites, aes(y = y, x = as.factor(mowing_date_start))) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent")


### b Outliers, zero-inflation, transformations? ----------------------------

sites %>% group_by(treatment) %>% count(treatment)
ggplot(sites, aes(x = treatment, y = y)) + geom_quasirandom()
ggplot(sites, aes(x = y)) + geom_histogram(binwidth = .1)
ggplot(sites, aes(x = y)) + geom_density()


### c Check collinearity ------------------------------------------------------

sites %>%
  select(height_vegetation, cover_vegetation) %>%
  GGally::ggpairs(lower = list(continuous = "smooth_loess")) +
  theme(strip.text = element_text(size = 7))
#--> exclude r > 0.7
# Dormann et al. 2013 Ecography
# https://doi.org/10.1111/j.1600-0587.2012.07348.x



## 2 Model building ###########################################################


### a Full dataset ------------------------------------------------------------

m1 <- lm(y ~ treatment + year_hay_transfer, data = sites)
simulateResiduals(m1, plot = TRUE)

m2 <- lm(y ~ treatment + year_hay_transfer + mem2, data = sites)
simulateResiduals(m2, plot = TRUE)


### Save ####

save(m1, file = here("outputs", "models", "model_richness_r1a_1.Rdata"))
save(m2, file = here("outputs", "models", "model_richness_r1a_2.Rdata"))


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

m1sub <- lm(y ~ treatment + year_hay_transfer, data = subset)
simulateResiduals(m1, plot = TRUE)

m2sub <- lm(y ~ treatment + year_hay_transfer + mem2, data = subset)
simulateResiduals(m2, plot = TRUE)


### Save ####

save(m1sub, file = here("outputs", "models", "model_richness_r1a_1sub.Rdata"))
save(m2sub, file = here("outputs", "models", "model_richness_r1a_2sub.Rdata"))
