#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Management Garchinger Heide restoration sites
# Specific leaf area (SLA) ####
# Model building
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-03-03



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(ade4)

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
  rename(y = CWM_SLA)

sites_fc <- sites %>%
  select(
    id, treatment, grass_cover, graminoid_cover
    ) %>% 
  column_to_rownames(var = "id")

traits <- read_csv(
  here("data", "processed", "data_processed_traits.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types =
    cols(
      .default = "?"
    )
) %>%
  mutate(log_y = log(sla)) %>%
  column_to_rownames(var = "accepted_name") %>% 
  select(log_y) %>%
  drop_na() %>%
  rownames_to_column(var = "accepted_name")

species <- read_csv(
  here("data", "processed", "data_processed_species.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types =
    cols(
      .default = "?"
    )
) %>%
  semi_join(traits, by = "accepted_name") %>%
  pivot_longer(-accepted_name, names_to = "id_plot", values_to = "cover") %>%
  pivot_wider(names_from = "accepted_name", values_from = "cover") %>%
  column_to_rownames(var = "id_plot")

traits <- traits %>%
  column_to_rownames(var = "accepted_name")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics #################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Data exploration ##########################################################


ggplot(sites, aes(y = y, x = treatment)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent")

sites %>% group_by(treatment) %>% count(treatment)



## 2 Model building ###########################################################


### a Full dataset ------------------------------------------------------------

m <- ade4::fourthcorner(
  tabR = sites_fc, tabL = species, tabQ = traits, modeltype = 6,
  nrepet = 999
)
m
summary(m)


### b Subset of data -----------------------------------------------------------

subset_sites <- sites %>%
  mutate(
    hay_and_mowing_date = str_c(
      year_hay_transfer, mowing_date, sep = "_"
    )
  ) %>%
  filter(
    is.na(hay_and_mowing_date) | !(hay_and_mowing_date %in% c("1993_summer")),
    is.na(year_topsoil_removal) | year_topsoil_removal != "1996/2003"
  )

subset_sites_fc <- subset_sites %>%
  select(
    id, treatment, grass_cover, graminoid_cover
  ) %>% 
  column_to_rownames(var = "id")

subset_species <- species %>%
  rownames_to_column(var = "id") %>%
  pivot_longer(-id, names_to = "accepted_name", values_to = "cover") %>%
  semi_join(subset_sites, by = "id") %>% 
  pivot_wider(names_from = "accepted_name", values_from = "cover") %>%
  column_to_rownames(var = "id")

m_sub <- ade4::fourthcorner(
  tabR = subset_sites_fc, tabL = subset_species, tabQ = traits, modeltype = 6,
  nrepet = 999
)
m_sub
summary(m_sub)
