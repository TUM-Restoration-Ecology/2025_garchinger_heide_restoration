#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Garchinger Heide
# Indicator species ####
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
library(indicspecies)
library(ggplot2)
# library(gt)

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
  arrange(id)

species <- read_csv(
  here("data", "processed", "data_processed_species.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(.default = "?")
) %>%
  pivot_longer(-accepted_name, names_to = "id", values_to = "value") %>%
  semi_join(sites, by = "id") %>%
  arrange(id) %>%
  pivot_wider(names_from = "accepted_name", values_from = "value") %>%
  column_to_rownames(var = "id")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Indicator species ##########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
species_wide <- species

sites$treatment2 <- ifelse(sites$treatment %in% c("control_2003", "control_2018", 
                                                  "control_2021"),
                                 "control", sites$treatment)
# significant indicator species
phi_taxa <- multipatt(species_wide, sites$treatment2,
                      func = "r.g", duleg = TRUE,
                      control = how(nperm = 999))


summary(phi_taxa)
phi_taxa <- as.data.frame(phi_taxa$sign)

phi_taxa$species <- rownames(phi_taxa)

phi_sig <- subset(phi_taxa, p.value <= 0.001)

# Create table

phi_sig <- phi_sig %>%
  mutate(index = as.character(index)) %>%
  mutate(index = recode (index,
                         "1" = "Reference",
                         "2" = "Mowing autumn",
                         "3" = "Mowing summer",
                         "4" = "Topsoil removal")) %>%
  rename(sites = index) %>%
  select(-c("s.control", "s.cut_autumn", "s.cut_summer", "s.topsoil_removal")) %>%
  mutate(stat = round(stat,3)) %>%
  select(species, everything()) %>%
  mutate(sites = factor(sites, levels = c("Reference", "Mowing summer", 
                                          "Mowing autumn", "Topsoil removal"))) %>%
  arrange(sites, species)


# Save processed data

write_csv(
  phi_sig,
  here("data", "processed", "data_indicator_species.csv")
)