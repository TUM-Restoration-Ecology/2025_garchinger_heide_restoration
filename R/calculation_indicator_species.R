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
library(lme4)
library(tidytable)

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

phi_sig <- subset(phi_taxa, p.value <= 0.01)


# # significant species names
# species_sig <- phi_sig$species
# 
# # likelihood
# species$id <- rownames(species)
# 
# species_long <- species %>%
#   pivot_longer(
#     cols = -id,
#     names_to = "species",
#     values_to = "cover") %>%
#   mutate(presence = ifelse(cover > 0, 1, 0)) %>%
#   merge(sites %>% select(id,treatment2), by = "id")
# 
# all_species <- unique(species_long$species)
# 
# 
# glm_species <- function(x) {
#   
#   df <- species_long %>% filter(species == x)
#   
#   m  <- glm(presence ~ treatment2, data = df,
#               family = binomial)
#   
#   pred <- ggeffects::ggeffect(m, terms = "treatment2")
#   pred$species <- x
#   
#   pred$likelihood <- logLik(m)
#   return(pred)
# }
# 
# pred_list <- map_df(all_species, glm_species)
# 
# ggplot(pred_list) +
#   geom_point(aes(x = likelihood, y = species)) +
#   theme_mb()
# 

