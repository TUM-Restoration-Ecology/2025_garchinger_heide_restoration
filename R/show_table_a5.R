#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Management Garchinger Heide restoration sites
# Species occurences ####
# Show table A5
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-07-23



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d")))

### Load data ###
sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("", "na", "NA"), col_types = 
    cols(
      .default = "?",
      treatment = col_factor(
        levels = c(
          "control_2003", "control_2018", "control_2021", "cut_summer",
          "cut_autumn", "topsoil_removal"
          )
      )
    )
) %>%
  select(plot, treatment)

species <- read_csv(
  here("data", "processed", "data_processed_species.csv"),
  col_names = TRUE, na = c("", "na", "NA"), col_types = 
    cols(
      .default = "?"
    )
) %>%
  pivot_longer(-accepted_name, names_to = "plot", values_to = "cover") %>% 
  mutate(plot = str_remove(plot, "X[:digit:][:digit:][:digit:][:digit:]")) %>%
  filter(cover > 0)

data <- species %>%
  left_join(sites, by = "plot") %>%
  group_by(accepted_name, treatment) %>%
  filter(treatment %in% c("topsoil_removal")) %>%
  count()

data2 <- data %>%
  filter(accepted_name %in% c(
    "Aster amellus", "Aster linosyris", "Biscutella laevigata",
    "Centaurea triumfettii", "Globularia bisnagarica", "Pulsatilla patens",
    "Pulsatilla vulgaris", "Scabiosa canescens", "Seseli annuum",
    "Teucrium montanum", "Veronica spicata"
  ))



### Save ###
write_excel(data, here("outputs", "tables", "table_a2_habitat_types.xlsx"))
gtsave(table, here("outputs", "tables", "table_a2_habitat_types.html"))
gtsave(table, here("outputs", "tables", "table_a2_habitat_types.png"))
