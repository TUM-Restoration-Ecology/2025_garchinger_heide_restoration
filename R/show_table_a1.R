#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Management Garchinger Heide restoration sites
# EUNIS habitat types ####
# Show table A1
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-07-22



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(gt)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d")))

### Load data ###
data <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("", "na", "NA"), col_types = 
    cols(
      .default = "?",
      treatment = col_factor(
        levels = c("control", "cut_summer", "cut_autumn", "topsoil_removal")
      )
    )
)

data %>% 
  group_by(year_topsoil_removal) %>% 
  count()

data %>%
  group_by(mowing_date, mowing_date_start) %>%
  count()

data %>%
  group_by(year_topsoil_removal, year_hay_transfer) %>%
  count()

data %>%
  group_by(mowing_date, year_hay_transfer, mowing_date_start) %>%
  count()
