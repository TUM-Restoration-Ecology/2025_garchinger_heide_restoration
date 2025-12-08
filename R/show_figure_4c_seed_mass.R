#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Management Garchinger Heide restoration sites
# Seed mass ####
# Show figure 4c
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-12-08



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggeffects)
library(ggbeeswarm)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d")))

### Functions ###
theme_mb <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 9, color = "black"),
    strip.text = element_text(size = 10),
    axis.text = element_text(angle = 0, hjust = 0.5, size = 8.5,
                             color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.title = element_text(angle = 0, hjust = 0.5, size = 8.5,
                              color = "black"),
    axis.line = element_line(),
    legend.key = element_rect(fill = "white"),
    legend.position = "none",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

### Load data ###
sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("", "na", "NA"), col_types = cols(
    .default = "?",
    treatment = "f"
  )
) %>%
  rename(y = CWM_Seed) %>%
  mutate(
    treatment_age = fct_relevel(
      treatment_age, "control_2003", "control_2018", "control_2021",
      "cut_summer_old", "cut_summer_young", "cut_autumn_old",
      "cut_autumn_young", "topsoil_removal"
    ),
    treatment_age = fct_recode(
      treatment_age,
      "Reference\n2003" = "control_2003",
      "Reference\n2018" = "control_2018",
      "Reference\n2021" = "control_2021",
      "Mowing summer\n>10 yrs" = "cut_summer_old",
      "Mowing summer\n<10 yrs" = "cut_summer_young",
      "Mowing autumn\n>10 yrs" = "cut_autumn_old",
      "Mowing autumn\n<10 yrs" = "cut_autumn_young",
      "Topsoil removal" = "topsoil_removal"
    )
  ) %>%
  filter(
    !(id %in% c(
      "X2021tum03", "X2021tum27", "X2021tum43", "X2021tum48", "X2021tum51",
      "X2003roederS11", "X2018roederS12", "X2018roederS16", "X2018roederS18"
    )) # Plots with >=10% of Polygonatum odoratum (seed mass = 0.08 g)
  )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data <- sites %>%
  mutate(y = y * 1000) %>%
  rename(predicted = y, x = treatment_age)

(graph_c <- ggplot(
  data = data,
  aes(x = x, predicted, color = x, fill = x)
) +
    geom_quasirandom(color = "grey20", dodge.width = .6, size = 1, shape = 16) +
    geom_boxplot(alpha = .5, color = "black") +
    annotate(
      "text", x = 1.7, y = 7, size = 3,
      label = expression(4^th ~ corner * ": n.s.")
    ) +
    scale_y_continuous(breaks = seq(0, 100, 1)) +
    scale_fill_manual(
      values = c(
        "Reference\n2003" = "#f947d1", 
        "Reference\n2018" = "#f947d1", 
        "Reference\n2021" = "#f947d1", 
        "Mowing summer\n>10 yrs" = "#61a161", 
        "Mowing summer\n<10 yrs" = "#61a161", 
        "Mowing autumn\n>10 yrs" = "#87ceeb", 
        "Mowing autumn\n<10 yrs" = "#87ceeb", 
        "Topsoil removal" = "#b06e13"
      )
    ) +
    labs(x = "", y = expression(CWM ~ seed ~ mass ~ "[" * mg * "]")) +
    theme_mb())

### Save ###
ggsave(
  here("outputs", "figures", "figure_4c_seed_mass_800dpi_9x8cm.tiff"),
  dpi = 800, width = 9, height = 8, units = "cm"
  )
