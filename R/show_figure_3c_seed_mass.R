#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Management Garchinger Heide restoration sites
# Seed mass ####
# Show figure 3c
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-03-03



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
    treatment = fct_relevel(
      treatment, "control_2003", "control_2018", "control_2021", "cut_summer",
      "cut_autumn", "grazing"
    ),
    treatment = fct_recode(
      treatment, "Ref.\n2003" = "control_2003",
      "Ref.\n2018" = "control_2018", "Ref.\n2021" = "control_2021",
      "Mowing\nsummer" = "cut_summer", "Mowing\nautumn" = "cut_autumn",
      "Topsoil\nremoval" = "grazing"
    )
  ) %>%
  filter(
    !(id %in% c(
      "X2021tum03", "X2021tum27", "X2021tum43", "X2021tum48", "X2021tum51",
      "XroederS11"
    )) # Plots with >=10% of Polygonatum odoratum (seed mass = 0.08 g)
  )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data <- sites %>%
  mutate(y = y * 1000) %>%
  rename(predicted = y, x = treatment)

(graph_c <- ggplot(
  data = data,
  aes(x = x, predicted, color = x, fill = x)
) +
    geom_quasirandom(color = "grey20", dodge.width = .6, size = 1, shape = 16) +
    geom_boxplot(alpha = .5, color = "black") +
    annotate(
      "text", x = 1.5, y = 0, size = 2.5,
      label = expression(4^th~corner*": n.s.")
    ) +
    scale_y_continuous(limits = c(0, 5.6), breaks = seq(-100, 400, .5)) +
    scale_fill_manual(
      values = c(
        "Ref.\n2003" = "#f947d1", 
        "Ref.\n2018" = "#f947d1", 
        "Ref.\n2021" = "#f947d1", 
        "Mowing\nsummer" = "#61a161", 
        "Mowing\nautumn" = "#87ceeb", 
        "Topsoil\nremoval" = "#b06e13"
      )
    ) +
    labs(x = "", y = expression(CWM ~ seed ~ mass ~ "[" * mg * "]")) +
    theme_mb())

### Save ###
ggsave(
  here("outputs", "figures", "figure_3c_seed_mass_800dpi_8x8cm.tiff"),
  dpi = 800, width = 8, height = 8, units = "cm"
  )
