#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Management Garchinger Heide restoration sites
# Specific leaf area (SLA) ####
# Show figure 4a
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sina Appeltauer, Markus Bauer
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
    text = element_text(size = 8.5, color = "black"),
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
  rename(y = CWM_SLA) %>%
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
  )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data <- sites %>%
  mutate(y = exp(y)) %>%
  rename(predicted = y, x = treatment_age)

(graph <- ggplot(
  data = data,
  aes(x = x, predicted, color = x, fill = x)
) +
    geom_quasirandom(color = "grey20", dodge.width = .6, size = 1, shape = 16) +
    geom_boxplot(alpha = .5, color = "black") +
    annotate(
      "text", x = 2, y = 250, size = 3,
      label = expression(4^th~corner*": n.s.")
      ) +
    scale_y_continuous(limits = c(150, 253), breaks = seq(-100, 400, 10)) +
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
    labs(x = "", y = expression(CWM ~ SLA ~ "[" * cm^2 * g^-1 * "]")) +
    theme_mb()); graph


### Save ###
ggsave(
  here("outputs", "figures", "figure_4a_sla_800dpi_9x8cm.tiff"),
  dpi = 800, width = 9, height = 8, units = "cm"
)

graph_a <- graph +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()
  )
