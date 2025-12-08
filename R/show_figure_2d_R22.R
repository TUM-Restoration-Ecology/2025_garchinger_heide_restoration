#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Management Garchinger Heide restoration sites
# Species richness EUNIS habitat type R22 ####
# Show figure 2b
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
    text = element_text(size = 9, color = "black"),
    strip.text = element_text(size = 10),
    axis.text = element_text(angle = 0, hjust = 0.5, size = 9,
                             color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.title = element_text(angle = 0, hjust = 0.5, size = 9,
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
    treatment = "f",
    treatment_age = "f"
  )
) %>%
  rename(y = richness_R22) %>%
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

### * Model ####
load(file = here("outputs", "models", "model_richness_r22_2.Rdata"))
m <- m2
m



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data_model <- ggeffect(
  m, terms = c("treatment_age"), back.transform = TRUE, ci_level = .95
) %>%
  as_tibble() %>%
  mutate(
    x = fct_recode(
      x,
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

data <- sites %>%
  rename(predicted = y, x = treatment_age)

(graph_d <- ggplot() +
    geom_quasirandom(
      data = data,
      aes(x = x, predicted, color = x),
      dodge.width = .6, size = 1, shape = 16
    ) +
    geom_errorbar(
      data = data_model,
      aes(x, predicted, ymin = conf.low, ymax = conf.high),
      width = 0.0, linewidth = 0.4
    ) +
    geom_point(
      data = data_model,
      aes(x, predicted),
      size = 2
    ) +
    annotate("text", label = "b", x = 1, y = 49) +
    annotate("text", label = "b", x = 2, y = 49) +
    annotate("text", label = "bc", x = 3, y = 49) +
    annotate("text", label = "a", x = 4, y = 49) +
    annotate("text", label = "ab", x = 5, y = 49) +
    annotate("text", label = "ab", x = 6, y = 49) +
    annotate("text", label = "ab", x = 7, y = 49) +
    annotate("text", label = "c", x = 8, y = 49) +
    annotate(
      "text", label = expression(italic(R)^2~"="~0.39),
      x = 7, y = 41, size = 2.5
    ) +
    scale_y_continuous(limits = c(0, 49), breaks = seq(-100, 400, 5)) +
    scale_color_manual(
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
    labs(
      x = "", y = expression(Indicator ~ species ~ R22 ~ "[" * '# / 4m²' * "]")
    ) +
    theme_mb())

### Save ###
ggsave(
  here("outputs", "figures", "figure_2d_R22_800dpi_8x8cm.tiff"),
  dpi = 800, width = 8, height = 8, units = "cm"
)
