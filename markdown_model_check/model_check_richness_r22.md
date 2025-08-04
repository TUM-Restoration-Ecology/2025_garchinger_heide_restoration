Garchinger Heide and restoration sites: <br> R22 indicator richness
================
<b>Sina Appeltauer, Markus Bauer</b> <br>
<b>2025-08-04</b>

- [Preparation](#preparation)
- [Statistics](#statistics)
  - [Data exploration](#data-exploration)
    - [Means and deviations](#means-and-deviations)
    - [Graphs of raw data (Step 2, 6,
      7)](#graphs-of-raw-data-step-2-6-7)
    - [Outliers, zero-inflation, transformations? (Step 1, 3,
      4)](#outliers-zero-inflation-transformations-step-1-3-4)
    - [Check collinearity part 1 (Step
      5)](#check-collinearity-part-1-step-5)
  - [Models](#models)
  - [Model check](#model-check)
    - [DHARMa](#dharma)
    - [Check collinearity part 2 (Step
      5)](#check-collinearity-part-2-step-5)
  - [Model comparison](#model-comparison)
    - [<i>R</i><sup>2</sup> values](#r2-values)
    - [AICc](#aicc)
  - [Predicted values](#predicted-values)
    - [Summary table](#summary-table)
    - [Forest plot](#forest-plot)
    - [Effect sizes](#effect-sizes)
- [Session info](#session-info)

<br/> <br/> <b>Markus Bauer</b>\*, <b>Sina Appeltauer</b>, <b>Malte
Knöppler</b>, <b>Maren Teschauer</b> & <b>Johannes Kollmann</b>

Technichal University of Munich, TUM School of Life Sciences, Chair of
Restoration Ecology, Emil-Ramann-Straße 6, 85354 Freising, Germany

\*<markus1.bauer@tum.de>

ORCiD ID: [0000-0001-5372-4174](https://orcid.org/0000-0001-5372-4174)
<br> [Google
Scholar](https://scholar.google.de/citations?user=oHhmOkkAAAAJ&hl=de&oi=ao)
<br> GitHub: [markus1bauer](https://github.com/markus1bauer) <br>
GitHub:
[TUM-Restoration-Ecology](https://github.com/TUM-Restoration-Ecology)

To compare different models, you only have to change the models in
section ‘Load models’

# Preparation

Protocol of data exploration (Steps 1-8) used from Zuur et al. (2010)
Methods Ecol Evol
[DOI:10.1111/2041-210X.12577](https://doi.org/10.1111/2041-210X.12577)

#### Packages

``` r
library(here)
library(tidyverse)
library(ggbeeswarm)
library(patchwork)
library(DHARMa)
library(emmeans)
```

#### Load data

``` r
sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("", "na", "NA"), col_types = 
    cols(
      .default = "?",
      year_hay_transfer = "f",
      treatment = col_factor(
        levels = c("control_2003", "control_2018", "control_2021", "cut_summer",
                   "cut_autumn", "topsoil_removal")
      )
    )
  ) %>%
  rename(y = richness_R22)

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
```

# Statistics

## Data exploration

### Means and deviations

``` r
Rmisc::CI(sites$y, ci = .95)
```

    ##    upper     mean    lower 
    ## 6.060940 5.593074 5.125207

``` r
median(sites$y)
```

    ## [1] 6

``` r
sd(sites$y)
```

    ## [1] 3.609016

``` r
quantile(sites$y, probs = c(0.05, 0.95), na.rm = TRUE)
```

    ##   5%  95% 
    ##  1.0 11.5

### Graphs of raw data (Step 2, 6, 7)

![](model_check_richness_r22_files/figure-gfm/data-exploration-1.png)<!-- -->![](model_check_richness_r22_files/figure-gfm/data-exploration-2.png)<!-- -->![](model_check_richness_r22_files/figure-gfm/data-exploration-3.png)<!-- -->![](model_check_richness_r22_files/figure-gfm/data-exploration-4.png)<!-- -->![](model_check_richness_r22_files/figure-gfm/data-exploration-5.png)<!-- -->![](model_check_richness_r22_files/figure-gfm/data-exploration-6.png)<!-- -->

### Outliers, zero-inflation, transformations? (Step 1, 3, 4)

    ## # A tibble: 6 × 2
    ##   treatment           n
    ##   <fct>           <int>
    ## 1 control_2003       42
    ## 2 control_2018       42
    ## 3 control_2021       62
    ## 4 cut_summer         25
    ## 5 cut_autumn         30
    ## 6 topsoil_removal    30

![](model_check_richness_r22_files/figure-gfm/outliers-1.png)<!-- -->

### Check collinearity part 1 (Step 5)

Exclude r \> 0.7 <br> Dormann et al. 2013 Ecography
[DOI:10.1111/j.1600-0587.2012.07348.x](https://doi.org/10.1111/j.1600-0587.2012.07348.x)

``` r
sites %>%
    select(
    cover_vegetation, height_vegetation, grass_cover, graminoid_cover, mem1,
    mem2
    ) %>%
  GGally::ggpairs(lower = list(continuous = "smooth_loess")) +
  theme(strip.text = element_text(size = 7))
```

![](model_check_richness_r22_files/figure-gfm/collinearity-1.png)<!-- -->

## Models

Only here you have to modify the script to compare other models

``` r
load(file = here("outputs", "models", "model_richness_r22_2.Rdata"))
load(file = here("outputs", "models", "model_richness_r22_2sub.Rdata"))
m_1 <- m2
m_2 <- m2sub
```

``` r
m_1
## 
## Call:
## lm(formula = y ~ treatment + mem2, data = sites)
## 
## Coefficients:
##              (Intercept)     treatmentcontrol_2018     treatmentcontrol_2021  
##                   6.3799                   -1.2143                   -2.7427  
## treatmenttopsoil_removal       treatmentcut_summer       treatmentcut_autumn  
##                  -3.4083                    3.5917                    1.7250  
##                     mem2  
##                  -0.0965
m_2
## 
## Call:
## lm(formula = y ~ treatment + mem2, data = subset)
## 
## Coefficients:
##              (Intercept)     treatmentcontrol_2018     treatmentcontrol_2021  
##                   6.3799                   -1.2143                   -2.7427  
## treatmenttopsoil_removal       treatmentcut_summer       treatmentcut_autumn  
##                  -3.4518                    3.4667                    1.7250  
##                     mem2  
##                  -0.0965
```

## Model check

### DHARMa

``` r
simulation_output_1 <- simulateResiduals(m_1, plot = TRUE)
```

![](model_check_richness_r22_files/figure-gfm/dharma_all-1.png)<!-- -->

``` r
simulation_output_2 <- simulateResiduals(m_2, plot = TRUE)
```

![](model_check_richness_r22_files/figure-gfm/dharma_all-2.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$treatment)
```

![](model_check_richness_r22_files/figure-gfm/dharma_single-1.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, subset$treatment) # dataframe subset
```

![](model_check_richness_r22_files/figure-gfm/dharma_single-2.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$mem2)
```

![](model_check_richness_r22_files/figure-gfm/dharma_single-3.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, subset$mem2) # dataframe subset
```

![](model_check_richness_r22_files/figure-gfm/dharma_single-4.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$botanist)
```

![](model_check_richness_r22_files/figure-gfm/dharma_single-5.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, subset$botanist) # dataframe subset
```

![](model_check_richness_r22_files/figure-gfm/dharma_single-6.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$year_hay_transfer)
```

![](model_check_richness_r22_files/figure-gfm/dharma_single-7.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, subset$year_hay_transfer) # dataframe subset
```

![](model_check_richness_r22_files/figure-gfm/dharma_single-8.png)<!-- -->

### Check collinearity part 2 (Step 5)

Remove VIF \> 3 or \> 10 <br> Zuur et al. 2010 Methods Ecol Evol
[DOI:10.1111/j.2041-210X.2009.00001.x](https://doi.org/10.1111/j.2041-210X.2009.00001.x)

``` r
car::vif(m_1)
```

    ##              GVIF Df GVIF^(1/(2*Df))
    ## treatment 1.05719  5        1.005577
    ## mem2      1.05719  1        1.028197

``` r
car::vif(m_2)
```

    ##               GVIF Df GVIF^(1/(2*Df))
    ## treatment 1.050357  5        1.004925
    ## mem2      1.050357  1        1.024869

## Model comparison

### <i>R</i><sup>2</sup> values

``` r
MuMIn::r.squaredGLMM(m_1)
##            R2m       R2c
## [1,] 0.3793246 0.3793246
MuMIn::r.squaredGLMM(m_2)
##            R2m       R2c
## [1,] 0.3550319 0.3550319
```

### AICc

Use AICc and not AIC since ratio n/K \< 40 <br> Burnahm & Anderson 2002
p. 66 ISBN:
[978-0-387-95364-9](https://search.worldcat.org/de/title/845688581)

``` r
MuMIn::AICc(m_1, m_2) %>%
  arrange(AICc)
##     df     AICc
## m_2  8 1061.229
## m_1  8 1151.633
```

## Predicted values

### Summary table

``` r
summary(m_1)
```

    ## 
    ## Call:
    ## lm(formula = y ~ treatment + mem2, data = sites)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.0000 -2.0000 -0.7353  2.4380  6.0000 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                6.3799     0.4447  14.347  < 2e-16 ***
    ## treatmentcontrol_2018     -1.2143     0.6255  -1.941   0.0535 .  
    ## treatmentcontrol_2021     -2.7427     0.5736  -4.781 3.15e-06 ***
    ## treatmenttopsoil_removal  -3.4083     0.6929  -4.919 1.69e-06 ***
    ## treatmentcut_summer        3.5917     0.7314   4.911 1.75e-06 ***
    ## treatmentcut_autumn        1.7250     0.6929   2.489   0.0135 *  
    ## mem2                      -0.0965     0.1939  -0.498   0.6192    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.867 on 224 degrees of freedom
    ## Multiple R-squared:  0.3856, Adjusted R-squared:  0.3691 
    ## F-statistic: 23.43 on 6 and 224 DF,  p-value: < 2.2e-16

### Forest plot

``` r
dotwhisker::dwplot(
  list(m_1, m_2),
  ci = 0.95,
  show_intercept = FALSE,
  vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_classic()
```

![](model_check_richness_r22_files/figure-gfm/predicted_values-1.png)<!-- -->

### Effect sizes

Effect sizes of chosen model just to get exact values of means etc. if
necessary.

``` r
(emm <- emmeans(
  m_1,
  revpairwise ~ treatment,
  type = "response"
  ))
```

    ## $emmeans
    ##  treatment       emmean    SE  df lower.CL upper.CL
    ##  control_2003      6.38 0.445 224     5.50     7.26
    ##  control_2018      5.17 0.445 224     4.29     6.04
    ##  control_2021      3.64 0.364 224     2.92     4.36
    ##  topsoil_removal   2.97 0.526 224     1.93     4.01
    ##  cut_summer        9.97 0.576 224     8.84    11.11
    ##  cut_autumn        8.10 0.526 224     7.07     9.14
    ## 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast                       estimate    SE  df t.ratio p.value
    ##  control_2018 - control_2003      -1.214 0.626 224  -1.941  0.3799
    ##  control_2021 - control_2003      -2.743 0.574 224  -4.781  <.0001
    ##  control_2021 - control_2018      -1.528 0.574 224  -2.664  0.0866
    ##  topsoil_removal - control_2003   -3.408 0.693 224  -4.919  <.0001
    ##  topsoil_removal - control_2018   -2.194 0.693 224  -3.166  0.0215
    ##  topsoil_removal - control_2021   -0.666 0.642 224  -1.037  0.9049
    ##  cut_summer - control_2003         3.592 0.731 224   4.911  <.0001
    ##  cut_summer - control_2018         4.806 0.731 224   6.571  <.0001
    ##  cut_summer - control_2021         6.334 0.683 224   9.273  <.0001
    ##  cut_summer - topsoil_removal      7.000 0.776 224   9.017  <.0001
    ##  cut_autumn - control_2003         1.725 0.693 224   2.489  0.1316
    ##  cut_autumn - control_2018         2.939 0.693 224   4.242  0.0005
    ##  cut_autumn - control_2021         4.468 0.642 224   6.962  <.0001
    ##  cut_autumn - topsoil_removal      5.133 0.740 224   6.936  <.0001
    ##  cut_autumn - cut_summer          -1.867 0.776 224  -2.405  0.1590
    ## 
    ## P value adjustment: tukey method for comparing a family of 6 estimates

``` r
plot(emm, comparison = TRUE)
```

![](model_check_richness_r22_files/figure-gfm/effect-sizes-1.png)<!-- -->

# Session info

    ## R version 4.5.0 (2025-04-11 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 11 x64 (build 26100)
    ## 
    ## Matrix products: default
    ##   LAPACK version 3.12.1
    ## 
    ## locale:
    ## [1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8   
    ## [3] LC_MONETARY=German_Germany.utf8 LC_NUMERIC=C                   
    ## [5] LC_TIME=German_Germany.utf8    
    ## 
    ## time zone: America/Denver
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] emmeans_1.11.2   DHARMa_0.4.7     patchwork_1.3.1  ggbeeswarm_0.7.2
    ##  [5] lubridate_1.9.4  forcats_1.0.0    stringr_1.5.1    dplyr_1.1.4     
    ##  [9] purrr_1.1.0      readr_2.1.5      tidyr_1.3.1      tibble_3.3.0    
    ## [13] ggplot2_3.5.2    tidyverse_2.0.0  here_1.0.1      
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rdpack_2.6.4           gridExtra_2.3          rlang_1.1.6           
    ##  [4] magrittr_2.0.3         compiler_4.5.0         mgcv_1.9-1            
    ##  [7] vctrs_0.6.5            pkgconfig_2.0.3        crayon_1.5.3          
    ## [10] fastmap_1.2.0          backports_1.5.0        labeling_0.4.3        
    ## [13] utf8_1.2.6             ggstance_0.3.7         promises_1.3.3        
    ## [16] rmarkdown_2.29         tzdb_0.5.0             nloptr_2.2.1          
    ## [19] bit_4.6.0              xfun_0.52              later_1.4.2           
    ## [22] parallel_4.5.0         R6_2.6.1               gap.datasets_0.0.6    
    ## [25] stringi_1.8.7          qgam_2.0.0             RColorBrewer_1.1-3    
    ## [28] GGally_2.2.1           car_3.1-3              boot_1.3-31           
    ## [31] estimability_1.5.1     Rcpp_1.1.0             iterators_1.0.14      
    ## [34] knitr_1.50             parameters_0.27.0      httpuv_1.6.16         
    ## [37] Matrix_1.7-3           splines_4.5.0          timechange_0.3.0      
    ## [40] tidyselect_1.2.1       rstudioapi_0.17.1      abind_1.4-8           
    ## [43] yaml_2.3.10            MuMIn_1.48.11          doParallel_1.0.17     
    ## [46] codetools_0.2-20       lattice_0.22-6         plyr_1.8.9            
    ## [49] shiny_1.11.1           withr_3.0.2            bayestestR_0.16.1     
    ## [52] coda_0.19-4.1          evaluate_1.0.4         marginaleffects_0.28.0
    ## [55] ggstats_0.10.0         pillar_1.11.0          gap_1.6               
    ## [58] carData_3.0-5          foreach_1.5.2          stats4_4.5.0          
    ## [61] reformulas_0.4.1       insight_1.3.1          generics_0.1.4        
    ## [64] vroom_1.6.5            rprojroot_2.1.0        hms_1.1.3             
    ## [67] scales_1.4.0           minqa_1.2.8            xtable_1.8-4          
    ## [70] glue_1.8.0             tools_4.5.0            data.table_1.17.8     
    ## [73] lme4_1.1-37            mvtnorm_1.3-3          grid_4.5.0            
    ## [76] rbibutils_2.3          datawizard_1.1.0       nlme_3.1-168          
    ## [79] Rmisc_1.5.1            performance_0.15.0     beeswarm_0.4.0        
    ## [82] vipor_0.4.7            Formula_1.2-5          cli_3.6.5             
    ## [85] gtable_0.3.6           digest_0.6.37          farver_2.1.2          
    ## [88] htmltools_0.5.8.1      lifecycle_1.0.4        mime_0.13             
    ## [91] bit64_4.6.0-1          dotwhisker_0.8.4       MASS_7.3-65
