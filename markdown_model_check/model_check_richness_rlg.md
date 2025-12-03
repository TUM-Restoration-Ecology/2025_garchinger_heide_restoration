Garchinger Heide and restoration sites: <br> Red List Germany
================
<b>Sina Appeltauer, Markus Bauer</b> <br>
<b>2025-12-03</b>

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
      ),
      treatment_age = col_factor(
        levels = c("control_2003", "control_2018", "control_2021", "cut_summer_old",
                   "cut_summer_young", "cut_autumn_old", "cut_autumn_young",
                   "topsoil_removal")
      )
    )
  ) %>%
  rename(y = rlg)

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
    ## 17.04127 16.29870 15.55613

``` r
median(sites$y)
```

    ## [1] 17

``` r
sd(sites$y)
```

    ## [1] 5.728035

``` r
quantile(sites$y, probs = c(0.05, 0.95), na.rm = TRUE)
```

    ##  5% 95% 
    ##   7  24

### Graphs of raw data (Step 2, 6, 7)

![](model_check_richness_rlg_files/figure-gfm/data-exploration-1.png)<!-- -->![](model_check_richness_rlg_files/figure-gfm/data-exploration-2.png)<!-- -->![](model_check_richness_rlg_files/figure-gfm/data-exploration-3.png)<!-- -->![](model_check_richness_rlg_files/figure-gfm/data-exploration-4.png)<!-- -->![](model_check_richness_rlg_files/figure-gfm/data-exploration-5.png)<!-- -->

### Outliers, zero-inflation, transformations? (Step 1, 3, 4)

    ## # A tibble: 8 × 2
    ##   treatment_age        n
    ##   <fct>            <int>
    ## 1 control_2003        42
    ## 2 control_2018        42
    ## 3 control_2021        62
    ## 4 cut_summer_old      15
    ## 5 cut_summer_young    10
    ## 6 cut_autumn_old      10
    ## 7 cut_autumn_young    20
    ## 8 topsoil_removal     30

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](model_check_richness_rlg_files/figure-gfm/outliers-1.png)<!-- -->

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

![](model_check_richness_rlg_files/figure-gfm/collinearity-1.png)<!-- -->

## Models

Only here you have to modify the script to compare other models

``` r
load(file = here("outputs", "models", "model_richness_rlg_2.Rdata"))
load(file = here("outputs", "models", "model_richness_rlg_2sub.Rdata"))
m_1 <- m2
m_2 <- m2sub
```

``` r
m_1
## 
## Call:
## lm(formula = y ~ treatment_age + mem2, data = sites)
## 
## Coefficients:
##                   (Intercept)      treatment_agecontrol_2018  
##                       18.5559                         0.6667  
##     treatment_agecontrol_2021    treatment_agecut_autumn_old  
##                        2.6017                       -10.3556  
## treatment_agecut_autumn_young    treatment_agecut_summer_old  
##                       -9.2556                        -9.9222  
## treatment_agecut_summer_young   treatment_agetopsoil_removal  
##                      -10.8556                        -5.4889  
##                          mem2  
##                       -0.3381
m_2
## 
## Call:
## lm(formula = y ~ treatment + mem2, data = subset)
## 
## Coefficients:
##              (Intercept)     treatmentcontrol_2018     treatmentcontrol_2021  
##                  18.5559                    0.6667                    2.6017  
## treatmenttopsoil_removal       treatmentcut_summer       treatmentcut_autumn  
##                  -5.6990                  -10.6556                   -9.6222  
##                     mem2  
##                  -0.3381
```

## Model check

### DHARMa

``` r
simulation_output_1 <- simulateResiduals(m_1, plot = TRUE)
```

![](model_check_richness_rlg_files/figure-gfm/dharma_all-1.png)<!-- -->

``` r
simulation_output_2 <- simulateResiduals(m_2, plot = TRUE)
```

![](model_check_richness_rlg_files/figure-gfm/dharma_all-2.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$treatment_age)
```

![](model_check_richness_rlg_files/figure-gfm/dharma_single-1.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, subset$treatment) # dataframe subset
```

![](model_check_richness_rlg_files/figure-gfm/dharma_single-2.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$mem2)
```

![](model_check_richness_rlg_files/figure-gfm/dharma_single-3.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, subset$mem2) # dataframe subset
```

![](model_check_richness_rlg_files/figure-gfm/dharma_single-4.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$botanist)
```

![](model_check_richness_rlg_files/figure-gfm/dharma_single-5.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, subset$botanist) # dataframe subset
```

![](model_check_richness_rlg_files/figure-gfm/dharma_single-6.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$year_hay_transfer)
```

![](model_check_richness_rlg_files/figure-gfm/dharma_single-7.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, subset$year_hay_transfer) # dataframe subset
```

![](model_check_richness_rlg_files/figure-gfm/dharma_single-8.png)<!-- -->

### Check collinearity part 2 (Step 5)

Remove VIF \> 3 or \> 10 <br> Zuur et al. 2010 Methods Ecol Evol
[DOI:10.1111/j.2041-210X.2009.00001.x](https://doi.org/10.1111/j.2041-210X.2009.00001.x)

``` r
car::vif(m_1)
```

    ##                  GVIF Df GVIF^(1/(2*Df))
    ## treatment_age 1.05719  7        1.003980
    ## mem2          1.05719  1        1.028197

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
## [1,] 0.7187002 0.7187002
MuMIn::r.squaredGLMM(m_2)
##            R2m       R2c
## [1,] 0.7072793 0.7072793
```

### AICc

Use AICc and not AIC since ratio n/K \< 40 <br> Burnahm & Anderson 2002
p. 66 ISBN:
[978-0-387-95364-9](https://search.worldcat.org/de/title/845688581)

``` r
MuMIn::AICc(m_1, m_2) %>%
  arrange(AICc)
##     df     AICc
## m_2  8 1104.138
## m_1 10 1183.018
```

## Predicted values

### Summary table

``` r
summary(m_1)
```

    ## 
    ## Call:
    ## lm(formula = y ~ treatment_age + mem2, data = sites)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.6113 -1.9300  0.0647  2.2441  8.5703 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    18.5559     0.4736  39.181  < 2e-16 ***
    ## treatment_agecontrol_2018       0.6667     0.6662   1.001    0.318    
    ## treatment_agecontrol_2021       2.6017     0.6109   4.259 3.04e-05 ***
    ## treatment_agecut_autumn_old   -10.3556     1.0798  -9.590  < 2e-16 ***
    ## treatment_agecut_autumn_young  -9.2556     0.8366 -11.063  < 2e-16 ***
    ## treatment_agecut_summer_old    -9.9222     0.9248 -10.729  < 2e-16 ***
    ## treatment_agecut_summer_young -10.8556     1.0798 -10.053  < 2e-16 ***
    ## treatment_agetopsoil_removal   -5.4889     0.7380  -7.438 2.21e-12 ***
    ## mem2                           -0.3381     0.2065  -1.637    0.103    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.053 on 222 degrees of freedom
    ## Multiple R-squared:  0.7258, Adjusted R-squared:  0.7159 
    ## F-statistic: 73.45 on 8 and 222 DF,  p-value: < 2.2e-16

### Forest plot

``` r
dotwhisker::dwplot(
  list(m_1, m_2),
  ci = 0.95,
  show_intercept = FALSE,
  vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_classic()
```

![](model_check_richness_rlg_files/figure-gfm/predicted_values-1.png)<!-- -->

### Effect sizes

Effect sizes of chosen model just to get exact values of means etc. if
necessary.

``` r
(emm <- emmeans(
  m_1,
  revpairwise ~ treatment_age,
  type = "response"
  ))
```

    ## $emmeans
    ##  treatment_age    emmean    SE  df lower.CL upper.CL
    ##  control_2003      18.56 0.474 222    17.62    19.49
    ##  control_2018      19.22 0.474 222    18.29    20.16
    ##  control_2021      21.16 0.388 222    20.39    21.92
    ##  cut_autumn_old     8.20 0.967 222     6.29    10.11
    ##  cut_autumn_young   9.30 0.685 222     7.95    10.65
    ##  cut_summer_old     8.63 0.791 222     7.08    10.19
    ##  cut_summer_young   7.70 0.967 222     5.79     9.61
    ##  topsoil_removal   13.07 0.561 222    11.96    14.17
    ## 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast                            estimate    SE  df t.ratio p.value
    ##  control_2018 - control_2003            0.667 0.666 222   1.001  0.9740
    ##  control_2021 - control_2003            2.602 0.611 222   4.259  0.0008
    ##  control_2021 - control_2018            1.935 0.611 222   3.167  0.0367
    ##  cut_autumn_old - control_2003        -10.356 1.080 222  -9.590  <.0001
    ##  cut_autumn_old - control_2018        -11.022 1.080 222 -10.208  <.0001
    ##  cut_autumn_old - control_2021        -12.957 1.040 222 -12.419  <.0001
    ##  cut_autumn_young - control_2003       -9.256 0.837 222 -11.063  <.0001
    ##  cut_autumn_young - control_2018       -9.922 0.837 222 -11.860  <.0001
    ##  cut_autumn_young - control_2021      -11.857 0.789 222 -15.028  <.0001
    ##  cut_autumn_young - cut_autumn_old      1.100 1.180 222   0.930  0.9828
    ##  cut_summer_old - control_2003         -9.922 0.925 222 -10.729  <.0001
    ##  cut_summer_old - control_2018        -10.589 0.925 222 -11.450  <.0001
    ##  cut_summer_old - control_2021        -12.524 0.882 222 -14.200  <.0001
    ##  cut_summer_old - cut_autumn_old        0.433 1.250 222   0.348  1.0000
    ##  cut_summer_old - cut_autumn_young     -0.667 1.040 222  -0.639  0.9983
    ##  cut_summer_young - control_2003      -10.856 1.080 222 -10.053  <.0001
    ##  cut_summer_young - control_2018      -11.522 1.080 222 -10.671  <.0001
    ##  cut_summer_young - control_2021      -13.457 1.040 222 -12.898  <.0001
    ##  cut_summer_young - cut_autumn_old     -0.500 1.370 222  -0.366  1.0000
    ##  cut_summer_young - cut_autumn_young   -1.600 1.180 222  -1.353  0.8771
    ##  cut_summer_young - cut_summer_old     -0.933 1.250 222  -0.749  0.9953
    ##  topsoil_removal - control_2003        -5.489 0.738 222  -7.438  <.0001
    ##  topsoil_removal - control_2018        -6.156 0.738 222  -8.341  <.0001
    ##  topsoil_removal - control_2021        -8.091 0.683 222 -11.837  <.0001
    ##  topsoil_removal - cut_autumn_old       4.867 1.110 222   4.366  0.0005
    ##  topsoil_removal - cut_autumn_young     3.767 0.881 222   4.274  0.0007
    ##  topsoil_removal - cut_summer_old       4.433 0.965 222   4.592  0.0002
    ##  topsoil_removal - cut_summer_young     5.367 1.110 222   4.814  0.0001
    ## 
    ## P value adjustment: tukey method for comparing a family of 8 estimates

``` r
plot(emm, comparison = TRUE)
```

![](model_check_richness_rlg_files/figure-gfm/effect-sizes-1.png)<!-- -->

# Session info

    ## R version 4.5.1 (2025-06-13 ucrt)
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
    ## time zone: Europe/Berlin
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
    ##  [4] magrittr_2.0.3         compiler_4.5.1         mgcv_1.9-3            
    ##  [7] vctrs_0.6.5            pkgconfig_2.0.3        crayon_1.5.3          
    ## [10] fastmap_1.2.0          backports_1.5.0        labeling_0.4.3        
    ## [13] utf8_1.2.6             ggstance_0.3.7         promises_1.3.3        
    ## [16] rmarkdown_2.29         tzdb_0.5.0             nloptr_2.2.1          
    ## [19] bit_4.6.0              xfun_0.52              later_1.4.2           
    ## [22] parallel_4.5.1         R6_2.6.1               gap.datasets_0.0.6    
    ## [25] stringi_1.8.7          qgam_2.0.0             RColorBrewer_1.1-3    
    ## [28] GGally_2.2.1           car_3.1-3              boot_1.3-31           
    ## [31] estimability_1.5.1     Rcpp_1.1.0             iterators_1.0.14      
    ## [34] knitr_1.50             parameters_0.27.0      httpuv_1.6.16         
    ## [37] Matrix_1.7-3           splines_4.5.1          timechange_0.3.0      
    ## [40] tidyselect_1.2.1       rstudioapi_0.17.1      abind_1.4-8           
    ## [43] yaml_2.3.10            MuMIn_1.48.11          doParallel_1.0.17     
    ## [46] codetools_0.2-20       lattice_0.22-7         plyr_1.8.9            
    ## [49] shiny_1.11.1           withr_3.0.2            bayestestR_0.16.1     
    ## [52] coda_0.19-4.1          evaluate_1.0.4         marginaleffects_0.28.0
    ## [55] ggstats_0.10.0         pillar_1.11.0          gap_1.6               
    ## [58] carData_3.0-5          foreach_1.5.2          stats4_4.5.1          
    ## [61] reformulas_0.4.1       insight_1.3.1          generics_0.1.4        
    ## [64] vroom_1.6.5            rprojroot_2.1.0        hms_1.1.3             
    ## [67] scales_1.4.0           minqa_1.2.8            xtable_1.8-4          
    ## [70] glue_1.8.0             tools_4.5.1            data.table_1.17.8     
    ## [73] lme4_1.1-37            mvtnorm_1.3-3          grid_4.5.1            
    ## [76] rbibutils_2.3          datawizard_1.1.0       nlme_3.1-168          
    ## [79] Rmisc_1.5.1            performance_0.15.0     beeswarm_0.4.0        
    ## [82] vipor_0.4.7            Formula_1.2-5          cli_3.6.5             
    ## [85] gtable_0.3.6           digest_0.6.37          farver_2.1.2          
    ## [88] htmltools_0.5.8.1      lifecycle_1.0.4        mime_0.13             
    ## [91] bit64_4.6.0-1          dotwhisker_0.8.4       MASS_7.3-65
