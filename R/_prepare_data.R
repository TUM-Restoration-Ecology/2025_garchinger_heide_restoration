#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Management Garchinger Heide restoration sites
# Prepare data ####
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer, Sina Appeltauer
# 2025-07-13



### Packages ###
library(renv)
suppressPackageStartupMessages(library(installr))
library(here)
library(tidyverse)
library(TNRS)
library(GIFT)
library(FD)
library(vegan)
library(adespatial)
library(vegdata)

### Start ###
rm(list = ls())
# installr::updateR(
#   browse_news = FALSE,
#   install_R = TRUE,
#   copy_packages = TRUE,
#   copy_site_files = TRUE,
#   keep_old_packages = FALSE,
#   update_packages = FALSE,
#   start_new_R = FALSE,
#   quit_R = TRUE
#   )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Load data #################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Sites ####################################################################


sites_reference <- read_csv2(
  here("data", "raw", "data_raw_sites_reference.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(
      .default = "?",
      aufnahmedatum_2021 = col_date(format = "%d.%m.%Y")
    )
) %>%
  rename(
    location = verortung,
    botanist = botaniker_2021,
    survey_date = aufnahmedatum_2021,
    cover_vegetation = vegetationsdeckung_2021,
    height_vegetation = vegetationshoehe_2021
  ) %>%
  filter(!str_detect(location, "rollfeld")) %>%
  select(
    plot, location, botanist, survey_date, cover_vegetation, height_vegetation
    ) %>%
  mutate(
    location = str_replace(location, "nord", "north"),
    location = str_replace(location, "mitte", "middle"),
    location = str_replace(location, "sued", "south"),
    year = year(survey_date)
  )

sites_restoration <- read_csv(
  here("data", "raw", "data_raw_sites_restoration.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(
    .default = "?",
    survey_date_2024 = col_date()
  )
) %>%
  rename(
    botanist = botanist_2024,
    survey_date = survey_date_2024,
    cover_vegetation = cover_vegetation_2024,
    height_vegetation = height_vegetation_2024
    ) %>%
  select(
    -cover_moss_2024, -cover_litter_2024, -comments
    ) %>%
  mutate(year = year(survey_date)) %>%
  filter(!(plot %in% c(
    "res49",  # Site did not receive hay transfer
    "res29", "res30", "res31", "res32" # Mowing regime changed after 2020
  )))

sites_bauer_etal_2020 <- read_csv(
  here("data", "raw", "data_raw_sites_bauer_etal-2020.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(
      .default = "?"
    )
) %>%
  rename(
    id = ID, plot_size = plotSize, cover_vegetation = herbCover,
    height_vegetation = herbHeight, location = block
    ) %>%
  filter(dataset == "blocks") %>%
  select(
    id, plot, location, plot_size, botanist, year,
    cover_vegetation, height_vegetation
    )



## 2 Species ##################################################################


species_reference <- read_csv2(
  here("data", "raw", "data_raw_species_reference.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types = cols(.default = "?")
  ) %>%
  mutate(name = str_replace_all(name, "_", " "))

species_restoration <- read_csv(
  here("data", "raw", "data_raw_species_restoration.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types = cols(.default = "?")
  )

species_bauer_etal_2020 <- read_csv(
  here("data", "raw", "data_raw_species_bauer_etal-2020.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types = cols(.default = "?")
) %>%
  mutate(name = str_replace_all(name, "_", " ")) %>%
  select(name, starts_with("X03"), starts_with("X18"))



## 3 Coordinates ###############################################################

# The following coordinates have an embargo due to occurences of extremely rare species

coordinates_reference <- read_csv2(
  here("data", "raw", "data_raw_coordinates_reference.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types = cols(.default = "?")
  ) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 25832) %>%
  sf::st_transform(4326) %>%
  mutate(
    latitude = sf::st_coordinates(.)[, 2],
    longitude = sf::st_coordinates(.)[, 1]
    ) %>%
  sf::st_drop_geometry()

coordinates_restoration <- read_csv(
  here("data", "raw", "data_raw_coordinates_restoration.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types = cols(.default = "?")
  ) %>%
  mutate(
    id = paste0("X2024", plot),
    longitude = longitude / 100000,
    latitude = latitude / 100000
    ) %>%
  select(id, plot, latitude, longitude)

coordinates_bauer_etal_2020 <- read_csv(
  here("data", "raw", "data_raw_sites_bauer_etal-2020.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types = cols(.default = "?")
) %>%
  filter(dataset == "blocks") %>%
  select(ID, longitude, latitude) %>%
  rename(id = ID) %>%
  mutate(
    id = str_replace(id, "^X03", "X2003roeder"),
    id = str_replace(id, "^X18", "X2018roeder")
  ) %>%
  filter()

coordinates <- coordinates_reference %>%
  bind_rows(coordinates_restoration) %>%
  bind_rows(coordinates_bauer_etal_2020)



## 4 FloraVeg.EU species #######################################################

# Chytrý et al. (2020) Appl Veg Sci https://doi.org/10.1111/avsc.12519
# Version 2021-06-01: https://doi.org/10.5281/zenodo.4812736


traits <- readxl::read_excel(
  here(
    "data", "raw",
    "Characteristic-species-combinations-EUNIS-habitats-2021-06-01.xlsx"
    ),
  col_names = TRUE, na = c("", "NA", "na")
)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Combine reference and restoration plots ##################################


sites <- sites_reference %>%
  bind_rows(sites_restoration) %>%
  bind_rows(sites_bauer_etal_2020) %>%
  mutate(
    id = str_replace(id, "^X03", "X2003roeder"),
    id = str_replace(id, "^X18", "X2018roeder"),
    id = if_else(str_detect(plot, "^tum"), paste0("X2021", plot), id),
    id = if_else(str_detect(plot, "^res"), paste0("X2024", plot), id),
    elevation = if_else(is.na(elevation), 469, elevation),
    plot_size = if_else(is.na(plot_size), 4, plot_size),
    treatment = if_else(is.na(treatment), str_c("control_", year), treatment)
  ) %>%
  arrange(id) %>%
  relocate(id, .before = "plot") %>%
  relocate(
    elevation, plot_size, treatment, mowing, survey_date, year,
    .after = "location"
    )

species <- species_reference %>%
  full_join(species_restoration, by = "name") %>%
  full_join(species_bauer_etal_2020, by = "name") %>%
  pivot_longer(-name, names_to = "plot", values_to = "value") %>%
  mutate(
    id = if_else(str_detect(plot, "^res"), paste0("X2024", plot), plot),
    id = str_replace(id, "^X03", "X2003roeder"),
    id = str_replace(id, "^X18", "X2018roeder")
  ) %>%
  filter(!is.na(value)) %>%
  select(-plot) %>%
  arrange(id) %>%
  semi_join(sites, by = "id") %>%
  pivot_wider(names_from = "id", values_from = "value") %>%
  arrange(name)

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 2 Select target species #####################################################


# Chytrý et al. (2024) FloraVeg.EU. Appl Veg Sci 27, e12798.
# https://doi.org/10.1111/avsc.12798

data <- traits %>%
  rename_with(~ tolower(gsub(" ", "_", .x))) %>%
  filter(
    habitat_code %in% c("R1A", "R22") &
      species_type %in% c("Diagnostic", "Constant")
    ) %>%
  select(species, habitat_code, species_type) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = "habitat_code", values_from = "value") %>%
  group_by(species) %>%
  summarize(across(c("R1A", "R22"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(
    across(c("R1A", "R22"), ~ if_else(. > 0, 1, 0)),
    both = if_else(R1A > 0 & R22 > 0, 1, 0)
    ) %>%
  rename(name = species)
traits <- data

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 3 Names from TNRS database #################################################


### a Harmonize species and traits matrices ------------------------------------

metadata <- TNRS_metadata()
metadata$version
metadata$sources %>% tibble()

data_species <- species %>%
  mutate(
    name = str_replace(name, "Anemone pulsatilla", "Pulsatilla vulgaris Mill."),
    name = str_replace(name, "Carex cary eric", "Carex spp_cary_eric"),
    name = str_replace(name, "Carlina vulgaris aggr.", "Carlina vulgaris L."),
    name = str_replace(name, "Centaurea pannonica", "Centaurea jacea"),
    name = str_replace(
      name, "Cirsium acaulon", "Cirsium acaule (L.) A.A.Weber ex Wigg."
      ),
    name = str_replace(name, "Erica herbacea", "Erica carnea"),
    name = str_replace(
      name, "Euphrasia officinalis ssp rostkoviana", "Euphrasia officinalis"
      ),
    name = str_replace(
      name, "Festuca arundinacea", "Festuca arundinacea Schreb."
      ),
    name = str_replace(name, "Inula hirta", "Inula hirta L."),
    name = str_replace(name, "Inula salicina", "Inula salicina L."),
    name = str_replace(name, "Knautia arvensis", "Knautia arvensis L."),
    name = str_replace(
      name, "Lotus corniculatus ssp corniculatus", "Lotus corniculatus"
      ),
    name = str_replace(
      name, "Lotus corniculatus ssp hirsutus", "Lotus corniculatus"
      ),
    name = str_replace(
      name, "Lotus corniculatus var corniculatus", "Lotus corniculatus"
      ),
    name = str_replace(
      name, "Lotus corniculatus var hirsutus", "Lotus corniculatus"
      ),
    name = str_replace(
      name, "Linum catharticum subsp. suecicum", "Linum catharticum"
      ),
    name = str_replace(
      name, "Picris hieracioides", "Picris hieracioides L."
      ),
    name = str_replace(name, "Pilosella macrantha", "Pillosella hopeana"),
    name = str_replace(name, "Potentilla incana", "Potentilla cinerea"),
    name = str_replace(name, "Potentilla tabernaemontani", "Potentilla verna"),
    name = str_replace(name, "Pulicaria vulgaris", "Pulicaria vulgaris Gaertn"),
    name = str_replace(name, "Pulsatilla grandis", "Pulsatilla grandis Wender."),
    name = str_replace(name, "Stachys officinalis", "Betonica officinalis"),
    name = str_replace(
      name, "Taraxacum sect. Taraxacum", "Taraxacum campylodes"
      ),
    name = str_replace(name, "Taraxacum$", "Taraxacum campylodes")
    )

data_traits <- traits %>%
  full_join(data_species %>% select(name), by = "name")

# Harmonization ran once and were than saved --> load below processed file

# harmonized_names <- data_traits %>%
#   rowid_to_column("id") %>%
#   select(id, name) %>%
#   TNRS::TNRS(
#     sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
#     classification = "wfo", # family classification WFO
#     mode = "resolve"
#   )
# write.csv(
#   harmonized_names,
#   here("data", "processed", "data_processed_harmonized_taxonomy.csv")
#   )

data_names <- read.csv(
  here("data", "processed", "data_processed_harmonized_taxonomy.csv")
  ) %>%
  select(
    Name_submitted, Taxonomic_status, Accepted_name, Accepted_name_rank,
    Overall_score, Warnings, WarningsEng,
    Accepted_name_url, Accepted_family, Source
  ) %>%
  rename_with(tolower) %>%
  arrange(desc(warnings), overall_score)


### b Summarize species matrix -------------------------------------------------

data_joined <- data_species %>% 
  rename(name_submitted = name) %>%
  left_join(
    data_names %>% select(name_submitted, accepted_name),
    by = "name_submitted"
  ) %>%
  select(name_submitted, accepted_name, everything())

data_joined %>% filter(duplicated(accepted_name))

data_summarized <- data_joined %>%
  group_by(accepted_name) %>%
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

data_summarized %>% filter(duplicated(accepted_name))

species <- data_summarized %>%
  pivot_longer(-accepted_name, names_to = "id", values_to = "cover") %>%
  group_by(accepted_name) %>%
  mutate(sum = sum(cover, na.rm = TRUE)) %>%
  filter(sum > 0) %>%
  select(-sum) %>%
  ungroup() %>% 
  pivot_wider(names_from = "id", values_from = "cover")


### c Summarize traits matrix --------------------------------------------------

data_joined <- data_traits %>% 
  rename(name_submitted = name) %>%
  left_join(data_names, by = "name_submitted") %>%
  select(
    name_submitted, accepted_name, taxonomic_status, accepted_name_rank,
    accepted_family, R1A, R22, both, accepted_name_url
    )

data_joined %>% filter(duplicated(accepted_name))

data_summarized <- data_joined %>%
  group_by(
    accepted_name, accepted_name_rank, accepted_family, accepted_name_url
    ) %>%
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

data_summarized %>% filter(duplicated(accepted_name))

traits <- data_summarized %>%
  semi_join(species, by = "accepted_name") %>%
  relocate(accepted_name_url, .after = last_col()) %>%
  mutate(across(where(is.numeric), replace_na, 0))

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 4 Get red list status ######################################################


### a Load red list ------------------------------------------------------------

data_redlist <- readxl::read_excel(
  here("data", "raw", "data_raw_species_redlist_2018.xlsx"),
  col_names = TRUE, na = c("", "NA", "na")
  ) %>%
  rename(redlist_germany = "RL Kat.", responsibility = Verantwortlichkeit) %>%
  rename_with(tolower) %>%
  select(name, status, redlist_germany) %>%
  mutate(
    name = str_replace(
      name, "Cirsium acaulon \\(L\\.\\) Scop\\.", "Cirsium acaule"
      )
    )

# Calculate just once to save time (afterwards load file below)
#
# harmonized_names <- data_redlist %>%
#   rowid_to_column("id") %>%
#   select(id, name) %>%
#   TNRS::TNRS(
#     sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
#     classification = "wfo", # family classification
#     mode = "resolve"
#   )
# write_csv(
#   harmonized_names,
#   here("data", "processed", "data_processed_harmonized_redlist.csv")
#   )

redlist <- read_csv(
  here("data", "processed", "data_processed_harmonized_redlist.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(.default = "?")
  ) %>%
  select(
    Name_submitted, Taxonomic_status, Accepted_name, Accepted_name_url,
    Accepted_family
    ) %>%
  rename_with(tolower) %>%
  full_join(
    data_redlist %>% rename(name_submitted = name), by = "name_submitted"
    )

redlist %>% filter(duplicated(accepted_name))

redlist_summarized <- redlist %>%
  group_by(accepted_name) %>%
  summarize(across(c(status, redlist_germany), ~ first(.x)))

redlist_summarized  %>% filter(duplicated(accepted_name))


### b Combine red list status and traits --------------------------------------

data <- traits %>%
  left_join(redlist_summarized, by = "accepted_name") %>%
  relocate(accepted_name_url, .after = last_col())
traits <- data

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 5 Traits from GIFT database ################################################


### a Load traits from GIFT ---------------------------------------------------

trait_ids <- c("1.2.2", "1.6.3", "3.2.3", "4.1.3")

GIFT::GIFT_traits_meta() %>%
  filter(Lvl3 %in% trait_ids) %>%
  tibble()

# Download of traits data ran once and were than saved --> load below processed file

# data_gift <- GIFT::GIFT_traits(
#   trait_IDs = trait_ids,
#   agreement = 0.66, bias_ref = FALSE, bias_deriv = FALSE
# )
# 
# write_csv(
#   data_gift, here("data", "processed", "data_processed_gift_traits.csv")
#   )

gift <- data.table::fread(
  here("data", "processed", "data_processed_gift_traits.csv")
) %>%
  rename(
    accepted_name = work_species,
    growth_form = trait_value_1.2.2,
    height = trait_value_1.6.3,
    seedmass = trait_value_3.2.3,
    sla = trait_value_4.1.3
  ) %>%
  select(accepted_name, growth_form, sla, height, seedmass) %>%
  mutate(
    accepted_name = str_replace(
      accepted_name, "Asperula cynanchica",
      "Cynanchica pyrenaica subsp. cynanchica"
    ),
    accepted_name = str_replace(
      accepted_name, "Avenula pubescens", "Helictotrichon pubescens"
    ),
    accepted_name = str_replace(
      accepted_name, "Euphrasia officinalis subsp. pratensis",
      "Euphrasia officinalis"
    ),
    accepted_name = str_replace(
      accepted_name, "Helianthemum nummularium",
      "Helianthemum nummularium subsp. obscurum"
    ),
    accepted_name = str_replace(
      accepted_name, "Lotus dorycnium", "Lotus germanicus"
    )
  )

### b Combine gift and traits -------------------------------------------------

data_traits <- traits %>%
  left_join(
    gift %>%
      select(accepted_name, growth_form, sla, height, seedmass),
    by = "accepted_name"
  )

### c Check completeness -------------------------------------------------------

data <- data_traits %>%
  filter(accepted_name_rank %in% c("subspecies", "species")) %>%
  ungroup() %>% 
  select(accepted_name, sla, seedmass, height)

data %>%
  naniar::miss_var_summary(order = TRUE)
data %>%
  naniar::vis_miss(cluster = FALSE, sort_miss = TRUE)
data %>%
  filter(is.na(sla) & is.na(height) & is.na(seedmass)) %>%
  filter(str_detect(accepted_name, " "))


### d Add values manually ------------------------------------------------------

data_traits2 <- data_traits %>%
  mutate(
    sla = if_else(
      accepted_name == "Festuca rupicola",
      gift %>%
        filter(accepted_name == "Festuca ovina") %>%
        pull(sla),
      sla
    ),
    sla = if_else(
      accepted_name == "Medicago falcata",
      gift %>%
        filter(accepted_name == "Medicago sativa") %>%
        pull(sla),
      sla
      ),
    sla = if_else(
      accepted_name == "Rhinanthus glacialis",
      gift %>%
        filter(accepted_name == "Rhinanthus minor") %>%
        pull(sla),
      sla
      ),
    height = if_else(
      accepted_name == "Medicago falcata",
      gift %>%
        filter(accepted_name == "Medicago sativa") %>%
        pull(height),
      height
      ),
    height = if_else(accepted_name == "Allium suaveolens", 0.35, height), # Rothmaler DOI 10.1007/978-3-662-49710-4
    height = if_else(accepted_name == "Erica carnea", 0.225, height), # Rothmaler DOI 10.1007/978-3-662-49710-4
    height = if_else(accepted_name == "Euphrasia picta", 0.15, height), # Rothmaler DOI 10.1007/978-3-662-49710-4
    height = if_else(accepted_name == "Lolium arundinaceum", 1.20, height), # Rothmaler DOI 10.1007/978-3-662-49710-4
    height = if_else(accepted_name == "Orobanche gracilis", 0.25, height), # Rothmaler DOI 10.1007/978-3-662-49710-4
    height = if_else(accepted_name == "Pentanema hirta", 0.30, height), # Rothmaler DOI 10.1007/978-3-662-49710-4
    height = if_else(accepted_name == "Pentanema salicinum", 0.525, height), # Rothmaler DOI 10.1007/978-3-662-49710-4
    height = if_else(accepted_name == "Seseli annuum", 0.50, height), # Rothmaler DOI 10.1007/978-3-662-49710-4
    height = if_else(accepted_name == "Taraxacum campylodes", 0.275, height), # Rothmaler DOI 10.1007/978-3-662-49710-4
    seedmass = if_else(accepted_name == "Ophrys apifera", 0.000005, seedmass),
    seedmass = if_else(accepted_name == "Platanthera bifolia", 0.000005, seedmass)
  )


### e Check completeness II ----------------------------------------------------

data <- data_traits2 %>%
  filter(
    accepted_name_rank %in% c("subspecies", "species"),
    !(growth_form %in% c("shrub", "tree"))
    ) %>%
  ungroup() %>% 
  select(accepted_name, sla, seedmass, height)

data %>%
  naniar::miss_var_summary(order = TRUE)
data %>%
  naniar::vis_miss(cluster = FALSE, sort_miss = TRUE)
data %>%
  filter(is.na(sla) | is.na(height) | is.na(seedmass)) %>%
  arrange(sla, seedmass, height) %>%
  print(n = 34)

data %>%
  select(accepted_name, sla) %>%
  full_join(species, by = "accepted_name") %>%
  pivot_longer(-c(accepted_name, sla), names_to = "id", values_to = "cover") %>%
  mutate(group = if_else(is.na(sla), "missing_value", "value")) %>%
  group_by(group) %>%
  summarize(sum = sum(cover, na.rm = TRUE)) %>%
  mutate(ratio = sum / (3143 + 34461))

data <- data_traits %>%
  filter(
    accepted_name_rank %in% c("subspecies", "species") &
      !(growth_form %in% c("tree", "shrub")) &
      !(accepted_name %in% c(
        "Acer platanoides"
      ))
  ) %>%
  ungroup() %>% 
  select(accepted_name, sla, seedmass, height)

traits <- data_traits %>% ungroup()

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 6 Create variables #########################################################


### Grass cover ---------------------------------------------------------------

data <- species %>%
  left_join(
    traits %>% select(accepted_name, accepted_family), by = "accepted_name"
    ) %>%
  filter(accepted_family == "Poaceae") %>%
  select(-accepted_name, -accepted_family) %>%
  summarize(across(where(is.numeric), ~ sum(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "id", values_to = "grass_cover")
sites <- sites %>%
  left_join(data, by = "id")


### Graminoid cover ---------------------------------------------------------------

data <- species %>%
  left_join(
    traits %>% select(accepted_name, accepted_family), by = "accepted_name"
  ) %>%
  filter(
    accepted_family == "Poaceae" |
      accepted_family == "Cyperaceae" |
      accepted_family == "Juncaceae"
    ) %>%
  select(-accepted_name, -accepted_family) %>%
  summarize(across(where(is.numeric), ~ sum(., na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(), names_to = "id", values_to = "graminoid_cover"
    )
sites <- sites %>%
  left_join(data, by = "id")



## 7 Alpha diversity ##########################################################


### a Species richness --------------------------------------------------------

richness <- species %>%
  left_join(traits, by = "accepted_name") %>%
  select(
    accepted_name, status, redlist_germany, R1A, R22, both, starts_with("X")
  ) %>%
  pivot_longer(
    cols = starts_with("X"),  
    names_to = "plot_id",     
    values_to = "n"           
  ) %>%
  mutate(n = if_else(n > 0, 1, 0))

richness_total <- richness %>%
  group_by(plot_id) %>%
  summarise(richness_total = sum(n, na.rm = TRUE))

richness_R1A <- richness %>%
  mutate(R1A_occ = R1A * n) %>%
  group_by(plot_id) %>%
  summarise(richness_R1A = sum(R1A_occ, na.rm = TRUE)) 
  
richness_R22 <- richness %>%
  mutate(R22_occ = R22 * n) %>%
  group_by(plot_id) %>%
  summarise(richness_R22 = sum(R22_occ, na.rm = TRUE)) 

richness_both <- richness %>%
  mutate(both_occ = both * n) %>%
  group_by(plot_id) %>%
  summarise(richness_both = sum(both_occ, na.rm = TRUE))

richness_rlg <- richness %>%
  filter(n != 0) %>%
  group_by(plot_id, redlist_germany) %>%
  summarise(richness_rlg = n()) %>%
  pivot_wider(names_from = redlist_germany, values_from = richness_rlg) 

richness_rlg[is.na(richness_rlg)] <- 0

richness_rlg <- richness_rlg %>%
  rename(rlg_LC = `*`, 
         rlg_VU = `3`,
         rlg_EN = `2`,
         rlg_CR = `1`,
         rlg_NT = `V`,
         rlg_NE = `nb`,
         rlg_NA = `NA`)
         
full_richness <- full_join(richness_total, richness_R1A, by = "plot_id") %>%
  full_join(richness_R22, by = "plot_id") %>%
  full_join(richness_both, by = "plot_id") %>%
  full_join(richness_rlg, by = "plot_id") %>%
  rename(id = plot_id)

sites <- sites %>%
  left_join(full_richness, by = "id")

sites$rlg <- sites$rlg_CR + sites$rlg_EN + sites$rlg_VU + sites$rlg_NT

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))


### b Species eveness ---------------------------------------------------------

data <- as.data.frame(t(species))
colnames(data) <- data[1,]
data <- data[-1,]

data <- data %>% 
  mutate(across(where(is.character), str_trim)) %>%
  rownames_to_column(var = "id")

data <- data.frame(data[1], sapply(data[-1], as.numeric)) %>%
  column_to_rownames(var = "id")

shannon <- data %>%
  diversity(index = "shannon") %>%
  as.data.frame() %>%
  rownames_to_column(var = "id") %>%
  rename(shannon = ".")

sites <- sites %>%
  left_join(shannon, by = "id")

sites$evenness <- sites$shannon / log(sites$richness_total)

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 8 Calculation of CWMs ######################################################


traits_without_trees <- traits %>%
  filter(is.na(growth_form) | growth_form != "tree") %>%
  filter(accepted_name != "Prunus spinosa") %>%
  filter(accepted_name_rank %in% c("species", "subspecies")) %>%
  mutate(
    seed_mass = log(seedmass),
    sla = log(sla)
      )


### a CWM Plant height 1.6.3 --------------------------------------------------

traits_height <- traits_without_trees[,c("accepted_name", "height")]
traits_height <- na.omit(traits_height)

species_height <- species[species$accepted_name %in% traits_height$accepted_name, ]
species_height <- as.data.frame(t(species_height))
colnames(species_height) <- species_height[1,] 
species_height <- species_height[-1,]
species_height <- species_height %>% mutate_all(as.numeric)

traits_height <- traits_height %>%
  column_to_rownames(var = "accepted_name")

CWM.Height <- dbFD(
  traits_height, species_height, 
  w.abun = TRUE, corr = "lingoes", calc.FRic = FALSE, calc.CWM = TRUE
)

# Mean value
CWM_Height <- CWM.Height$CWM
colnames(CWM_Height) <- "CWM_Height"
CWM_Height$id <- row.names(CWM_Height)


### b CWM Seed mass 3.2.3 -----------------------------------------------------

traits_seed <- traits_without_trees[,c("accepted_name", "seedmass")]
traits_seed <- na.omit(traits_seed)

species_seed <- species[species$accepted_name %in% traits_seed$accepted_name, ]
species_seed <- as.data.frame(t(species_seed))
colnames(species_seed) <- species_seed[1,] 
species_seed <- species_seed[-1,]
species_seed <- species_seed %>% mutate_all(as.numeric)
data_test <- species_seed %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))

traits_seed <- traits_seed %>%
  column_to_rownames(var = "accepted_name")

CWM.Seed <- dbFD(
  traits_seed, species_seed, 
  w.abun = TRUE, corr = "lingoes", calc.FRic = FALSE, calc.CWM = TRUE
)

# Mean value
CWM_Seed <- CWM.Seed$CWM
colnames(CWM_Seed) <- "CWM_Seed"
CWM_Seed$id <- row.names(CWM_Seed)


### c CWM SLA 4.1.3 -----------------------------------------------------------

traits_SLA <- traits_without_trees[,c("accepted_name", "sla")]
traits_SLA <- na.omit(traits_SLA)

species_SLA <- species[species$accepted_name %in% traits_SLA$accepted_name, ]
species_SLA <- as.data.frame(t(species_SLA))
colnames(species_SLA) <- species_SLA[1,] 
species_SLA <- species_SLA[-1,]
species_SLA <- species_SLA %>% mutate_all(as.numeric)

traits_SLA <- traits_SLA %>%
  column_to_rownames(var = "accepted_name")

CWM.SLA <- dbFD(
  traits_SLA, species_SLA, 
  w.abun = TRUE, corr = "lingoes", calc.FRic = FALSE, calc.CWM = TRUE
)

# Mean value
CWM_SLA <- CWM.SLA$CWM
colnames(CWM_SLA) <- "CWM_SLA"
CWM_SLA$id <- row.names(CWM_SLA)


### d Add to sites table ------------------------------------------------------

CWM <- full_join(CWM_Height, CWM_Seed, by = "id") %>%
  full_join(CWM_SLA, by = "id")

sites <- sites %>%
  left_join(CWM)


rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 9 ESy: EUNIS expert vegetation classification system ########################


### a Preparation --------------------------------------------------------------

# Markus: Calculate again with harmonized species names

expertfile <- "EUNIS-ESy-2020-06-08.txt" ### file of 2021 is not working

obs <- species %>%
  pivot_longer(
    cols = -accepted_name,
    names_to = "RELEVE_NR",
    values_to = "Cover_Perc"
  ) %>%
  rename(TaxonName = "accepted_name") %>%
  mutate(
    TaxonName = str_replace_all(TaxonName, "_", " "),
    TaxonName = str_replace_all(TaxonName, "ssp", "subsp."),
    TaxonName = as.factor(TaxonName),
    RELEVE_NR = as.factor(RELEVE_NR)
  ) %>%
  filter(!is.na(Cover_Perc) & Cover_Perc != 0) %>%
  data.table::as.data.table()


# Coordinates are in WGS84
header <- sites %>%
  left_join(coordinates %>% select(-plot), by = "id") %>%
  rename(
    RELEVE_NR = id,
    Latitude = latitude,
    Longitude = longitude,
    "Altitude (m)" = elevation
  ) %>%
  mutate(
    RELEVE_NR = as.factor(RELEVE_NR),
    Country = "Germany",
    Coast_EEA = "N_COAST",
    Dunes_Bohn = "N_DUNES",
    Ecoreg = 686,
    dataset = "garchinger_heide"
  ) %>%
  select(
    RELEVE_NR, "Altitude (m)", Latitude, Longitude, Country,
    Coast_EEA, Dunes_Bohn, Ecoreg, dataset
  )


### b Calculation --------------------------------------------------------------

### Bruelheide et al. 2021 Appl Veg Sci
### https://doi.org/10.1111/avsc.12562

setwd(here("R", "esy"))
source(here("R", "esy", "code", "prep.R"))

#### Step 1 and 2: Load and parse the expert file ###
source(here("R", "esy", "code", "step1and2_load-and-parse-the-expert-file.R"))

#### Step 3: Create a numerical plot x membership condition matrix  ###
plot.cond <- array(
  0,
  c(length(unique(obs$RELEVE_NR)), length(conditions)),
  dimnames = list(
    as.character(unique(obs$RELEVE_NR)),
    conditions
  )
)

### Step 4: Aggregate taxon levels ###
source(here("R", "esy", "code", "step4_aggregate-taxon-levels.R"))

(data <- obs %>%
    group_by(TaxonName) %>%
    slice(1) %>%
    anti_join(AGG, by = c("TaxonName" = "ind")))

#### Step 5: Solve the membership conditions ###
mc <- 1
source(
  here(
    "R", "esy", "code", "step3and5_extract-and-solve-membership-conditions.R"
  )
)


### c Summary and integration --------------------------------------------------

table(result.classification)
eval.EUNIS(which(result.classification == "R18")[1])
# H26a (= U27) = Temperate, lowland to montane base-rich scree
# R = Grassland
# R18 = Perennial rocky calcareous grassland of subatlantic-submediterranean Europe
# R1A = Semi-dry perennial calcareous grassland (meadow steppe)
# R22 = Low and medium altitude hay meadow
# S22 =  Alpine and subalpine ericoid heath

data <- sites %>%
  mutate(
    esy = result.classification,
    esy = if_else(id == "X2003roederM13", "R", esy), # ?
    esy = if_else(id == "X2021tum11", "R1A", esy), # S22
    esy = if_else(id == "X2021tum40", "R", esy), # S22
    esy = if_else(id == "X2021tum52", "R1A", esy), # S22
    esy = if_else(id == "X2021tum58", "R1A", esy), # S22
    esy = if_else(id == "X2021tum63", "R1A", esy), # S22
    esy = if_else(id == "X2024res07", "R1A", esy), # S22
    esy = if_else(id == "X2024res41", "R1A", esy), # H26a
    esy = if_else(id == "X2024res44", "R1A", esy), # H26a
    esy = if_else(id == "X2024res57", "R", esy), # H26a
    esy = if_else(id == "X2024res59", "R", esy), # H26a
    esy = if_else(id == "X2024res61", "R", esy), # H26a
    esy = if_else(id == "X2024res88", "R", esy) # R18
  )
table(data$esy)
sites <- data


rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 10 dbMEM: Distance-based Moran's eigenvector maps ############################


# Borcard et al. (2018) Numerical Ecology https://doi.org/10.1007/978-1-4419-7976-6
# function 'quickMEM' from Numerical Ecology textbook:

source("https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/NumEcolR2/quickMEM.R")
data_sites <- sites %>%
  left_join(coordinates, by = "id") %>%
  select(id, longitude, latitude) %>%
  arrange(id)
  
data_species <- species %>%
  pivot_longer(-accepted_name, names_to = "id", values_to = "value") %>%
  pivot_wider(id_cols = id, names_from = "accepted_name", values_from = "value") %>%
  arrange(id) %>%
  semi_join(data_sites, by = "id") %>%
  column_to_rownames(var = "id") %>%
  decostand("hellinger")

data_sites <- data_sites %>%
  column_to_rownames("id")

m <- quickMEM(
  data_species, data_sites,
  alpha = 0.05,
  method = "fwd",
  rangexy = TRUE,
  perm.max = 9999
)

# OUTPUT (2025-07-14)
# --
#   A significant linear trend has been found in the response data.
# The response data have been detrended prior to dbMEM analysis.
# --
#   20 dbMEM eigenvectors have been produced 
# R2 of global model =  0.1382 
# Adjusted R2 of global model =  0.058 
# 5  dbMEM eigenvectors have been selected 
# R2 of minimum (final) model =  0.0753                      
# Adjusted R2 of minimum (final) model =  0.0552          
# The final model has 2 significant canonical axes 
# --

m$RDA_test # p: 0.001
m$RDA_axes_test #  RDA1 and RDA2: sig. axis
m$RDA # Eigenvalues: RDA1: 0.02, RDA2: 0.006

data <- m$dbMEM %>%
  rownames_to_column(var = "id") %>%
  select(id, MEM1, MEM2) %>%
  rename(mem1 = MEM1, mem2 = MEM2)
sites <- sites %>%
  left_join(data, by = "id")

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 11 Final selection of variables ############################################


sites <- sites %>%
  relocate(botanist, .after = last_col()) %>%
  select(-rlg_LC, -rlg_NA, -rlg_NE)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save processed data #######################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



write_csv(
  sites,
  here("data", "processed", "data_processed_sites.csv")
)
write_csv(
  species,
  here("data", "processed", "data_processed_species.csv")
)
write_csv(
  traits,
  here("data", "processed", "data_processed_traits.csv")
)
