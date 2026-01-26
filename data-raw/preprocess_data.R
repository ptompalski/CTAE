# code to prepare the package data files containing model parameters

library(tidyverse)


# data - merchantability  criteria ####

merchcrit <- tibble::tribble(
  ~Province , ~Species    , ~BEC_group     , ~StumpHT , ~TopDBH , ~MinDBH ,
  # --- Non-BC stays as-is, BEC_group = NA ---
  "NL"      , "ALL"       , NA_character_  ,       15 ,  7.6    ,  9.0    ,
  "NS"      , "ALL"       , NA_character_  ,       15 ,  7.0    ,  9.0    ,
  "PE"      , "ALL"       , NA_character_  ,       15 ,  8.0    ,  9.0    ,
  "NB"      , "ALL"       , NA_character_  ,       15 ,  8.0    ,  9.1    ,
  "QC"      , "ALL"       , NA_character_  ,       15 ,  9.0    ,  9.0    ,
  "ON"      , "ALL"       , NA_character_  ,       30 ,  7.0    ,  9.0    ,
  "MB"      , "ALL"       , NA_character_  ,       30 ,  7.6    ,  9.1    ,
  "SK"      , "ALL"       , NA_character_  ,       30 ,  7.0    ,  7.0    ,
  "AB"      , "ALL"       , NA_character_  ,       30 ,  7.0    , 13.0    ,
  "YT"      , "ALL"       , NA_character_  ,       30 , 10.0    , 15.0    ,
  "NT"      , "ALL"       , NA_character_  ,       30 , 10.2    , 10.2    ,

  # --- BC: now BEC-aware ---
  # Coast wet (e.g., CWH/MH typical mature coastal utilization)
  "BC"      , "THUJ.PLI"  , "Coast_wet"    ,       30 , 15.0    , 17.5    ,
  "BC"      , "TSUG.HET"  , "Coast_wet"    ,       30 , 15.0    , 17.5    ,
  "BC"      , "PSEU.MEN"  , "Coast_wet"    ,       30 , 15.0    , 17.5    ,
  "BC"      , "ABIE.AMA"  , "Coast_wet"    ,       30 , 15.0    , 17.5    ,
  "BC"      , "PICE.SPP"  , "Coast_wet"    ,       30 , 15.0    , 17.5    ,

  # Coast dry / transition (CDF etc.)
  "BC"      , "ALNU.RUB"  , "Coast_dry"    ,       30 , 10.0    , 12.5    ,
  "BC"      , "PICE.SPP"  , "Coast_dry"    ,       30 , 10.0    , 17.5    ,
  "BC"      , "TSUG.HET"  , "Coast_dry"    ,       30 , 10.0    , 17.5    ,
  "BC"      , "PSEU.MEN"  , "Coast_dry"    ,       30 , 10.0    , 17.5    ,
  "BC"      , "ABIE.AMA"  , "Coast_dry"    ,       30 , 10.0    , 17.5    ,

  # Interior (default interior conifer)
  "BC"      , "PICE.SPP"  , "Interior_wet" ,       30 , 10.0    , 17.5    ,
  "BC"      , "PICE.SPP"  , "Interior_dry" ,       30 , 10.0    , 17.5    ,
  "BC"      , "PINU.CON"  , "Interior_dry" ,       30 , 10.0    , 12.5    ,

  # --- BC: BEC-independent conservative fallback layer ---
  # Use when BEC_zone is missing/unknown (conservative: TopDBH=15, MinDBH=17.5)
  "BC"      , "THUJ.PLI"  , "UNKNOWN"      ,       30 , 15.0    , 17.5    ,
  "BC"      , "TSUG.HET"  , "UNKNOWN"      ,       30 , 15.0    , 17.5    ,
  "BC"      , "PSEU.MEN"  , "UNKNOWN"      ,       30 , 15.0    , 17.5    ,
  "BC"      , "ABIE.AMA"  , "UNKNOWN"      ,       30 , 15.0    , 17.5    ,
  "BC"      , "PICE.SPP"  , "UNKNOWN"      ,       30 , 15.0    , 17.5    ,
  # Broadleaf already in table; keep MinDBH as you had, but apply conservative TopDBH
  "BC"      , "ALNU.RUB"  , "UNKNOWN"      ,       30 , 15.0    , 12.5    ,

  # --- BC: additional conifers, BEC-independent conservative ---
  "BC"      , "ABIE.LAS"  , "UNKNOWN"      ,       30 , 15.0    , 17.5    , # Subalpine fir
  "BC"      , "LARI.OCC"  , "UNKNOWN"      ,       30 , 15.0    , 17.5    , # Western larch
  "BC"      , "PINU.PON"  , "UNKNOWN"      ,       30 , 15.0    , 17.5    , # Ponderosa pine
  "BC"      , "PINU.MON"  , "UNKNOWN"      ,       30 , 15.0    , 17.5    , # Western white pine
  "BC"      , "CHAM.NOOT" , "UNKNOWN"      ,       30 , 15.0    , 17.5    , # Yellow cedar

  # Catch-all: species unknown AND BEC unknown
  "BC"      , "ALL"       , "UNKNOWN"      ,       30 , 15.0    , 17.5
)

usethis::use_data(merchcrit, overwrite = T)

# old version, updated above:
# based on a csv file provided by Juha Metsaranta.
# merchcrit = read.csv("data-raw/MerchCrit.csv")

# merchcrit <- merchcrit %>%
#   mutate(
#     Province = standardize_province_code(Province),
#     Species = standardize_species_code(Species)
#   )

# data - model parameters for the Canadian national taper models (Ung et al 2013) ####
# original csv files with model parameters provided by Juha Metsaranta.

natdbh = read.csv("data-raw/NationalDBH.csv") # Canadian national taper models, DBH
parameters_NationalTaperModelsDBH <- natdbh %>%
  select(-me_eng) %>%
  mutate(Species = standardize_species_code(Species))
usethis::use_data(parameters_NationalTaperModelsDBH, overwrite = T)

natdbhht = read.csv("data-raw/NationalDBHHT.csv") #Canadian national taper models, DBH + H
parameters_NationalTaperModelsDBHHT <- natdbhht %>%
  select(-name_eng) %>%
  mutate(Species = standardize_species_code(Species))
usethis::use_data(parameters_NationalTaperModelsDBHHT, overwrite = T)

# data for the Honer model
# Entered manually from Honer et al 1983 paper.
parameters_Honer <- readxl::read_excel("data-raw/Honer1983_parameters.xlsx")
usethis::use_data(parameters_Honer, overwrite = T)

# data - model paramters for the regional models (several models) ####
# original csv files with model parameters provided by Juha Metsaranta.
# the original csv file includes parameters for multiple models. Data will be split by model for clarity.

regdbhht <- read.csv("data-raw/RegionalDBHHT.csv")
regdbhht <- regdbhht %>%
  mutate(
    Province = standardize_province_code(Province),
    Species = standardize_species_code(Species)
  )

# regdbhht %>% group_by(ModelName) %>% count()
# parameters_Honer <- regdbhht %>% filter(ModelName == "Honer") # incomplete - replaced by the parameters entered manually
parameters_Kozak88 <- regdbhht %>% filter(ModelName == "Kozak88")
# parameters_Kozak94 <- regdbhht %>% filter(ModelName == "Kozak94")

usethis::use_data(parameters_Kozak88, overwrite = T)

# data - Kozak 1994 models for BC
parameters_Kozak94 <- readxl::read_excel("data-raw/kozak1994_parameters.xlsx")
parameters_Kozak94 <-
  parameters_Kozak94 %>%
  mutate(
    bec_zone = bec_zone %>%
      str_remove_all("\\s*\\([^)]*\\)")
  ) %>%
  select(-source_page, -species_name, -species_code_bc, -n_sample) %>%
  rename(Species = species, Subregion = bec_zone)
usethis::use_data(parameters_Kozak94, overwrite = T)
