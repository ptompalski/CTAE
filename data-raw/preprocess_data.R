# code to prepare the package data files containing model parameters

library(tidyverse)


# data - merchantability  criteria ####
# based on a csv file provided by Juha Metsaranta.
merchcrit = read.csv("data-raw/MerchCrit.csv")

merchcrit <- merchcrit %>%
  mutate(
    Province = standardize_province_code(Province),
    Species = standardize_species_code(Species)
  )
usethis::use_data(merchcrit, overwrite = T)


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
  select(-source_page, -species_name, -species_code_bc, -n_sample)
usethis::use_data(parameters_Kozak94, overwrite = T)
