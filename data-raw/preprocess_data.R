# code to prepare the package data files containing model parameters
# original csv files sent by Juha Metsaranta.

library(tidyverse)


# data - merchantability  criteria
merchcrit = read.csv("data-raw/MerchCrit.csv")

merchcrit <- merchcrit %>%
  mutate(
    Province = standardize_province_code(Province),
    Species = standardize_species_code(Species)
  )
usethis::use_data(merchcrit, overwrite = T)


# data - model parameters for the Canadian national taper models (Ung et al 2013)

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
