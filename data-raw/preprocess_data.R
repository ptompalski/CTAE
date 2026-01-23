# code to prepare the package data files containing model parameters
# original csv files provided by Juha Metsaranta.

library(tidyverse)


# data - merchantability  criteria ####
merchcrit = read.csv("data-raw/MerchCrit.csv")

merchcrit <- merchcrit %>%
  mutate(
    Province = standardize_province_code(Province),
    Species = standardize_species_code(Species)
  )
usethis::use_data(merchcrit, overwrite = T)


# data - model parameters for the Canadian national taper models (Ung et al 2013) ####

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


# data - model paramters for the regional models (several models) ####
# the original csv file includes parameters for multiple models. Data will be split by model for clarity.

regdbhht <- read.csv("data-raw/RegionalDBHHT.csv")
regdbhht <- regdbhht %>%
  mutate(
    Province = standardize_province_code(Province),
    Species = standardize_species_code(Species)
  )

regdbhht %>% group_by(ModelName) %>% count()

parameters_Honer <- regdbhht %>% filter(ModelName == "Honer")
parameters_Kozak88 <- regdbhht %>% filter(ModelName == "Kozak88")
parameters_Kozak94 <- regdbhht %>% filter(ModelName == "Kozak94")

usethis::use_data(parameters_Honer, overwrite = T)
usethis::use_data(parameters_Kozak88, overwrite = T)
usethis::use_data(parameters_Kozak94, overwrite = T)
