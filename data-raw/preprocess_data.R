# code to prepare the package data files containing model parameters

library(tidyverse)
source("R/utils.R")

# data - merchantability  criteria ####

merchcrit <- tibble::tribble(
  ~Province , ~Species    , ~BEC_group     , ~StumpHT , ~TopDBH , ~MinDBH ,
  # --- Non-BC stays as-is, BEC_group = NA ---
  "NL"      , "ALL"       , NA_character_  ,       15 ,  7.6    ,  9.0    ,
  "NS"      , "ALL"       , NA_character_  ,       15 ,  7.0    ,  9.0    ,
  "PE"      , "ALL"       , NA_character_  ,       15 ,  8.0    ,  9.0    ,
  "NB"      , "ALL"       , NA_character_  ,       15 ,  8.0    ,  9.1    ,
  "QC"      , "ALL"       , NA_character_  ,       15 ,  9.0    ,  9.0    ,

  "MB"      , "ALL"       , NA_character_  ,       30 ,  7.6    ,  9.1    ,
  "SK"      , "ALL"       , NA_character_  ,       30 ,  7.0    ,  7.0    ,
  "AB"      , "ALL"       , NA_character_  ,       30 ,  7.0    , 13.0    ,
  "YT"      , "ALL"       , NA_character_  ,       30 , 10.0    , 15.0    ,
  "NT"      , "ALL"       , NA_character_  ,       30 , 10.2    , 10.2    ,

  # --- Ontario (ON): ----
  "ON"      , "ALL"       , NA_character_  ,       30 , 13.1    ,  9.0    , #conservative for unknown species,
  # Ontario (ON): species-group upper diameter limits (DOB) from Scaling Manual Table 3
  # Note: these values map to TopDBH (minimum top diameter outside bark)
  "ON"      , "POPU.SPP"  , NA_character_  ,       30 , 13.1    ,  9.0    , # Poplar group
  "ON"      , "BETU.PAP"  , NA_character_  ,       30 , 13.1    ,  9.0    , # White birch (explicit exception)
  "ON"      , "PINU.STR"  , NA_character_  ,       30 , 13.1    ,  9.0    , # White pine
  "ON"      , "PINU.RES"  , NA_character_  ,       30 , 13.1    ,  9.0    , # Red pine
  "ON"      , "TSUG.CAN"  , NA_character_  ,       30 , 13.1    ,  9.0    , # Hemlock (eastern hemlock)

  # Conifers (except white/red pine + hemlock): use genus-level conifer fallbacks
  "ON"      , "PICE.SPP"  , NA_character_  ,       30 ,  9.1    ,  9.0    ,
  "ON"      , "ABIE.SPP"  , NA_character_  ,       30 ,  9.1    ,  9.0    ,
  "ON"      , "LARI.SPP"  , NA_character_  ,       30 ,  9.1    ,  9.0    ,
  "ON"      , "THUJ.SPP"  , NA_character_  ,       30 ,  9.1    ,  9.0    ,
  "ON"      , "PINU.SPP"  , NA_character_  ,       30 ,  9.1    ,  9.0    ,

  # Hardwoods (except poplar/white birch): genus-level hardwood fallbacks
  "ON"      , "ACER.SPP"  , NA_character_  ,       30 , 17.1    ,  9.0    ,
  "ON"      , "FAGU.SPP"  , NA_character_  ,       30 , 17.1    ,  9.0    ,
  "ON"      , "QUER.SPP"  , NA_character_  ,       30 , 17.1    ,  9.0    ,
  "ON"      , "FRAX.SPP"  , NA_character_  ,       30 , 17.1    ,  9.0    ,
  "ON"      , "ULMU.SPP"  , NA_character_  ,       30 , 17.1    ,  9.0    ,
  "ON"      , "BETU.SPP"  , NA_character_  ,       30 , 17.1    ,  9.0    , # overrides for birches EXCEPT BETU.PAP (handled above)

  # --- BC: BEC-specific. BEC zones are grouped ---
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


# data - national agb parameters (Lambert/Ung) ####
parameters_LambertUng <- read.csv("data-raw/parameters_LambertUng.csv")


# data - model parameters for the Canadian national taper models (Ung et al 2013) ####
# original csv files with model parameters provided by Juha Metsaranta.

natdbh = read.csv("data-raw/NationalDBH.csv") # Canadian national taper models, DBH
parameters_NationalTaperModelsDBH <- natdbh %>%
  select(-me_eng) %>%
  mutate(Species = standardize_species_code(Species))
# usethis::use_data(
#   parameters_NationalTaperModelsDBH,
#   overwrite = T,
#   internal = T
# )

natdbhht = read.csv("data-raw/NationalDBHHT.csv") #Canadian national taper models, DBH + H
parameters_NationalTaperModelsDBHHT <- natdbhht %>%
  select(-name_eng) %>%
  mutate(Species = standardize_species_code(Species))
# usethis::use_data(
#   parameters_NationalTaperModelsDBHHT,
#   overwrite = T,
#   internal = T
# )

# data for the Honer model
# Entered manually from Honer et al 1983 paper.
parameters_Honer <- readxl::read_excel("data-raw/Honer1983_parameters.xlsx")
# usethis::use_data(parameters_Honer, overwrite = T, internal = T)

# data - model paramters for the regional models (several models) ####
# original csv files with model parameters provided by Juha Metsaranta.
# the original csv file includes parameters for multiple models. Data will be split by model for clarity.

regdbhht <- read.csv("data-raw/RegionalDBHHT.csv")
regdbhht <- regdbhht %>%
  mutate(
    Province = standardize_jurisdiction_code(Province),
    Species = standardize_species_code(Species)
  )

# regdbhht %>% group_by(ModelName) %>% count()
# parameters_Honer <- regdbhht %>% filter(ModelName == "Honer") # incomplete - replaced by the parameters entered manually
parameters_Kozak88 <- regdbhht %>% filter(ModelName == "Kozak88")
# parameters_Kozak94 <- regdbhht %>% filter(ModelName == "Kozak94")

# usethis::use_data(parameters_Kozak88, overwrite = T, internal = T)

# data - Kozak 1994 models for BC ####
parameters_Kozak94 <- readxl::read_excel("data-raw/kozak1994_parameters.xlsx")
parameters_Kozak94 <-
  parameters_Kozak94 %>%
  mutate(
    bec_zone = bec_zone %>%
      str_remove_all("\\s*\\([^)]*\\)")
  ) %>%
  select(-source_page, -species_name, -species_code_bc, -n_sample) %>%
  rename(Species = species, Subregion = bec_zone)
# usethis::use_data(parameters_Kozak94, overwrite = T, internal = T)

# data - Zakrzewski 2013 model for ON ####

# translating species to NFI codes
ON_species_dict <- read.csv("data-raw/ON_species_dict.csv")

CanadianTreeSpecies <- readr::read_csv(
  "data-raw/CanadianTreeSpeciesData.csv",
  na = c("", "NA"),
  show_col_types = FALSE
)
# 'https://raw.githubusercontent.com/ptompalski/CanadianTreeSpecies/refs/heads/main/data-raw/CanadianTreeSpeciesData.csv'
# )

species_dictionary <- CanadianTreeSpecies %>%
  transmute(
    NFI_code,
    CommonNameEnglish,
    CommonNameFrench,
    ScientificName,
    Genus,
    Species,
    Var
  ) %>%
  distinct() %>%
  group_by(NFI_code) %>%
  summarise(
    CommonNameEnglish = str_c(
      sort(unique(na.omit(CommonNameEnglish))),
      collapse = " / "
    ),
    CommonNameFrench = str_c(
      sort(unique(na.omit(CommonNameFrench))),
      collapse = " / "
    ),
    ScientificName = str_c(
      sort(unique(na.omit(ScientificName))),
      collapse = " / "
    ),
    Genus = str_c(sort(unique(na.omit(Genus))), collapse = " / "),
    Species = str_c(sort(unique(na.omit(Species))), collapse = " / "),
    Var = str_c(sort(unique(na.omit(Var))), collapse = " / "),
    .groups = "drop"
  ) %>%
  arrange(NFI_code)

jurisdiction_code_cols <- c(
  "ab_code",
  "bc_code",
  "nb_code",
  "nt_code",
  "on_code",
  "ns_code",
  "sk_code",
  "yt_code",
  "pe_code",
  "qc_code",
  "mb_code",
  "nl_code"
)

species_code_lookup <- CanadianTreeSpecies %>%
  transmute(
    NFI_code,
    canfi_code,
    ab_code,
    bc_code,
    nb_code,
    nt_code,
    on_code,
    ns_code,
    sk_code,
    yt_code,
    pe_code,
    qc_code,
    mb_code,
    nl_code
  ) %>%
  mutate(
    canfi_code = as.character(canfi_code)
  ) %>%
  select(
    NFI_code,
    canfi_code,
    all_of(jurisdiction_code_cols)
  ) %>%
  pivot_longer(
    cols = c(canfi_code, all_of(jurisdiction_code_cols)),
    names_to = "source_column",
    values_to = "code",
    values_drop_na = TRUE
  ) %>%
  mutate(
    code = as.character(code),
    code_system = case_when(
      source_column == "canfi_code" ~ "canfi",
      TRUE ~ "jurisdiction"
    ),
    jurisdiction = case_when(
      code_system == "jurisdiction" ~ str_remove(source_column, "_code$"),
      TRUE ~ NA_character_
    )
  ) %>%
  select(code_system, jurisdiction, code, NFI_code) %>%
  distinct() %>%
  arrange(code_system, jurisdiction, code, NFI_code)

CanadianTreeSpecies_ON <- species_code_lookup %>%
  filter(code_system == "jurisdiction", jurisdiction == "on") %>%
  transmute(on_code = code, NFI_code) %>%
  distinct()

# usethis::use_data(species_dictionary, overwrite = TRUE)
# usethis::use_data(species_code_lookup, overwrite = TRUE)
# CanadianTreeSpecies_ON

ON_vol_coef <- read_csv(
  "data-raw/ON_Vol_Coef.csv",
  show_col_types = FALSE,
  trim_ws = TRUE
)

parameters_Zakrzewski2013 <-
  ON_vol_coef %>%
  left_join(ON_species_dict, by = join_by(tree_spec == Spp_num)) %>%
  relocate(Spp_alpha) %>%
  mutate(Spp_alpha = toupper(Spp_alpha)) %>%
  left_join(CanadianTreeSpecies_ON, by = join_by(Spp_alpha == on_code)) %>%

  # two codes missing, entering them manually
  mutate(
    NFI_code = case_when(
      is.na(NFI_code) & Spp_alpha == "YB" ~ "BETU.ALL",
      is.na(NFI_code) & is.na(Spp_alpha) ~ "UNKN.SPP",
      TRUE ~ NFI_code
    )
  ) %>%

  relocate(NFI_code) %>%
  rename(Species = NFI_code) %>%
  select(-Spp_alpha, -tree_spec)

# usethis::use_data(parameters_Zakrzewski2013, overwrite = T, internal = T)

# ------------------------------------------------------------------------------
# NOTE: The three datasets imported below (Huang, GalBella, Klos) contain parameters for the same model - Kozak88 taper equation.

# data - Huang 1994 models for AB ####
# parameters_HuangV <- parameters_HuangV %>% rename(Species = species)
# parameters_HuangV <- parameters_HuangV %>% mutate(Subregion = NaturalSubregionCode)
# write.csv(parameters_HuangV, "data-raw/parameters_HuangV.csv", row.names=F)
parameters_HuangV <- read.csv("data-raw/parameters_HuangV.csv")

# select column, change to wide
parameters_Huang94 <-
  parameters_HuangV %>%
  select(Species, parameter, estimate, Subregion) %>%
  pivot_wider(names_from = parameter, values_from = estimate)

# usethis::use_data(parameters_Huang94, overwrite = T, internal = T)

# data - Gal & Bella 1994 parameters for SK ####
# equation 6
parameters_GalBella94 <- read.csv(
  "data-raw/GalBella1994_Table5_K2_params_with_NFI_species.csv"
)

parameters_GalBella94 <- parameters_GalBella94 %>%
  rename(Species = Species_NFI) %>%
  select(-Species_common)

# usethis::use_data(parameters_GalBella94, overwrite = T, internal = T)

# data - Klos et al 2007 (previously Klos 2004 Master Thesis) - parameters for MN ####
parameters_Klos2007 <- readxl::read_excel("data-raw/Klos2007_parameters.xlsx")
# usethis::use_data(parameters_Klos2007, overwrite = T, internal = T)

# parameters_Klos2004 <- read_csv("data-raw/klos_manitoba_parameters.csv")
# parameters_Klos2004 <-
#   parameters_Klos2004 %>%
#   filter(model_component == "taper_eq1") %>%
#   arrange(region_type, region, parameter) %>%
#   select(-jurisdiction, -species_original, -table_id, -model_component) %>%
#   rename(Species = species_nfi)

# param_wide <- parameters_Klos2004 %>%
#   filter(parameter %in% c("a0","a1","a2","b1","b2","b3","b4","b5")) %>%
#   filter(region_type !="site_type") %>%
#   pivot_wider(names_from = parameter, values_from = value)
# write.csv(param_wide, file="data-raw/params_Klos.csv", row.names=T)

# parameters_Klos2004 %>%
#   filter(parameter %in% c("a0","a1","a2","b1","b2","b3","b4","b5")) %>%
#   filter(region_type !="site_type") %>%
#  dplyr::summarise(n = dplyr::n(), .by = c(Species, region_type, region, parameter)) |>
#   dplyr::filter(n > 1L)

# data - Sharma 2021 model for central and eastern Canada ####

path_tbl2 <- "data-raw/Sharma2021_Table2.csv" # inside bark, total
path_tbl3 <- "data-raw/Sharma2021_Table3.csv" # outside bark, total
path_tbl4 <- "data-raw/Sharma2021_Table4.csv" # merchantable (inside bark)

# ---- read tables ----
tbl_inside_total <- read_csv(path_tbl2, show_col_types = FALSE) |>
  mutate(
    volume_type = "total_inside_bark"
  )

tbl_outside_total <- read_csv(path_tbl3, show_col_types = FALSE) |>
  mutate(
    volume_type = "total_outside_bark"
  )

tbl_merchantable <- read_csv(path_tbl4, show_col_types = FALSE) |>
  mutate(
    volume_type = "merchantable_inside_bark"
  )

parameters_Sharma2021 <- bind_rows(
  tbl_inside_total,
  tbl_outside_total,
  tbl_merchantable
) |>
  dplyr::select(
    Species = species,
    volume_type,
    alpha,
    beta,
    gamma
  ) |>
  arrange(volume_type, Species)

# Sharma 2021 includes to models for Cedar (genus): one species-specific for THUJ.OCC (eastern white-cedar),
# second for "Cedar species". He does not specify what that group consist of.
# Because there are only two cedar species occuring in eastern/central Canada, and one is already included as a
# separate entry, the "Cedar species" is converted to Eastern red cedar (JUNI.VIR)

parameters_Sharma2021 <- parameters_Sharma2021 |>
  mutate(
    Species = if_else(Species == "CEDA.SPP", "JUNI.VIR", Species)
  )
# usethis::use_data(parameters_Sharma2021, overwrite = T, internal = T)

# data - parameters for the QC merch volume model (Fortin et al 2007)

parameters_fortin2007 <- read_csv("data-raw/fortin2007_qc_merch_params.csv")

# select column, change to wide
parameters_fortin2007 <-
  parameters_fortin2007 %>%
  select(
    Species = nfi_species_code,
    b1 = beta1_ht_over_dbh,
    b2 = beta2_cyl,
    b3 = beta3_conif_cyl_dbh
  )
# usethis::use_data(parameters_fortin2007, overwrite = T, internal = T)

# data - parameters for the BC total and merch volume model (Nigh 2016) ####

parameters_Nigh2016 <- readxl::read_excel("data-raw/Nigh2016_parameters.xlsx")
parameters_Nigh2016 <-
  parameters_Nigh2016 %>%
  select(Species = species_nfi, Subregion = region, volume_type, b0, b1, b2) %>%
  mutate(
    subregion_type = if_else(
      Subregion %in% c("Coast", "Interior"),
      "region",
      "bec"
    )
  )

# usethis::use_data(parameters_Nigh2016, overwrite = T, internal = T)

# data - Boudewyn et al 2007 #####

CodesEcozones <- tribble(
  ~ecozone , ~ecozone_name_en     , ~ecozone_name_fr           ,
         1 , "Arctic Cordillera"  , "Cordillère arctique"      ,
         2 , "Northern Arctic"    , "Haut-Arctique"            ,
         3 , "Southern Arctic"    , "Bas-Arctique"             ,
         4 , "Taiga Plain"        , "Taïga des plaines"        ,
         5 , "Taiga Shield"       , "Taïga du Bouclier"        ,
         6 , "Boreal Shield"      , "Bouclier boréal"          ,
         7 , "Atlantic Maritime"  , "Maritime de l'Atlantique" ,
         8 , "MixedWood Plain"    , "Plaines à foréts mixtes"  ,
         9 , "Boreal PLain"       , "Plaines boréales"         ,
        10 , "Prairie"            , "Prairies"                 ,
        11 , "Taiga Cordillera"   , "Taïga de la Cordillère"   ,
        12 , "Boreal Cordillera"  , "Cordillère borèale"       ,
        13 , "Pacific Maritime"   , "Maritime du Pacifique"    ,
        14 , "Montane Cordillera" , "Cordillère montagnarde"   ,
        15 , "Hudson Plain"       , "Plaines hudsonniennes"
)

canfi_genus_codes <-
  tibble::tribble(
    ~canfi_genus , ~genus        ,
               1 , "PICE"        ,
               2 , "PINU"        ,
               3 , "ABIE"        ,
               5 , "PSEU"        ,
               6 , "LARI"        ,
               9 , "POPU"        ,
              10 , "BETU"        ,
              11 , "ACER"        ,
              12 , "GENH"        ,
             121 , "CARY"        ,
             122 , "JUGL"        ,
             123 , "ALNU"        ,
             124 , "OSTR"        ,
             125 , "CARP"        ,
             126 , "FAGU"        ,
             127 , "QUER"        ,
             128 , "ULMU"        ,
             129 , "MORU"        ,
             130 , "LIRI"        ,
             131 , "MAGN"        ,
             132 , "SASS"        ,
             133 , "PLAT"        ,
             134 , "PRUN"        ,
             135 , "GLED"        ,
             136 , "ROBI"        ,
             137 , "TILI"        ,
             138 , "NYSS"        ,
             139 , "CORN"        ,
             140 , "ARBU"        ,
             141 , "FRAX"        ,
             142 , "SALI"        ,
             143 , "GYMN"        ,
             144 , "CELT"        ,
             145 , "AMEL"        ,
             146 , "CORY"        ,
             147 , "CRAT"        ,
             148 , "ILEX"        ,
             149 , "MALU"        ,
             150 , "NEMO"        ,
             151 , "RHUS"        ,
             152 , "SORB"        ,
             153 , "VIBU"        ,
             154 , "CAST"        ,
             155 , "ASIM"        ,
               8 , "GENC"        ,
               4 , "TSUG"        ,
               7 , "THUJ"        ,
              71 , "JUNI"        ,
              72 , "TAXU"        ,
              73 , "CHAM"        ,
              81 , NA_character_
  )


add_ecozone_names <- function(df, ecozone_tbl = CodesEcozones) {
  df %>%
    left_join(ecozone_tbl, by = "ecozone") %>%
    relocate(ecozone_name_en, ecozone_name_fr, .after = ecozone)
}

# CANFI genus -> NFI genus code mapping (your object)
# canfi_genus_codes <- tibble::tribble(...)

## --- helpers ---------------------------------------------------------

make_species_nfi <- function(
  genus,
  species = NA_character_,
  variety = NA_character_
) {
  genus <- as.character(genus)
  species <- as.character(species)
  variety <- as.character(variety)

  out <- ifelse(
    !is.na(species) & nzchar(species),
    paste(genus, species, sep = "."),
    paste(genus, "SPP", sep = ".")
  )

  ifelse(!is.na(variety) & nzchar(variety), paste(out, variety, sep = "."), out)
}

prep_species_level <- function(
  df,
  component,
  source_table,
  ecozones = CodesEcozones
) {
  df %>%
    dplyr::rename(
      canfi_species = dplyr::any_of("canfi_spec")
    ) %>%
    dplyr::mutate(
      component = component,
      source_table = source_table
      # species_nfi = make_species_nfi(genus, species, variety)
    ) %>%
    add_ecozone_names(ecozones) %>%
    dplyr::relocate(
      component,
      source_table,
      juris_id,
      ecozone,
      ecozone_name_en,
      ecozone_name_fr,
      # species_nfi,
      dplyr::any_of("canfi_species"),
      genus,
      species,
      variety
    )
}


# For tables that are genus-level (no species/variety columns)
prep_genus_level <- function(
  df,
  component,
  source_table,
  ecozones = CodesEcozones
) {
  df %>%
    mutate(
      component = component,
      source_table = source_table,
      species = NA_character_,
      variety = NA_character_,
      canfi_species = NA_character_
      # species_nfi = paste0(genus, ".SPP")
    ) %>%
    add_ecozone_names(ecozones) %>%
    relocate(
      component,
      source_table,
      juris_id,
      ecozone,
      ecozone_name_en,
      ecozone_name_fr,
      # species_nfi,
      canfi_genus,
      genus,
      species,
      variety
    )
}

# Table 14: has canfi_genus numeric only; join to get genus code
prep_table14 <- function(
  df,
  component,
  source_table,
  ecozones = CodesEcozones,
  canfi_genus_codes
) {
  df %>%
    left_join(canfi_genus_codes, by = "canfi_genus") %>%
    mutate(
      component = component,
      source_table = source_table,
      species = NA_character_,
      variety = NA_character_,
      canfi_species = NA_character_
      # species_nfi = paste0(genus, ".SPP")
    ) %>%
    add_ecozone_names(ecozones) %>%
    relocate(
      component,
      source_table,
      juris_id,
      ecozone,
      ecozone_name_en,
      ecozone_name_fr,
      # species_nfi,
      canfi_genus,
      genus,
      species,
      variety
    )
}

prep_caps_species_level <- function(
  df,
  component,
  source_table,
  ecozones = CodesEcozones
) {
  df %>%
    # standardize CANFI column name
    dplyr::rename(canfi_species = dplyr::any_of("canfi_spec")) %>%
    # standardize min/max column names to a common pair
    dplyr::rename(
      x_min = dplyr::any_of(c("vol_min", "biom_min", "tb_min")),
      x_max = dplyr::any_of(c("vol_max", "biom_max", "tb_max"))
    ) %>%
    dplyr::mutate(
      component = component,
      source_table = source_table
      # species_nfi = make_species_nfi(genus, species, variety)
    ) %>%
    add_ecozone_names(ecozones) %>%
    dplyr::relocate(
      component,
      source_table,
      juris_id,
      ecozone,
      ecozone_name_en,
      ecozone_name_fr,
      # species_nfi,
      dplyr::any_of("canfi_species"),
      genus,
      species,
      variety,
      x_min,
      x_max,
      p_sw_low,
      p_sb_low,
      p_br_low,
      p_fl_low,
      p_sw_high,
      p_sb_high,
      p_br_high,
      p_fl_high
    )
}


## --- import ----------------------------------------------------------

# Adjust these filenames to match your local copies
files <- list(
  B3 = "appendix2_table3.csv",
  B3a = "appendix2_table3a.csv",
  B4 = "appendix2_table4.csv",
  B5 = "appendix2_table5.csv",
  B6_vol = "appendix2_table6.csv",
  B6_tb = "appendix2_table6_tb.csv",
  B7_vol = "appendix2_table7.csv",
  B7_tb = "appendix2_table7_tb.csv",
  B14 = "appendix6_table14.csv"
)

parameters_v2b <- list(
  B3 = read_csv(file.path("data-raw", files$B3), show_col_types = FALSE) %>%
    prep_species_level("B3", "Appendix 2 - Table 3"),

  # B3a = read_csv(file.path("data-raw", files$B3a), show_col_types = FALSE) %>%
  #   prep_species_level("B3a", "Appendix 2 - Table 3a (dead tree biomass)"),

  B4 = read_csv(file.path("data-raw", files$B4), show_col_types = FALSE) %>%
    prep_species_level("B4", "Appendix 2 - Table 4"),

  B5 = read_csv(file.path("data-raw", files$B5), show_col_types = FALSE) %>%
    # Table 5 is genus-level and already includes genus as a code in your version
    prep_genus_level("B5", "Appendix 2 - Table 5"),

  B6_vol = read_csv(
    file.path("data-raw", files$B6_vol),
    show_col_types = FALSE
  ) %>%
    prep_species_level(
      "B6_vol",
      "Appendix 2 - Table 6 (volm-based proportions)"
    ),

  B6_tb = read_csv(
    file.path("data-raw", files$B6_tb),
    show_col_types = FALSE
  ) %>%
    prep_species_level(
      "B6_tb",
      "Appendix 2 - Table 6 tb (tb-based proportions)"
    ),

  B7_vol = read_csv(
    file.path("data-raw", files$B7_vol),
    show_col_types = FALSE
  ) %>%
    prep_caps_species_level(
      "B7_vol",
      "Appendix 2 - Table 7 (caps for Table 6 volm)"
    ),

  B7_tb = read_csv(
    file.path("data-raw", files$B7_tb),
    show_col_types = FALSE
  ) %>%
    prep_caps_species_level(
      "B7_tb",
      "Appendix 2 - Table 7 tb (caps for Table 6 tb)"
    ),

  B14 = read_csv(file.path("data-raw", files$B14), show_col_types = FALSE) %>%
    prep_table14(
      "B14",
      "Appendix 6 - Table 14",
      canfi_genus_codes = canfi_genus_codes
    )
)

## --- save ------------------------------------------------------------
# usethis::use_data(parameters_v2b, overwrite = T, internal = TRUE)

# ecozone - internal dataset

ecozones <- tribble(
  ~ecozone , ~ecozone_name_en     , ~ecozone_name_fr           ,
         1 , "Arctic Cordillera"  , "Cordillère arctique"      ,
         2 , "Northern Arctic"    , "Haut-Arctique"            ,
         3 , "Southern Arctic"    , "Bas-Arctique"             ,
         4 , "Taiga Plain"        , "Taïga des plaines"        ,
         5 , "Taiga Shield"       , "Taïga du Bouclier"        ,
         6 , "Boreal Shield"      , "Bouclier boréal"          ,
         7 , "Atlantic Maritime"  , "Maritime de l'Atlantique" ,
         8 , "MixedWood Plain"    , "Plaines à foréts mixtes"  ,
         9 , "Boreal Plain"       , "Plaines boréales"         ,
        10 , "Prairie"            , "Prairies"                 ,
        11 , "Taiga Cordillera"   , "Taïga de la Cordillère"   ,
        12 , "Boreal Cordillera"  , "Cordillère borèale"       ,
        13 , "Pacific Maritime"   , "Maritime du Pacifique"    ,
        14 , "Montane Cordillera" , "Cordillère montagnarde"   ,
        15 , "Hudson Plain"       , "Plaines hudsonniennes"
)

# usethis::use_data(ecozones, overwrite = T, internal = TRUE)

# Newfoundland - parameters from C. Hennigar implemantation in OSM ####
# https://github.com/OSM-Contributors/OSM/blob/main/OSM.NewfoundlandModels/Volume/HonerVolume_N_X_242_122.cs

# PlantCodes (OSM/USDA) -> NFI species codes
plant_to_nfi <- tibble::tribble(
  ~plant_code , ~Species   ,
  "ABBA"      , "ABIE.BAL" , # balsam fir
  "PIMA"      , "PICE.MAR" , # black spruce
  "PIGL"      , "PICE.GLA" , # white spruce
  "PIST"      , "PINU.STR" , # eastern white pine
  "PIRE"      , "PINU.RES" , # red pine
  "PIBA2"     , "PINU.BAN" , # jack pine
  "LALA"      , "LARI.LAR" , # tamarack
  "POTR5"     , "POPU.TRE" , # trembling aspen
  "BEPA"      , "BETU.PAP" , # paper birch
  "BEAL2"     , "BETU.ALL" , # yellow birch
  "ACRU"      , "ACER.RUB" , # red maple
  "BEPO"      , "BETU.POP" # gray birch
)

## read csvs ----------------------------------------------------------------

bf <- read_csv(
  file.path("data-raw/nx242_bf_district_original.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    param_set = "NX242_BF_DISTRICT",
    plant_code = "ABBA",
    Species = "ABIE.BAL"
  )

bs <- read_csv(
  file.path("data-raw/nx242_bs_district_original.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    param_set = "NX242_BS_DISTRICT",
    plant_code = "PIMA",
    Species = "PICE.MAR"
  )

sp <- read_csv(
  file.path("data-raw/nx122_nx67_species_original.csv"),
  show_col_types = FALSE
) %>%
  mutate(param_set = "NX122_NX67_SPECIES") %>%
  left_join(plant_to_nfi, by = "plant_code")

## ---- combine to one tidy table ------------------------------------------------

parameters_volNL <-
  bind_rows(
    bf %>% mutate(district = as.integer(district)),
    bs %>% mutate(district = as.integer(district)),
    sp %>% mutate(district = NA_integer_)
  ) %>%
  # keep a consistent column order
  select(
    param_set,
    district,
    plant_code,
    Species,
    t,
    a,
    b,
    c,
    d,
    e,
    nv_a,
    nv_b,
    nv_c
  ) %>%
  arrange(param_set, district, plant_code) %>%
  # rename district to "Subregion"
  mutate(Subregion = as.character(district)) %>%

  # province-wide parameters get Subregion="ALL"
  mutate(Subregion = if_else(is.na(Subregion), "ALL", Subregion))


# usethis::use_data(parameters_volNL, overwrite = T, internal = TRUE)

# site index datasets ####

## Payandeh 1974 ####
parameters_Payandeh1974 <- read.csv("data-raw/Payandeh_1974_parameters.csv")

## Lundgren & Dolid 1970 ####
# Lundgren, Allen L.; Dolid, William A. 1970. Biological growth functions describe published site index curves for Lake States timber species. Research Paper NC-36. St. Paul, MN: U.S. Dept. of Agriculture, Forest Service, North Central Forest Experiment Station

parameters_LungrenDolid1970 <- read.csv(
  "data-raw/DolidLundgren1970_parameters.csv"
)
parameters_LungrenDolid1970 <- parameters_LungrenDolid1970 %>%
  select(Species = nfi_species, model:b3)

## Ker & Bowling 1991 ####
parameters_KerBowling1991 <- read.csv(
  "data-raw/KerBowling1991_parameters.csv"
)
parameters_KerBowling1991 <-
  parameters_KerBowling1991 %>%
  select(Species = nfi_species, b0:b4)

## Cieszewski & Bella 1991 ####
parameters_CieszewskiBella1991 <- read.csv(
  "data-raw/CieszewskiBella1991_parameters.csv"
)
parameters_CieszewskiBella1991 <-
  parameters_CieszewskiBella1991 %>%
  select(
    Species = nfi_species,
    a,
    b,
    base_age_bh
  )

## Scott & Voorhis ####
parameters_ScottVoorhis1986 <- read.csv(
  "data-raw/ScottVoorhis1986_parameters.csv"
)
parameters_ScottVoorhis1986 <-
  parameters_ScottVoorhis1986 %>%
  select(Species = nfi_species, b1:b5)

##  Nigh 2000 growth intercept (site index model is for a single species and can be hardcoded) ####
parameters_Nigh2000_gi <- read.csv("data-raw/nigh_2000_gi.csv")

## Thrower 1994 #####
parameters_Thrower1994 <- read.csv("data-raw/Thrower1994_parameters.csv")
parameters_Thrower1994 <- parameters_Thrower1994 %>%
  select(Species = nfi_species, model_form:source_short)

## Chen & Klinka 2000 (ESSF height growth / site index) ####
parameters_ChenKlinka1998 <- read.csv("data-raw/ChenKlinka1998_parameters.csv")
parameters_ChenKlinka1998 <- parameters_ChenKlinka1998 %>%
  select(Species = nfi_species, b1, b2, b3)

## Hu & Garcia 2009 (interior spruce site index) ####
parameters_HuGarcia2009 <- read.csv("data-raw/HuGarcia2009_parameters.csv")
parameters_HuGarcia2009 <- parameters_HuGarcia2009 %>%
  select(
    Species = nfi_species,
    a_coef,
    a_exp,
    c,
    h0,
    t0,
    base_age,
    source_short
  )

## Alemdag 1991 (national white spruce site index / height growth) ####
parameters_Alemdag1991 <- read.csv("data-raw/Alemdag1991_parameters.csv")
parameters_Alemdag1991 <- parameters_Alemdag1991 %>%
  select(
    Species = nfi_species,
    c1,
    c2,
    c4,
    c5,
    b1,
    b2,
    b4,
    b5,
    base_age,
    source_short
  )

## Nigh et al. 2002 (trembling aspen height-age / site index, BC) ####
parameters_Nigh2002 <- read.csv("data-raw/Nigh2002_parameters.csv")
parameters_Nigh2002 <- parameters_Nigh2002 %>%
  transmute(
    Species = nfi_species,
    model,
    bec_zone = dplyr::na_if(as.character(bec_zone), ""),
    a0 = as.numeric(a0),
    a1 = as.numeric(a1),
    a2 = as.numeric(a2),
    source_short
  )

## Nigh 1997 (Sitka spruce height-age / site index, coastal BC) ####
parameters_Nigh1997 <- read.csv("data-raw/Nigh1997_parameters.csv")
parameters_Nigh1997 <- parameters_Nigh1997 %>%
  transmute(
    Species = nfi_species,
    model,
    a0 = as.numeric(a0),
    a1 = as.numeric(a1),
    a2 = as.numeric(a2),
    source_short
  )

## Nigh 1998 growth intercept (interior western hemlock, single species) ####
# eq. 4: SI = 1.3 + exp(b1_log) * GI^b2, with per-BHA coefficients (Table 3).
# b1 was fitted on the log scale (Ratkowsky transformation) and stored as b1_log.
parameters_Nigh1998_gi <- read.csv(
  "data-raw/Nigh1998_growthintercept_parameters.csv"
)
parameters_Nigh1998_gi <- parameters_Nigh1998_gi %>%
  transmute(
    bha = as.integer(bha),
    b1_log = as.numeric(b1_log),
    b2 = as.numeric(b2)
  )

## Carmean et al. 2006 (black spruce & trembling aspen, NW Ontario) ####
# Newnham (1988) constrained polymorphic SI model. Aspen coefficients confirmed
# against an independent SAS implementation; black spruce b2 is a best reading
# of a degraded exponent in the published PDF (no independent implementation).
parameters_Carmean2006 <- read.csv("data-raw/Carmean2006_parameters.csv")
parameters_Carmean2006 <- parameters_Carmean2006 %>%
  transmute(
    Species = nfi_species,
    b1 = as.numeric(b1),
    b2 = as.numeric(b2),
    b3 = as.numeric(b3),
    b4 = as.numeric(b4),
    base_age = as.numeric(base_age),
    source_short
  )

# Carmean, Niznowski & Hazenberg (2001) jack pine SI model, northern Ontario.
# Newnham (1988) constrained polymorphic form; the recommended all-region
# equation (<=100 yr BH age, 383 plots) matches the NRCan SAS macro
# %SI/HT_Carmean_2001 digit-for-digit.
parameters_Carmean2001 <- read.csv("data-raw/Carmean2001_parameters.csv")
parameters_Carmean2001 <- parameters_Carmean2001 %>%
  transmute(
    Species = nfi_species,
    b1 = as.numeric(b1),
    b2 = as.numeric(b2),
    b3 = as.numeric(b3),
    b4 = as.numeric(b4),
    base_age = as.numeric(base_age),
    source_short
  )

## Goelz & Burk 1992 (base-age invariant jack pine SI, north central Ontario) ####
# Chapman-Richards difference equation (eq. 16); parameters b1-b4 from Table 2.
parameters_Goelz1992 <- read.csv("data-raw/Goelz1992_parameters.csv")
parameters_Goelz1992 <- parameters_Goelz1992 %>%
  transmute(
    Species = nfi_species,
    b1 = as.numeric(b1),
    b2 = as.numeric(b2),
    b3 = as.numeric(b3),
    b4 = as.numeric(b4),
    base_age = as.numeric(base_age),
    source_short
  )

## Nigh et al. 2009 (paper birch height-age / site index, BC) ####
# Model 1 (base) reproduces the SAS reference (SI_Nigh_2009 / HT_Nigh_2009);
# Models 2 (operational) and 3 (zonal) are additional published variants.
parameters_Nigh2009 <- read.csv("data-raw/Nigh2009_parameters.csv")
parameters_Nigh2009 <- parameters_Nigh2009 %>%
  transmute(
    Species = nfi_species,
    model,
    bec_zone = dplyr::na_if(as.character(bec_zone), ""),
    a0 = as.numeric(a0),
    a1 = as.numeric(a1),
    a2 = as.numeric(a2),
    source_short
  )

## Nigh 2004 (juvenile height-age / site index, BC lodgepole pine + interior spruce) ####
# Two species (lodgepole pine PINU.CON, interior spruce modelled as white spruce
# PICE.GLA). Province-wide row (bec_zone = "PROV", model 3, Table 2) plus seven
# BC biogeoclimatic zones whose a1-a4 are resolved from eqs. 5/6 (Table 3).
parameters_Nigh2004 <- read.csv("data-raw/Nigh2004_parameters.csv")
parameters_Nigh2004 <- parameters_Nigh2004 %>%
  transmute(
    Species = nfi_species,
    bec_zone = as.character(bec_zone),
    a1 = as.numeric(a1),
    a2 = as.numeric(a2),
    a3 = as.numeric(a3),
    a4 = as.numeric(a4),
    source_short
  )

## Nigh 2017 (lodgepole pine g-GADA height-age / site index, BC) ####
# Single species (lodgepole pine, PINU.CON). One global parameter set (Table 2,
# g-GADA block) plus the SI->beta0 cubic (Discussion, p. 18) for base age 50.
parameters_Nigh2017 <- read.csv("data-raw/Nigh2017_parameters.csv")
parameters_Nigh2017 <- parameters_Nigh2017 %>%
  transmute(
    Species = nfi_species,
    b10 = as.numeric(b10),
    b11 = as.numeric(b11),
    b20 = as.numeric(b20),
    b21 = as.numeric(b21),
    si_b0_c0 = as.numeric(si_b0_c0),
    si_b0_c1 = as.numeric(si_b0_c1),
    si_b0_c2 = as.numeric(si_b0_c2),
    si_b0_c3 = as.numeric(si_b0_c3),
    source_short
  )

## Brisco, Klinka & Nigh 2002 (western larch height-age / site index, BC) ####
# Recommended final model (eq. 6): the unconstrained Chapman-Richards form (eq. 3)
# refit to the complete data set.
parameters_Brisco2002 <- read.csv("data-raw/Brisco2002_parameters.csv")
parameters_Brisco2002 <- parameters_Brisco2002 %>%
  transmute(
    Species = nfi_species,
    source_species,
    b1 = as.numeric(b1),
    b2 = as.numeric(b2),
    b3 = as.numeric(b3),
    b4 = as.numeric(b4),
    b5 = as.numeric(b5),
    source_short
  )

## Goudie 1984 (lodgepole pine / white spruce height-age, BC) ####
# Coefficients follow the SAS reference implementation
# (SK_SiteIndex_SAS_macros_with_BHAge_20220409.sas); pine uses the dry-site set.
parameters_Goudie1984 <- read.csv("data-raw/Goudie1984_parameters.csv")
parameters_Goudie1984 <- parameters_Goudie1984 %>%
  transmute(
    Species,
    index_age = as.numeric(index_age),
    b1 = as.numeric(b1),
    b2 = as.numeric(b2),
    b3 = as.numeric(b3)
  )

## Cieszewski, Bella & Yeung 1993 (Saskatchewan variable-age site index) ####
parameters_Cieszewski1993 <- read.csv("data-raw/Cieszewski1993_parameters.csv")
parameters_Cieszewski1993 <- parameters_Cieszewski1993 %>%
  transmute(
    Species = nfi_species,
    a = as.numeric(a),
    b = as.numeric(b),
    base_age = 50,
    source_short
  )

## Huang et al 1994 (site index) ####
parameters_Huang1994_si <- read.csv("data-raw/Huang1994_parameters.csv")

## Carmean et al. 1989 (site index) ####
parameters_Carmean1989 <- readr::read_csv(
  "data-raw/Carmean1989_parameters.csv",
  show_col_types = FALSE
) %>%
  mutate(
    figure_no = as.integer(figure_no),
    years_to_bh = suppressWarnings(as.numeric(years_to_bh))
  ) %>%
  filter(figure_no %in% c(3, 6, 11, 13, 14, 34, 48, 51, 53, 57, 127)) %>%
  mutate(
    Species = case_when(
      figure_no == 3 ~ "ACER.SAH",
      figure_no == 6 ~ "BETU.ALL",
      figure_no == 11 ~ "FAGU.GRA",
      figure_no == 13 ~ "FRAX.AME",
      figure_no == 14 ~ "FRAX.NIG",
      figure_no == 34 ~ "PRUN.SER",
      figure_no == 48 ~ "QUER.RUB",
      figure_no == 51 ~ "TILI.AME",
      figure_no == 53 ~ "ULMU.AME",
      figure_no == 57 ~ "CHAM.THY",
      figure_no == 127 ~ "TSUG.CAN",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    Species,
    figure_no,
    height_b1:si_b5,
    years_to_bh,
    source
  )

## Carmean 1996 (site index) ####
parameters_Carmean1996 <- readr::read_csv(
  "data-raw/Carmean1996_parameters.csv",
  show_col_types = FALSE
) %>%
  transmute(
    Species = nfi_species,
    species_code,
    species_common,
    figure_no = suppressWarnings(as.integer(figure_no)),
    model_family,
    si_base_age_bh = as.numeric(si_base_age_bh),
    age_basis,
    predict_si_method,
    height_offset_m = as.numeric(height_offset_m),
    source_length_factor = as.numeric(source_length_factor),
    source_height_offset = as.numeric(source_height_offset),
    source_si_offset = as.numeric(source_si_offset),
    k_index_age = as.numeric(k_index_age),
    h_a = as.numeric(h_a),
    h_b = as.numeric(h_b),
    h_c = as.numeric(h_c),
    h_d = as.numeric(h_d),
    h_e = as.numeric(h_e),
    h_f = as.numeric(h_f),
    si_a = as.numeric(si_a),
    si_b = as.numeric(si_b),
    si_c = as.numeric(si_c),
    si_d = as.numeric(si_d),
    si_e = as.numeric(si_e),
    si_f = as.numeric(si_f),
    source_short,
    notes
  )

## Quebec ecological-site IQS parameters (Lafleche et al. 2013) ####
parameters_QC_IQS2013 <- readr::read_csv(
  "data-raw/qc_iqs_parameters_2013.csv",
  show_col_types = FALSE
) %>%
  transmute(
    curve_set,
    species_qc,
    Species = nfi_species,
    region_ecologique,
    subregion_ecologique = dplyr::na_if(subregion_ecologique, ""),
    type_ecologique,
    n_trees = as.integer(n_trees),
    n_observations = as.integer(n_observations),
    b1 = as.numeric(b1),
    b2 = as.numeric(b2),
    b3 = as.numeric(b3),
    pseudo_r2 = as.numeric(pseudo_r2),
    equation_used,
    raw_pdf_row
  )

qc_iqs_ecological_keys_2013 <- readr::read_csv(
  "data-raw/qc_iqs_ecological_keys_2013.csv",
  show_col_types = FALSE
) %>%
  transmute(
    region_ecologique,
    region_description,
    subregion_ecologique = dplyr::na_if(subregion_ecologique, ""),
    type_ecologique
  )

qc_type_ecologique_definitions_2013 <- readr::read_csv(
  "data-raw/qc_type_ecologique_definitions_2013.csv",
  show_col_types = FALSE
) %>%
  transmute(
    type_ecologique,
    type_ecologique_description_fr,
    type_ecologique_description_en
  )

qc_iqs_ecological_keys_2013 <- qc_iqs_ecological_keys_2013 %>%
  left_join(
    qc_type_ecologique_definitions_2013,
    by = "type_ecologique"
  )

# Convert Huang SI natural-region groups from numeric IDs to Alberta letter codes
# using the crosswalk embedded in parameters_HuangV (NaturalSubregionNum -> Code).
huang_subregion_xwalk <- parameters_HuangV %>%
  select(
    NaturalSubregionNum = NaturalSubregionNum,
    NaturalSubregionCode = NaturalSubregionCode
  ) %>%
  distinct() %>%
  mutate(
    NaturalSubregionNum = as.character(NaturalSubregionNum),
    NaturalSubregionCode = as.character(NaturalSubregionCode)
  )

translate_huang_regions_to_codes <- function(x, xwalk) {
  x_chr <- as.character(x)
  x_trim <- stringr::str_squish(x_chr)

  vapply(
    x_trim,
    FUN.VALUE = character(1),
    FUN = function(one_region) {
      if (toupper(one_region) == "ALL") {
        return("All")
      }

      parts <- strsplit(one_region, "\\s*,\\s*")[[1]]
      codes <- xwalk$NaturalSubregionCode[match(
        parts,
        xwalk$NaturalSubregionNum
      )]
      if (any(is.na(codes))) {
        stop(
          "Unmapped Huang natural region ID(s): ",
          paste(parts[is.na(codes)], collapse = ", ")
        )
      }
      paste(codes, collapse = ", ")
    }
  )
}

parameters_Huang1994_si <- parameters_Huang1994_si %>%
  select(Species = nfi_species, natural_regions:b5) %>%
  mutate(
    natural_regions = translate_huang_regions_to_codes(
      natural_regions,
      xwalk = huang_subregion_xwalk
    )
  )

## Carmean & Hahn 1981 (balsam fir & white spruce, Lake States) ####
parameters_CarmeanHahn1981 <- read.csv(
  "data-raw/CarmeanHahn1981_parameters.csv"
)
parameters_CarmeanHahn1981 <- parameters_CarmeanHahn1981 %>%
  transmute(
    Species = nfi_species,
    height_b1 = as.numeric(height_b1),
    height_b2 = as.numeric(height_b2),
    height_b3 = as.numeric(height_b3),
    height_b4 = as.numeric(height_b4),
    height_b5 = as.numeric(height_b5),
    si_b1 = as.numeric(si_b1),
    si_b2 = as.numeric(si_b2),
    si_b3 = as.numeric(si_b3),
    si_b4 = as.numeric(si_b4),
    si_b5 = as.numeric(si_b5)
  )

## Auger & Ward 2021 (QC black spruce & jack pine plantations) ####
parameters_AugerWard2021 <- read.csv("data-raw/AugerWard2021_parameters.csv")
parameters_AugerWard2021 <- parameters_AugerWard2021 %>%
  transmute(
    Species = nfi_species,
    beta0 = as.numeric(beta0),
    beta2 = as.numeric(beta2),
    form = as.character(form)
  )

## Sharma 2022 (ON black spruce & trembling aspen, McDill-Amateis) ####
parameters_Sharma2022 <- read.csv("data-raw/Sharma2022_parameters.csv")
parameters_Sharma2022 <- parameters_Sharma2022 %>%
  transmute(
    Species = nfi_species,
    a0 = as.numeric(a0),
    a1 = as.numeric(a1)
  )

## Nigh 1998 (interior western hemlock height-bha, single global coef set) ####
# eq. 6 log-logistic height-age model; one coefficient row (Table 2).
parameters_Nigh1998_ht <- read.csv(
  "data-raw/Nigh1998_heightbha_parameters.csv"
)
parameters_Nigh1998_ht <- parameters_Nigh1998_ht %>%
  transmute(
    b0 = as.numeric(b0),
    b1 = as.numeric(b1),
    b2 = as.numeric(b2)
  )

## Buckman et al 2006 (red pine, single global constant set) ####
# Constrained Buckman refit (A,B,C) plus younger-age polynomial (k,m).
parameters_Buckman2006 <- read.csv("data-raw/Buckman2006_parameters.csv")
parameters_Buckman2006 <- parameters_Buckman2006 %>%
  transmute(
    k = as.numeric(k),
    m = as.numeric(m),
    A = as.numeric(A),
    B = as.numeric(B),
    C = as.numeric(C)
  )

## Batho & Garcia 2014 (lodgepole pine, single global constant set) ####
# Bertalanffy-Richards "Power combined" fit constants (Eqs. 3-6).
parameters_Batho2014 <- read.csv("data-raw/Batho2014_parameters.csv")
parameters_Batho2014 <- parameters_Batho2014 %>%
  transmute(
    a_coef = as.numeric(a_coef),
    a_exp = as.numeric(a_exp),
    c = as.numeric(c),
    h0 = as.numeric(h0),
    t0 = as.numeric(t0),
    base_age = as.numeric(base_age)
  )

# combine all into one ####

internal_objs <- c(
  "parameters_LambertUng",
  "parameters_NationalTaperModelsDBH",
  "parameters_NationalTaperModelsDBHHT",
  "parameters_Honer",
  "parameters_Kozak88",
  "parameters_Kozak94",
  "parameters_Zakrzewski2013",
  "parameters_Huang94",
  "parameters_GalBella94",
  "parameters_Klos2007",
  "parameters_Sharma2021",
  "parameters_fortin2007",
  "parameters_Nigh2016",
  "parameters_v2b",
  "ecozones",
  "parameters_volNL",
  "parameters_Payandeh1974",
  "parameters_LungrenDolid1970",
  "parameters_KerBowling1991",
  "parameters_CieszewskiBella1991",
  "parameters_ScottVoorhis1986",
  "parameters_Nigh2000_gi",
  "parameters_Nigh1998_gi",
  "parameters_Thrower1994",
  "parameters_ChenKlinka1998",
  "parameters_HuGarcia2009",
  "parameters_Alemdag1991",
  "parameters_Nigh1997",
  "parameters_Nigh2002",
  "parameters_Nigh2004",
  "parameters_Nigh2009",
  "parameters_Nigh2017",
  "parameters_Brisco2002",
  "parameters_Goudie1984",
  "parameters_Cieszewski1993",
  "parameters_Huang1994_si",
  "parameters_Carmean1989",
  "parameters_Carmean1996",
  "parameters_Carmean2006",
  "parameters_Carmean2001",
  "parameters_Goelz1992",
  "parameters_QC_IQS2013",
  "qc_iqs_ecological_keys_2013",
  "parameters_CarmeanHahn1981",
  "parameters_AugerWard2021",
  "parameters_Sharma2022",
  "parameters_Nigh1998_ht",
  "parameters_Buckman2006",
  "parameters_Batho2014"
)

# sanity check: make sure they exist before saving
missing <- internal_objs[
  !vapply(internal_objs, exists, logical(1), envir = environment())
]
if (length(missing)) {
  stop("Missing internal objects: ", paste(missing, collapse = ", "))
}

save(list = internal_objs, file = "R/sysdata.rda", compress = "bzip2")
