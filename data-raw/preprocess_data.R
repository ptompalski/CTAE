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
usethis::use_data(parameters_Kozak94, overwrite = T)


# data - Zakrzewski 2013 model for ON ####

# translating species to NFI codes
ON_species_dict <- read.csv("data-raw/ON_species_dict.csv")

CanadianTreeSpecies <- read.csv(
  'https://raw.githubusercontent.com/ptompalski/CanadianTreeSpecies/refs/heads/main/data-raw/CanadianTreeSpeciesData.csv'
)
CanadianTreeSpecies_ON <- CanadianTreeSpecies %>%
  select(on_code, NFI_code) %>%
  filter(!is.na(on_code)) %>%
  distinct()
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

usethis::use_data(parameters_Zakrzewski2013, overwrite = T)

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

usethis::use_data(parameters_Huang94, overwrite = T)


# data - Gal & Bella 1994 parameters for SK ####
# equation 6
parameters_GalBella94 <- read.csv(
  "data-raw/GalBella1994_Table5_K2_params_with_NFI_species.csv"
)

parameters_GalBella94 <- parameters_GalBella94 %>%
  rename(Species = Species_NFI) %>%
  select(-Species_common)

usethis::use_data(parameters_GalBella94, overwrite = T)

# data - Klos et al 2007 (previously Klos 2004 Master Thesis) - parameters for MN ####
parameters_Klos2007 <- readxl::read_excel("data-raw/Klos2007_parameters.xlsx")
usethis::use_data(parameters_Klos2007, overwrite = T)

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
  select(
    Species = species,
    volume_type,
    alpha,
    beta,
    gamma
  ) |>
  arrange(volume_type, species)

# Sharma 2021 includes to models for Cedar (genus): one species-specific for THUJ.OCC (eastern white-cedar),
# second for "Cedar species". He does not specify what that group consist of.
# Because there are only two cedar species occuring in eastern/central Canada, and one is already included as a
# separate entry, the "Cedar species" is converted to Eastern red cedar (JUNI.VIR)

parameters_Sharma2021 <- parameters_Sharma2021 |>
  mutate(
    Species = if_else(Species == "CEDA.SPP", "JUNI.VIR", Species)
  )
usethis::use_data(parameters_Sharma2021, overwrite = T)

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
usethis::use_data(parameters_fortin2007, overwrite = T)


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

usethis::use_data(parameters_Nigh2016, overwrite = T)


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

# --- helpers ---------------------------------------------------------

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


# --- import ----------------------------------------------------------

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

# --- save ------------------------------------------------------------
usethis::use_data(parameters_v2b, overwrite = TRUE)


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

usethis::use_data(ecozones, internal = TRUE, overwrite = TRUE)
