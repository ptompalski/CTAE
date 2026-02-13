#' Volume model registry
#'
#' Returns a registry (metadata table) describing tree volume models available in CanadaForestAllometry.
#' The registry is used by wrapper functions to (1) discover models, (2) select the
#' best applicable model for a given tree, and (3) select the correct model function.
#'
#'
#' @return A tibble with one row per model and metadata fields used for model selection.
#' @export
volume_model_registry <- function() {
  tibble::tibble(
    model_id = c(
      "national_ung_dbh",
      "national_ung_dbh_ht",
      "regional_kozak94",
      "regional_honer83",
      "regional_huang94",
      "regional_zakrzewski2013",
      "regional_galbella94",
      "regional_klos2007",
      "regional_sharma2021",
      "regional_fortin2007",
      "regional_nigh2016",
      "regional_NL"
    ),

    reference = c(
      "Ung et al. 2013",
      "Ung et al. 2013",
      "Kozak 1994",
      "Honer et al. 1983",
      "Huang 1994",
      "Zakrzewski & Penner 2013",
      "Gal & Bella 1994",
      "Klos et al. 2007",
      "Sharma 2021",
      "Fortin et al. 2007",
      "Nigh 2016",
      "Honer 1967; Ker 1974; Warren & Meades 1986"
    ),

    engine = c(
      "vol_ung2013",
      "vol_ung2013",
      "vol_kozak94",
      "vol_honer83",
      "vol_huang94",
      "vol_zakrzewski2013",
      "vol_galbella94",
      "vol_klos2007",
      "vol_sharma2021",
      "vol_fortin2007",
      "vol_nigh2016",
      "vol_nl"
    ),

    requires_ht = c(
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    ),

    scope = c(
      "national",
      "national",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional"
    ),

    province_scope = list(
      "ALL",
      "ALL",
      "BC",
      c("ON", "QC", "NB", "NS", "PE", "NL"),
      "AB",
      "ON",
      "SK",
      "MB",
      c("ON", "QC", "NB", "NS", "PE", "NL"),
      "QC",
      "BC",
      "NL"
    ),

    # human-readable (docs)
    subregion_desc = c(
      "none",
      "none",
      "BEC zone",
      "none",
      "optional AB subregion",
      "none",
      "none",
      "optional MB ecozone",
      "none",
      "none",
      "BEC zone or Coast/Interior",
      "NL forest districts"
    ),

    # machine-readable (selection logic)
    subregion_required = c(
      FALSE, # national_ung_dbh
      FALSE, # national_ung_dbh_ht
      TRUE, # regional_kozak94 requires BEC zone
      FALSE, # honer
      FALSE, # huang: province-wide works without specifying subregion
      FALSE, # zakrzewski
      FALSE, # galbella
      FALSE, # klos: can run province-wide, ecozone optional
      FALSE, # sharma
      FALSE, # fortin
      TRUE, # nigh 2016
      FALSE # NL model from OSM
    ),

    # engine arg name for the optional/required subregion
    subregion_arg = c(
      NA_character_,
      NA_character_,
      "BEC_zone",
      NA_character_,
      "subregion", # vol_huang94() takes subregion=...
      NA_character_,
      NA_character_,
      "subregion", # vol_klos2007() takes subregion=...
      NA_character_,
      NA_character_,
      "subregion", # nigh2016 works with bec zones or coast/interior,
      "district" # NL district
    ),

    description = c(
      "National taper model for Canada, available in two variants: DBH-only and DBH with total height",
      "National taper model for Canada, available in two variants: DBH-only and DBH with total height",
      "Provincial taper model for British Columbia; requires BEC zone as a subregion input",
      "Regional volume models for central and eastern Canada, applicable across multiple provinces",
      "Provincial taper model for Alberta based on the Kozak variable-exponent form; applicable at the province level or by Alberta subregions",
      "Provincial taper model for Ontario",
      "Provincial taper model for Saskatchewan based on the Kozak variable-exponent form",
      "Provincial taper model for Manitoba based on the Kozak variable-exponent form; applicable at the province level or by ecozone",
      "Regional volume models for central and eastern Canada, applicable across multiple provinces",
      "Provincial merchantable volume model for Quebec",
      "Total and merchantable volume equations for BC",
      "Total and merchantable volume for Newfoundland and Labrador"
    ),

    rank = c(10, 20, 90, 60, 90, 90, 90, 90, 70, 90, 80, 90),

    params_key = c(
      "parameters_NationalTaperModelsDBH",
      "parameters_NationalTaperModelsDBHHT",
      "parameters_Kozak94",
      "parameters_Honer",
      "parameters_Huang94",
      "parameters_Zakrzewski2013",
      "parameters_GalBella94",
      "parameters_Klos2007",
      "parameters_Sharma2021",
      "parameters_fortin2007",
      "parameters_Nigh2016",
      "parameters_volNL"
    )
  )
}


# internal
get_params_tbl <- function(params_key) {
  ns <- asNamespace("CanadaForestAllometry")

  # internal (sysdata.rda)
  if (exists(params_key, envir = ns, inherits = FALSE)) {
    obj <- get(params_key, envir = ns, inherits = FALSE)
    if (!is.data.frame(obj)) {
      rlang::abort(paste0(
        "`",
        params_key,
        "` exists but is not a data.frame/tibble."
      ))
    }
    return(obj)
  }

  # optional fallback for non-parameter objects stored in data/
  if (!startsWith(params_key, "parameters_")) {
    tmp <- new.env(parent = emptyenv())
    utils::data(
      list = params_key,
      package = "CanadaForestAllometry",
      envir = tmp
    )
    if (exists(params_key, envir = tmp, inherits = FALSE)) {
      return(get(params_key, envir = tmp, inherits = FALSE))
    }
  }

  rlang::abort(paste0("Internal params object not found: `", params_key, "`."))
}


# internal
extract_species_from_params <- function(tbl) {
  stopifnot(inherits(tbl, "data.frame"))

  # find a plausible species column
  candidates <- c("species", "Species", "spp", "sp", "SPP", "species_code")
  sp_col <- candidates[candidates %in% names(tbl)][1]

  if (is.na(sp_col)) {
    return(character(0))
  }

  sp <- tbl[[sp_col]]

  sp <- sp[!is.na(sp)]
  sp <- unique(as.character(sp))
  sp <- sort(sp)

  sp
}

#' Volume model registry with species coverage
#'
#' @return A tibble like `volume_model_registry()` plus:
#'   - `species` (list-column of character vectors)
#'   - `n_species` (integer)
#'   - `species_text` (collapsed string for printing)
#' @keywords internal
volume_model_registry_species <- function() {
  reg <- volume_model_registry()

  # Backward-compatible defaults (in case older registry doesn't have them yet)
  if (!"subregion_required" %in% names(reg)) {
    reg <- reg |>
      dplyr::mutate(subregion_required = FALSE)
  }
  if (!"subregion_arg" %in% names(reg)) {
    reg <- reg |>
      dplyr::mutate(subregion_arg = NA_character_)
  }
  if (!"subregion_type" %in% names(reg)) {
    reg <- reg |>
      dplyr::mutate(subregion_type = "none")
  }

  reg |>
    dplyr::rowwise() |>
    dplyr::mutate(
      species = list({
        # make failures easier to debug
        tryCatch(
          {
            params <- get_params_tbl(params_key)
            extract_species_from_params(params)
          },
          error = function(e) {
            stop(
              sprintf(
                "Failed to build species list for model_id=%s (params_key=%s): %s",
                model_id,
                params_key,
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        )
      }),
      n_species = length(species),
      species_text = dplyr::if_else(
        n_species == 0L,
        NA_character_,
        paste(species, collapse = ", ")
      )
    ) |>
    dplyr::ungroup()
}

# x <- volume_model_registry_species()
