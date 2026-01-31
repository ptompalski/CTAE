#' Volume model registry
#'
#' Returns a registry (metadata table) describing tree volume models available in CTAE.
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
      "regional_sharma2021"
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
      "Sharma 2021"
    ),

    # comment = c(

    # ),

    # function names to run the models
    engine = c(
      "vol_national_dbh",
      "vol_national_dbh_ht",
      "vol_kozak94",
      "vol_honer83",
      "vol_huang94",
      "vol_zakrzewski2013",
      "vol_galbella94",
      "vol_klos2007",
      "vol_sharma2021"
    ),

    # What inputs are required?
    requires_ht = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),

    # Geographic scope: used for ranking + selection
    scope = c(
      "national",
      "national",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional",
      "regional"
    ),

    # # Province applicability. "ALL" means any province can use it (subject to params).
    # # Kozak94 is BC-specific in your legacy implementation.
    province_scope = list(
      "ALL",
      "ALL",
      "BC",
      c("ON", "QC", "NB", "NS", "PE", "NL"),
      "AB",
      "ON",
      "SK",
      "MN",
      c("ON", "QC", "NB", "NS", "PE", "NL")
    ),

    # # Subregion expectation:
    subregion_type = c(
      "none",
      "none",
      "BEC zone required",
      "none",
      "Province-wide or AB subregions",
      "none",
      "none",
      "Province-wide or ecozones",
      "none"
    ),

    # Rank: higher is preferred in "auto" mode.
    # Suggested preference: regional > national; and if ht is available prefer ht models.
    rank = c(10, 20, 90, 60, 90, 90, 90, 90, 70),

    # Key to request params from get_volume_params()
    params_key = c(
      "parameters_NationalTaperModelsDBH",
      "parameters_NationalTaperModelsDBHHT",
      "parameters_Kozak94",
      "parameters_Honer",
      "parameters_Huang94",
      "parameters_Zakrzewski2013",
      "parameters_GalBella94",
      "parameters_Klos2007",
      "parameters_Sharma2021"
    )
  )
}


# internal
get_params_tbl <- function(params_key) {
  stopifnot(
    is.character(params_key),
    length(params_key) == 1,
    nzchar(params_key)
  )

  # 1) If package is attached, datasets usually live here
  pkg_env <- tryCatch(as.environment("package:CTAE"), error = function(e) NULL)
  if (
    !is.null(pkg_env) && exists(params_key, envir = pkg_env, inherits = FALSE)
  ) {
    return(get(params_key, envir = pkg_env, inherits = FALSE))
  }

  # 2) Some internal objects live in the namespace
  ns_env <- asNamespace("CTAE")
  if (exists(params_key, envir = ns_env, inherits = FALSE)) {
    return(get(params_key, envir = ns_env, inherits = FALSE))
  }

  # 3) Try loading from data/ via utils::data()
  tmp <- rlang::env()
  ok <- tryCatch(
    {
      utils::data(list = params_key, package = "CTAE", envir = tmp)
      exists(params_key, envir = tmp, inherits = FALSE)
    },
    error = function(e) FALSE
  )

  if (ok) {
    return(get(params_key, envir = tmp, inherits = FALSE))
  }

  rlang::abort(paste0(
    "Internal params object not found: `",
    params_key,
    "`.\n",
    "Tried: package env (package:CTAE), namespace, and utils::data()."
  ))
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

  reg %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      species = list({
        params <- get_params_tbl(params_key)
        extract_species_from_params(params)
      }),
      n_species = length(species),
      species_text = dplyr::if_else(
        n_species == 0L,
        NA_character_,
        paste(species, collapse = ", ")
      )
    ) %>%
    dplyr::ungroup()
}
# x <- volume_model_registry_species()
