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
      "regional_kozak88",
      "regional_kozak94",
      "regional_honer83",
      "regional_huang94"
    ),

    label = c(
      "Ung et al. 2013 Canadian national taper models (DBH only)",
      "Ung et al. 2013 Canadian national taper models (DBH + height)",
      "Kozak 1988 taper (regional; DBH + height)",
      "Kozak 1994 taper (BC; BEC zones; DBH + height)",
      "Honer 1983 model (regional; DBH + height)",
      "Huang 1994 model (AB; subregions; DBH + height"
    ),

    # function names to run the models
    engine = c(
      "vol_national_dbh",
      "vol_national_dbh_ht",
      "vol_kozak88_dbh_ht",
      "vol_kozak94",
      "vol_honer83",
      "vol_huang94"
    ),

    # What inputs are required?
    requires_ht = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),

    # Geographic scope: used for ranking + selection
    scope = c(
      "national",
      "national",
      "regional",
      "regional",
      "regional",
      "regional"
    ),

    # # Province applicability. "ALL" means any province can use it (subject to params).
    # # Kozak94 is BC-specific in your legacy implementation.
    # province_scope = c("ALL", "ALL", "ALL", "BC", "ALL", "AB"),

    # # Subregion expectation:
    # # - national: none
    # # - Kozak88/Honer: often "ALL" but table has Subregion column
    # # - Kozak94: BEC zone (or "ALL" if your params are aggregated, but code is BEC-oriented)
    # subregion_type = c(
    #   "none",
    #   "none",
    #   "all_or_subregion",
    #   "bec",
    #   "all_or_subregion"
    # ),

    # Rank: higher is preferred in "auto" mode.
    # Suggested preference: regional > national; and if ht is available prefer ht models.
    rank = c(10, 20, 80, 90, 70, 90),

    # Key to request params from get_volume_params()
    params_key = c(
      "parameters_NationalTaperModelsDBH",
      "parameters_NationalTaperModelsDBHHT",
      "parameters_Kozak88",
      "parameters_Kozak94",
      "parameters_Honer",
      "parameters_HuangV"
    )
  )
}
