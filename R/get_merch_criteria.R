#' Get merchantability criteria for a jurisdiction/species
#'
#' Convenience helper to fetch CBM-style merchantability criteria (stump height,
#' top diameter, and minimum DBH) from the internal `merchcrit` lookup table.
#'
#' Before lookup, inputs are standardized using internal helpers:
#' * `standardize_prov_code()` for `jurisdiction`
#' * `standardize_species_code()` for `species` (keeps `"ALL"` as-is)
#'
#' Rules:
#' * For British Columbia (`"BC"`), `species` is required and criteria are looked up
#'   using (Province + Species). If no exact match is found, the function falls back
#'   to a BC-wide row if present (e.g., `Species == "ALL"` or `NA`).
#' * For other jurisdictions, criteria are jurisdiction-level (Province) only.
#'
#' Stump height is returned in metres (converted from centimetres).
#'
#' @param jurisdiction Province/Teritory code (e.g., `"BC"`, `"AB"`).
#' @param species Species code. Required for `"BC"`.
#'
#' @return A one-row tibble with columns:
#'   `jurisdiction`, `species`, `stumpht_m`, `topdbh_cm`, `mindbh_cm`.
#'   If no match is found, returns one-row tibble with `NA` values.
#'
#' @examples
#' get_merch_criteria("ON")
#' get_merch_criteria("PEI")             # standardized to "PE"
#' get_merch_criteria("BC", "picegla")   # standardized to "PICE.GLA"
#'
#' @export
get_merch_criteria <- function(
  jurisdiction,
  species = NA_character_
) {
  stopifnot(
    is.character(jurisdiction),
    length(jurisdiction) == 1,
    nzchar(jurisdiction)
  )

  jurisdiction_std <- CTAE:::standardize_jurisdiction_code(jurisdiction)

  species_std <- species
  if (!is.na(species_std) && nzchar(species_std)) {
    species_std <- CTAE:::standardize_species_code(species_std)
  }

  # ---- internal data ----
  stopifnot(exists("merchcrit", inherits = TRUE))

  required <- c("Province", "StumpHT", "TopDBH", "MinDBH")
  missing_cols <- setdiff(required, names(merchcrit))
  if (length(missing_cols) > 0) {
    stop(
      "Internal dataset `merchcrit` is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  is_bc <- identical(jurisdiction_std, "BC")

  if (is_bc) {
    if (is.na(species_std) || !nzchar(species_std)) {
      stop("For jurisdiction 'BC', `species` must be provided.")
    }
    if (!("Species" %in% names(merchcrit))) {
      stop(
        "Internal dataset `merchcrit` does not contain a `Species` column required for BC lookups."
      )
    }

    hits <- merchcrit[
      merchcrit$Province == jurisdiction_std & merchcrit$Species == species_std,
      ,
      drop = FALSE
    ]

    # Fallback to a BC-wide row if present (legacy-friendly)
    if (nrow(hits) == 0) {
      hits <- merchcrit[
        merchcrit$Province == jurisdiction_std &
          (is.na(merchcrit$Species) | merchcrit$Species == "ALL"),
        ,
        drop = FALSE
      ]
    }
  } else {
    # jurisdiction-only lookup (ignore species)
    hits <- merchcrit[merchcrit$Province == jurisdiction_std, , drop = FALSE]
  }

  if (nrow(hits) == 0) {
    return(tibble::tibble(
      jurisdiction = jurisdiction_std,
      species = species_std,
      stumpht_m = NA_real_,
      topdbh_cm = NA_real_,
      mindbh_cm = NA_real_
    ))
  }

  hits <- hits[1, , drop = FALSE]

  tibble::tibble(
    jurisdiction = jurisdiction_std,
    species = species_std,
    stumpht_m = as.numeric(hits$StumpHT) / 100, # cm -> m
    topdbh_cm = as.numeric(hits$TopDBH),
    mindbh_cm = as.numeric(hits$MinDBH)
  )
}
