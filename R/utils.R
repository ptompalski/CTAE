prov_codes <- tibble::tribble(
  ~province                   , ~code ,
  "Alberta"                   , "AB"  ,
  "British Columbia"          , "BC"  ,
  "Manitoba"                  , "MB"  ,
  "New Brunswick"             , "NB"  ,
  "Newfoundland and Labrador" , "NL"  ,
  "Northwest Territories"     , "NT"  ,
  "Nova Scotia"               , "NS"  ,
  "Nunavut"                   , "NU"  ,
  "Ontario"                   , "ON"  ,
  "Prince Edward Island"      , "PE"  ,
  "Quebec"                    , "QC"  ,
  "Saskatchewan"              , "SK"  ,
  "Yukon"                     , "YT"
)

prov_aliases <- tibble::tribble(
  ~alias  , ~code ,
  "PEI"   , "PE"  ,
  "P.E.I" , "PE"  ,
  "NFLD"  , "NL"  ,
  "NF"    , "NL"  ,
  "PQ"    , "QC"  ,
  "QUE"   , "QC"  ,
  "YK"    , "YT"  ,
  "NWT"   , "NT"
)

# internal
supported_provinces <- function() {
  stopifnot(exists("prov_codes", inherits = TRUE))

  prov_codes |>
    dplyr::pull(.data$code) |>
    unique() |>
    sort()
}

#' Standardize jurisdiction (province and territorial) codes
#'
#'
#'
#' @keywords internal
standardize_jurisdiction_code <- function(x) {
  x_clean <- x |>
    stringr::str_trim() |>
    stringr::str_to_upper()

  x_fixed <- x_clean |>
    dplyr::recode(!!!stats::setNames(prov_aliases$code, prov_aliases$alias))

  unknown <- setdiff(
    unique(x_fixed),
    prov_codes$code
  )

  if (length(unknown) > 0) {
    stop(
      "Unknown province/territory codes found: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  x_fixed
}

normalize_province_scope <- function(x) {
  # x is a list element from province_scope
  if (length(x) == 1 && identical(x[[1]], "ALL")) {
    return(supported_provinces())
  }
  unique(unlist(x))
}

select_volume_models <- function(jurisdiction) {
  prov <- standardize_jurisdiction_code(jurisdiction)

  volume_model_registry() |>
    dplyr::mutate(
      prov_vec = purrr::map(province_scope, normalize_province_scope),
      supported = purrr::map_lgl(prov_vec, ~ prov %in% .x)
    ) |>
    dplyr::filter(supported) |>
    dplyr::arrange(rank)
}


#' Standardize NFI species codes
#'
#' Converts compact codes like "PICEGLA" to "PICE.GLA". Keeps "ALL" unchanged.
#'
#' @keywords internal
standardize_species_code <- function(x, keep_all = TRUE) {
  x0 <- x |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_to_upper()

  # Keep "ALL" exactly as "ALL"
  is_all <- keep_all & !is.na(x0) & x0 == "ALL"

  y <- x0

  # Remove non-alphanumerics EXCEPT dot
  y <- y |>
    stringr::str_replace_all("[^A-Z0-9\\.]", "")

  # Already in dotted form:
  # - 4.3 (species)
  # - 4.SPP (genus-level)
  # - 4.3.3 (variety)
  is_dotted_ok <- stringr::str_detect(
    y,
    "^[A-Z]{4}\\.(?:[A-Z]{3}|SPP)(?:\\.[A-Z]{3})?$"
  )

  # Compact forms:
  # - 7 letters: 4+3  -> 4.3
  # - 10 letters: 4+3+3 -> 4.3.3
  is_compact_7 <- stringr::str_detect(y, "^[A-Z]{7}$")
  is_compact_10 <- stringr::str_detect(y, "^[A-Z]{10}$")

  y[is_compact_7] <- stringr::str_replace(
    y[is_compact_7],
    "^([A-Z]{4})([A-Z]{3})$",
    "\\1.\\2"
  )

  y[is_compact_10] <- stringr::str_replace(
    y[is_compact_10],
    "^([A-Z]{4})([A-Z]{3})([A-Z]{3})$",
    "\\1.\\2.\\3"
  )

  # Validate
  is_ok <- is_all |
    stringr::str_detect(y, "^[A-Z]{4}\\.(?:[A-Z]{3}|SPP)(?:\\.[A-Z]{3})?$") |
    is.na(y)

  bad <- unique(y[!is_ok])
  if (length(bad) > 0) {
    rlang::abort(
      paste0(
        "Unrecognized species codes (expected code like 'PICE.GLA', 'PICE.SPP', 'PICE.MAR.AAA', ",
        "or compact 'PICEGLA' / 'PICEMARAAA'): ",
        paste(bad, collapse = ", ")
      ),
      class = "ctae_invalid_species_code"
    )
  }

  # Restore ALL exactly
  y[is_all] <- "ALL"
  y
}
