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


standardize_ecozone <- function(ecozone) {
  if (is.null(ecozone)) {
    return(NULL)
  }

  ez <- as.vector(ecozone)

  # Build long lookup: one row per allowed name
  lookup_long <- ecozones |>
    tidyr::pivot_longer(
      cols = c(ecozone_name_en, ecozone_name_fr),
      names_to = "lang",
      values_to = "name"
    ) |>
    dplyr::mutate(
      key = stringi::stri_trans_general(tolower(trimws(name)), "Latin-ASCII")
    )

  # Output vector
  out <- rep(NA_integer_, length(ez))

  # Identify numeric-like elements (works for mixed vectors coerced to character)
  is_num <- suppressWarnings(!is.na(as.integer(ez)))

  ## ---- numeric-like inputs ----
  if (any(is_num)) {
    ez_num <- as.integer(ez[is_num])

    if (any(!ez_num %in% 1:15, na.rm = TRUE)) {
      bad <- unique(ez_num[!ez_num %in% 1:15])
      rlang::abort(glue::glue(
        "Invalid ecozone number(s): {bad}. Valid range is 1-15."
      ))
    }

    out[is_num] <- ez_num
  }

  ## ---- name inputs ----
  is_chr <- !is_num
  if (any(is_chr)) {
    ez_key <- stringi::stri_trans_general(
      tolower(trimws(as.character(ez[is_chr]))),
      "Latin-ASCII"
    )

    idx <- match(ez_key, lookup_long$key)
    matched <- lookup_long$ecozone[idx]

    if (anyNA(matched)) {
      bad <- unique(as.character(ez[is_chr])[is.na(matched)])

      rlang::abort(
        glue::glue(
          "Unknown ecozone name(s): {bad}. Use numbers 1-15 or official ecozone names."
        ),
        class = "ctae_unknown_ecozone"
      )
    }

    out[is_chr] <- matched
  }

  out
}


# internal
.get_internal_data <- function(name) {
  ns <- asNamespace("CanadaForestAllometry")
  if (!exists(name, envir = ns, inherits = FALSE)) {
    rlang::abort(paste0(
      "Internal data object `",
      name,
      "` not found in namespace.\n",
      "Expected it in R/sysdata.rda (created via usethis::use_data(..., internal = TRUE))."
    ))
  }
  get(name, envir = ns, inherits = FALSE)
}
