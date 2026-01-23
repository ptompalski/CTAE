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

#' Standardize jurisdiction (province and territorial) codes
#'
#'
#'
#' @keywords internal
standardize_jurisdiction_code <- function(x) {
  x_clean <- x %>%
    str_trim() %>%
    str_to_upper()

  x_fixed <- x_clean %>%
    recode(!!!setNames(prov_aliases$code, prov_aliases$alias))

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


#' Standardize NFI species codes
#'
#' Converts compact codes like "PICEGLA" to "PICE.GLA". Keeps "ALL" unchanged.
#'
#' @keywords internal
standardize_species_code <- function(x, keep_all = TRUE) {
  x0 <- x %>%
    as.character() %>%
    str_trim() %>%
    str_to_upper()

  # Keep "ALL" exactly as "ALL" (and optionally treat ""/NA as NA)
  is_all <- keep_all & !is.na(x0) & x0 == "ALL"

  # Work on everything else
  y <- x0

  # Remove non-alphanumerics EXCEPT dot, then handle dot insertion
  # (we'll re-build the canonical dot format)
  y <- y %>%
    str_replace_all("[^A-Z0-9\\.]", "")

  # If it's already in NFI-like form "ABCD.EFG", keep it
  is_nfi <- str_detect(y, "^[A-Z]{4}\\.[A-Z]{3}$")

  # If it's in compact form "ABCDEFG" (7 letters), convert to "ABCD.EFG"
  is_compact <- str_detect(y, "^[A-Z]{7}$")

  y[is_compact] <- str_replace(
    y[is_compact],
    "^([A-Z]{4})([A-Z]{3})$",
    "\\1.\\2"
  )

  # Optional: sometimes people provide 4+3 with a dash/space (already stripped above)
  # so the compact rule will already catch it after cleaning.

  # Validate: anything not ALL must be either NFI or become NFI
  is_ok <- is_all | str_detect(y, "^[A-Z]{4}\\.[A-Z]{3}$") | is.na(y)

  bad <- unique(y[!is_ok])
  if (length(bad) > 0) {
    stop(
      "Unrecognized species codes (expected NFI like 'PICE.GLA', or compact like 'PICEGLA'): ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  # Restore ALL exactly as ALL (even if later rules touched it)
  y[is_all] <- "ALL"

  y
}
