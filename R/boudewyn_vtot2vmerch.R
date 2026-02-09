# ---- total -> merch (B14) ----------------------------------------------------

total_to_merch_build_keys <- function(species, jurisdiction, ecozone) {
  # species/jurisdiction/ecozone are assumed already recycled to same length
  n <- length(species)

  if (length(jurisdiction) != n || length(ecozone) != n) {
    rlang::abort(
      "Internal error: inputs to total_to_merch_build_keys() must have equal length.",
      class = "ctae_internal_recycling_error"
    )
  }

  if (any(!is.na(ecozone) & !ecozone %in% 1:15)) {
    rlang::abort(
      "Invalid ecozone code detected. Valid ecozone codes are integers 1-15.",
      class = "ctae_invalid_ecozone"
    )
  }

  juris_id <- standardize_jurisdiction_code(jurisdiction)
  species_nfi <- standardize_species_code(species, keep_all = FALSE)
  sp <- v2b_split_species_nfi(species_nfi)

  tibble::tibble(
    .row_id = seq_len(n),
    juris_id = juris_id,
    ecozone = ecozone,
    species_nfi = species_nfi,
    genus = sp$genus
  )
}


total_to_merch_fetch_params <- function(
  keys,
  params_tbl = parameters_v2b$B14
) {
  join_cols <- c("juris_id", "ecozone", "genus")

  out <- keys |>
    dplyr::left_join(
      params_tbl,
      by = join_cols,
      relationship = "many-to-many"
    ) |>
    dplyr::arrange(.row_id)

  out <- assert_single_match(out, "B14")

  # Use 'a' as marker (must be non-NA if a row exists)
  missing <- out |>
    dplyr::filter(
      is.na(.data$a) | is.na(.data$b) | is.na(.data$c) | is.na(.data$k)
    )
  if (nrow(missing) > 0) {
    miss_rows <- missing |>
      dplyr::select(.row_id, juris_id, ecozone, genus, species_nfi) |>
      dplyr::distinct()

    rlang::abort(
      paste0(
        "total_to_merch_fetch_params(): no B14 parameters found for some rows.\n",
        paste(
          utils::capture.output(print(utils::head(miss_rows, 10))),
          collapse = "\n"
        )
      ),
      class = "ctae_missing_params_b14"
    )
  }

  out
}

total_to_merch_prop <- function(vol_total, k, a, b, c) {
  vol_total <- as.numeric(vol_total)
  k <- as.numeric(k)
  a <- as.numeric(a)
  b <- as.numeric(b)
  c <- as.numeric(c)

  dplyr::if_else(
    is.na(vol_total) | is.na(k) | is.na(a) | is.na(b) | is.na(c),
    NA_real_,
    k + a * (1 - exp(b * vol_total))^c
  )
}

#' Convert total volume to merchantable volume (Boudewyn et al. 2007; Appendix 6 / Table 14)
#'
#' Uses Boudewyn et al. (2007) Appendix 6 (Table 14) parameters
#' to convert total volume (all live trees) to merchantable volume via a genus-level model.
#'
#' @param vol_total Numeric vector. Total volume per hectare (all live trees).
#' @param species Character vector. NFI species codes (e.g. \code{"PICE.MAR"}). Only genus is used.
#' @param jurisdiction Character vector. Jurisdiction code (e.g. \code{"AB"}).
#' @param ecozone Numeric vector. Ecozone code (1--15).
#' @param include_prop Logical. If TRUE, return the predicted merchantable proportion.
#' @param clamp_prop Logical. If TRUE (default), clamp predicted proportion to [0, 1].
#' @param warn_outside Logical. If TRUE (default), warn when \code{vol_total < volmin}.
#' @param clamp_x Logical. If TRUE, replace \code{vol_total} with \code{pmax(vol_total, volmin)}
#'   before computing the proportion model. Disabled by default.
#'
#' @return A tibble with one row per input and column \code{merchantable_volume}.
#'   Optionally includes \code{prop_merch} and \code{vol_total_used}.
#'@examples
#' # ---- single stand ----------------------------------------------------------
#' vol_total_to_merchantable(
#'   vol_total = 300,
#'   species = "PICE.MAR",
#'   jurisdiction = "AB",
#'   ecozone = 4
#' )
#'
#' # ---- return predicted merchantable proportion ------------------------------
#' vol_total_to_merchantable(
#'   vol_total = 300,
#'   species = "PICE.MAR",
#'   jurisdiction = "AB",
#'   ecozone = 4,
#'   include_prop = TRUE
#' )
#'
#' # ---- vectorized over volume ------------------------------------------------
#' vol_total_to_merchantable(
#'   vol_total = c(50, 100, 300, 600),
#'   species = "PICE.MAR",
#'   jurisdiction = "AB",
#'   ecozone = 4,
#'   include_prop = TRUE
#' )
#'
#' # ---- tidyverse workflow ----------------------------------------------------
#' library(dplyr)
#' library(tidyr)
#'
#' stands <- tibble::tibble(
#'   stand_id = 1:4,
#'   vol_total = c(50, 150, 350, 600),
#'   species = c("PICE.MAR", "PICE.GLA", "ABIE.BAL", "PSEU.MEN"),
#'   jurisdiction = c("AB", "ON", "QC", "BC"),
#'   ecozone = c(6, 6, 8, 13)
#' )
#'
#' stands |>
#'   mutate(
#'     merch = vol_total_to_merchantable(
#'       vol_total = vol_total,
#'       species = species,
#'       jurisdiction = jurisdiction,
#'       ecozone = ecozone,
#'       include_prop = TRUE
#'     )
#'   ) |>
#'   unnest(merch)
#'
#' # ---- chaining with Boudewyn v2b() ------------------------------------------
#' # Convert total volume -> merchantable volume -> biomass components
#' stands |>
#'   mutate(
#'     merch = vol_total_to_merchantable(
#'       vol_total = vol_total,
#'       species = species,
#'       jurisdiction = jurisdiction,
#'       ecozone = ecozone
#'     ),
#'     biomass = v2b(
#'       volume = merch$vol_merchantable,
#'       species = species,
#'       jurisdiction = jurisdiction,
#'       ecozone = ecozone
#'     )
#'   ) |>
#'   unnest(biomass)
#' @export
vol_total_to_merchantable <- function(
  vol_total,
  species,
  jurisdiction,
  ecozone,
  include_prop = FALSE,
  clamp_prop = TRUE,
  warn_outside = TRUE,
  clamp_x = FALSE
) {
  n <- max(
    length(vol_total),
    length(species),
    length(jurisdiction),
    length(ecozone)
  )

  vol_total <- rep_len(as.numeric(vol_total), n)
  species <- rep_len(species, n)
  jurisdiction <- rep_len(jurisdiction, n)
  ecozone <- rep_len(ecozone, n)

  keys <- total_to_merch_build_keys(species, jurisdiction, ecozone)

  p14 <- total_to_merch_fetch_params(keys, params_tbl = parameters_v2b$B14)

  outside <- !is.na(vol_total) &
    !is.na(p14$volmin) &
    vol_total < p14$volmin

  if (isTRUE(warn_outside) && any(outside, na.rm = TRUE)) {
    rlang::warn(
      "Some vol_total values are below the calibration minimum (volmin) for Boudewyn Table 14. Results are extrapolations.",
      class = "ctae_b14_extrapolation"
    )
  }

  vol_total_used <- if (isTRUE(clamp_x)) {
    pmax(vol_total, p14$volmin)
  } else {
    vol_total
  }

  prop <- total_to_merch_prop(
    vol_total = vol_total_used,
    k = p14$k,
    a = p14$a,
    b = p14$b,
    c = p14$c
  )

  if (isTRUE(clamp_prop)) {
    prop <- pmin(pmax(prop, 0), 1)
  }

  merch <- prop * vol_total_used

  out <- tibble::tibble(vol_merchantable = merch)

  if (isTRUE(include_prop)) {
    out <- dplyr::mutate(out, prop_merch = prop)
  }
  if (isTRUE(clamp_x)) {
    out <- dplyr::mutate(out, vol_total_used = vol_total_used)
  }

  out
}
