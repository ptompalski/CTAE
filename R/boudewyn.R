assert_single_match <- function(x, component) {
  bad <- x |>
    dplyr::count(.row_id, name = "n_match") |>
    dplyr::filter(.data$n_match != 1)

  if (nrow(bad) > 0) {
    rlang::abort(
      paste0(
        "v2b_fetch_params(): expected exactly 1 match per row for component '",
        component,
        "'. ",
        "Problem row_id(s): ",
        paste(bad$.row_id, collapse = ", ")
      ),
      class = "ctae_v2b_nonunique_params"
    )
  }

  x
}

v2b_component_keys <- function(component) {
  switch(
    component,
    B3 = c("juris_id", "ecozone", "genus", "species", "variety"),
    B4 = c("juris_id", "ecozone", "genus", "species", "variety"),
    B6_vol = c("juris_id", "ecozone", "genus", "species", "variety"),
    B6_tb = c("juris_id", "ecozone", "genus", "species", "variety"),
    B7_vol = c("juris_id", "ecozone", "genus", "species", "variety"),
    B7_tb = c("juris_id", "ecozone", "genus", "species", "variety"),
    # genus-level components
    B5 = c("juris_id", "ecozone", "genus"),
    B14 = c("juris_id", "ecozone", "genus"),
    rlang::abort(
      paste0("Unknown component '", component, "'."),
      class = "ctae_v2b_unknown_component"
    )
  )
}

# helper: determine which column indicates a successful join
v2b_match_marker <- function(out, p, join_cols) {
  marker <- intersect(c("source_table", "component"), names(out))
  if (length(marker) > 0) {
    return(marker[1])
  }

  candidates <- setdiff(names(p), join_cols)
  candidates <- intersect(candidates, names(out))

  if (length(candidates) == 0) {
    rlang::abort(
      "v2b_fetch_params(): could not determine match marker column (parameter table has no non-key columns).",
      class = "ctae_v2b_bad_param_table"
    )
  }

  candidates[1]
}

v2b_fetch_params <- function(
  component,
  keys,
  params_list = parameters_v2b,
  allow_variety_fallback = TRUE,
  allow_species_spp_fallback = FALSE
) {
  if (!component %in% names(params_list)) {
    rlang::abort(
      paste0(
        "v2b_fetch_params(): component '",
        component,
        "' not found in params_list."
      ),
      class = "ctae_v2b_missing_component"
    )
  }

  p <- params_list[[component]]
  join_cols <- v2b_component_keys(component)

  missing_in_keys <- setdiff(c(".row_id", join_cols), names(keys))
  if (length(missing_in_keys) > 0) {
    rlang::abort(
      paste0(
        "v2b_fetch_params(): keys missing required column(s): ",
        paste(missing_in_keys, collapse = ", ")
      ),
      class = "ctae_v2b_bad_keys"
    )
  }

  missing_in_p <- setdiff(join_cols, names(p))
  if (length(missing_in_p) > 0) {
    rlang::abort(
      paste0(
        "v2b_fetch_params(): parameter table '",
        component,
        "' missing required column(s): ",
        paste(missing_in_p, collapse = ", ")
      ),
      class = "ctae_v2b_bad_param_table"
    )
  }

  out1 <- keys |>
    dplyr::left_join(p, by = join_cols, relationship = "many-to-many")

  marker_col <- v2b_match_marker(out1, p, join_cols)
  has_match <- !is.na(out1[[marker_col]])

  if (all(has_match)) {
    out1 <- out1 |> dplyr::arrange(.row_id) # keep input order
    return(assert_single_match(out1, component))
  }

  out <- out1

  # fallback 1: variety -> NA
  if (allow_variety_fallback && "variety" %in% join_cols) {
    need <- keys[!has_match, , drop = FALSE] |>
      dplyr::mutate(variety = NA_character_)

    out_fb <- need |>
      dplyr::left_join(p, by = join_cols, relationship = "many-to-many")

    out <- dplyr::bind_rows(out[has_match, , drop = FALSE], out_fb)
  }

  marker_col <- v2b_match_marker(out, p, join_cols)
  has_match2 <- !is.na(out[[marker_col]])

  if (all(has_match2)) {
    out <- out |> dplyr::arrange(.row_id) # keep input order
    return(assert_single_match(out, component))
  }

  # fallback 2: species -> "SPP"
  if (allow_species_spp_fallback && all(c("species", "genus") %in% join_cols)) {
    need <- keys[!has_match2, , drop = FALSE] |>
      dplyr::mutate(species = "SPP", variety = NA_character_)

    out_fb <- need |>
      dplyr::left_join(p, by = join_cols, relationship = "many-to-many")

    out <- dplyr::bind_rows(out[has_match2, , drop = FALSE], out_fb)
    out <- out |> dplyr::arrange(.row_id) # keep input order
    out <- assert_single_match(out, component)

    marker_col <- v2b_match_marker(out, p, join_cols)
    has_match3 <- !is.na(out[[marker_col]])

    if (all(has_match3)) return(out)
  }

  # improved missing rows: include species_nfi for user-friendly reporting
  missing_rows <- out |>
    dplyr::filter(!has_match2) |>
    dplyr::select(
      .row_id,
      juris_id,
      ecozone,
      species_nfi,
      genus,
      dplyr::any_of(c("species", "variety"))
    ) |>
    dplyr::distinct()

  rlang::abort(
    paste0(
      "v2b_fetch_params(): no parameters found for component '",
      component,
      "' for some rows.\n",
      paste(
        utils::capture.output(print(utils::head(missing_rows, 10))),
        collapse = "\n"
      )
    ),
    class = "ctae_v2b_missing_params"
  )
}

v2b_split_species_nfi <- function(species_nfi) {
  parts <- stringr::str_split(species_nfi, "\\.", simplify = TRUE)

  genus <- parts[, 1]
  spp <- parts[, 2]
  var <- if (ncol(parts) >= 3) {
    parts[, 3]
  } else {
    rep(NA_character_, length(species_nfi))
  }
  var[var == ""] <- NA_character_

  tibble::tibble(
    genus = genus,
    species = spp,
    variety = var
  )
}

v2b_build_keys <- function(species, jurisdiction, ecozone) {
  n <- max(length(species), length(jurisdiction), length(ecozone))
  species <- rep_len(species, n)
  jurisdiction <- rep_len(jurisdiction, n)
  ecozone <- rep_len(ecozone, n)

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
    genus = sp$genus,
    species = sp$species,
    variety = sp$variety
  )
}


# ---- math helpers ------------------------------------------------------------

v2b_stem_merch <- function(volume, a, b) {
  volume <- as.numeric(volume)
  a <- as.numeric(a)
  b <- as.numeric(b)

  dplyr::if_else(
    is.na(volume) | is.na(a) | is.na(b),
    NA_real_,
    a * (volume^b)
  )
}

v2b_nonmerch_factor <- function(b_m, a, b, k, cap) {
  b_m <- as.numeric(b_m)
  a <- as.numeric(a)
  b <- as.numeric(b)
  k <- as.numeric(k)
  cap <- as.numeric(cap)

  f <- dplyr::if_else(
    is.na(b_m) | is.na(a) | is.na(b) | is.na(k),
    NA_real_,
    k + a * (b_m^b)
  )

  dplyr::if_else(!is.na(cap) & !is.na(f) & f > cap, cap, f)
}

v2b_sapling_factor <- function(b_nm, a, b, k, cap) {
  b_nm <- as.numeric(b_nm)
  a <- as.numeric(a)
  b <- as.numeric(b)
  k <- as.numeric(k)
  cap <- as.numeric(cap)

  f <- dplyr::if_else(
    is.na(b_nm) | is.na(a) | is.na(b) | is.na(k),
    NA_real_,
    k + a * (b_nm^b)
  )

  dplyr::if_else(!is.na(cap) & !is.na(f) & f > cap, cap, f)
}

# ---- wrapper: volume -> stem biomass -----------------------------------------

#' Convert merchantable volume to stem biomass (Boudewyn et al. 2007)
#'
#' Calculates stem wood biomass components from gross merchantable volume
#' using the model-based volume-to-biomass conversion equations of
#' Boudewyn et al. (2007), with updated parameters from the National Forest
#' Inventory (NFI).
#'
#' The function supports species-, genus-, and ecozone-specific parameter
#' selection and is vectorized over all inputs.
#'
#' @param volume Numeric vector. Gross merchantable volume per hectare.
#' @param species Character vector. Tree species codes in NFI format
#'   (e.g., \code{"PICE.MAR"}). Genus-level codes (\code{"PICE.SPP"}) and
#'   variety-level codes (\code{"PICE.MAR.AAA"}) are supported.
#' @param jurisdiction Character vector. Jurisdiction code (e.g., \code{"AB"}).
#' @param ecozone Numeric vector. Ecozone code (1--15).
#'
#' @return
#' A tibble with one row per input observation and the following columns:
#' \describe{
#'   \item{b_m}{Stem wood biomass of merchantable-sized live trees (t/ha).}
#'   \item{b_n}{Stem wood biomass of nonmerchantable-sized live trees (t/ha).}
#'   \item{b_nm}{Total stem wood biomass of merchantable and nonmerchantable trees (t/ha).}
#'   \item{b_s}{Stem wood biomass of sapling-sized live trees (t/ha).}
#'   \item{f_nm}{Nonmerchantable factor (Table 4).}
#'   \item{f_s}{Sapling factor (Table 5; \code{NA} if not applicable).}
#'   \item{has_sapling}{Logical indicating whether sapling parameters were applied.}
#' }
#'
#' @details
#' The function implements Equations 1--3 in Boudewyn et al. (2007) and
#' corresponds to Scenarios 1 and 2 in the original report.
#'
#' This function returns stem wood biomass only. Conversion to total
#' aboveground biomass and biomass components (bark, branches, foliage)
#' requires additional proportion models.
#'
#' @references
#' Boudewyn, P.A.; Song, X.; Magnussen, S.; Gillis, M.D. (2007).
#' Model-based, volume-to-biomass conversion for forested and vegetated land
#' in Canada. Natural Resources Canada, Canadian Forest Service.
#'
#' @examples
#' v2b_stem_biomass(
#'   volume = 350,
#'   species = "PICE.MAR",
#'   jurisdiction = "AB",
#'   ecozone = 4
#' )
v2b_stem_biomass <- function(volume, species, jurisdiction, ecozone) {
  # Build standardized keys (validates species/juris/ecozone)
  keys <- v2b_build_keys(
    species = species,
    jurisdiction = jurisdiction,
    ecozone = ecozone
  )
  n <- nrow(keys)

  # align volume
  volume <- rep_len(as.numeric(volume), n)

  # fetch required params
  p3 <- v2b_fetch_params("B3", keys, allow_variety_fallback = TRUE)
  p4 <- v2b_fetch_params("B4", keys, allow_variety_fallback = TRUE)

  # Eq1
  b_m <- v2b_stem_merch(volume = volume, a = p3$a, b = p3$b)

  # Eq2
  f_nm <- v2b_nonmerch_factor(
    b_m = b_m,
    a = p4$a,
    b = p4$b,
    k = p4$k,
    cap = p4$cap
  )
  b_nm <- f_nm * b_m
  b_n <- b_nm - b_m
  b_mn <- b_m + b_n

  # Sapling params (B5) are genus-level and may be missing -> left-join, compute where present
  p5_tbl <- parameters_v2b$B5

  # keep only needed columns to avoid collisions
  p5_tbl <- p5_tbl |>
    dplyr::select(juris_id, ecozone, genus, a, b, k, cap)

  k5 <- keys |>
    dplyr::select(.row_id, juris_id, ecozone, genus) |>
    dplyr::left_join(
      p5_tbl,
      by = c("juris_id", "ecozone", "genus"),
      relationship = "many-to-many"
    ) |>
    dplyr::arrange(.row_id)

  # If B5 has multiple matches, fail early (should not happen if table is clean)
  k5 <- assert_single_match(k5, "B5")

  has_sapling <- !is.na(k5$a) & !is.na(k5$b) & !is.na(k5$k)

  f_s <- dplyr::if_else(
    has_sapling,
    v2b_sapling_factor(b_nm = b_nm, a = k5$a, b = k5$b, k = k5$k, cap = k5$cap),
    NA_real_
  )

  b_s <- dplyr::if_else(
    has_sapling,
    (f_s * b_nm) - b_nm,
    0
  )

  # Return computed columns ONLY (safe for unnest)
  tibble::tibble(
    b_m = b_m,
    b_n = b_n,
    b_nm = b_nm,
    # b_mn = b_mn,
    b_s = b_s,
    f_nm = f_nm,
    f_s = f_s,
    has_sapling = has_sapling
  )
}

# ---- proportion helpers (vol path only) --------------------------------------

v2b_props_from_table6 <- function(
  x,
  a1,
  a2,
  a3,
  b1,
  b2,
  b3,
  c1,
  c2,
  c3,
  offset = 5
) {
  # Boudewyn Eqs. 4–7 (Table 6): multinomial-logit-style proportions
  x <- as.numeric(x)
  lx <- log(x + offset)

  p_a <- exp(a1 + a2 * x + a3 * lx)
  p_b <- exp(b1 + b2 * x + b3 * lx)
  p_c <- exp(c1 + c2 * x + c3 * lx)

  den <- 1 + p_a + p_b + p_c

  tibble::tibble(
    p_sw = 1 / den,
    p_sb = p_a / den,
    p_br = p_b / den,
    p_fl = p_c / den
  )
}

v2b_apply_caps_table7 <- function(props, caps, renormalize = TRUE) {
  clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)

  out <- props %>%
    dplyr::mutate(
      p_sw = clamp(p_sw, caps$p_sw_low, caps$p_sw_high),
      p_sb = clamp(p_sb, caps$p_sb_low, caps$p_sb_high),
      p_br = clamp(p_br, caps$p_br_low, caps$p_br_high),
      p_fl = clamp(p_fl, caps$p_fl_low, caps$p_fl_high)
    )

  if (renormalize) {
    s <- out$p_sw + out$p_sb + out$p_br + out$p_fl
    out <- out %>%
      dplyr::mutate(
        p_sw = p_sw / s,
        p_sb = p_sb / s,
        p_br = p_br / s,
        p_fl = p_fl / s
      )
  }

  out
}

v2b_clamp_x_table7 <- function(x, caps) {
  # caps has x_min/x_max; clamp x into that domain (row-wise)
  x <- as.numeric(x)
  pmin(pmax(x, caps$x_min), caps$x_max)
}

# ---- wrapper: volume -> total biomass + components (vol path) ----------------

#' Convert merchantable volume to total aboveground biomass and components (Boudewyn et al. 2007)
#'
#' Computes total aboveground biomass (AGB) and biomass components (bark, branches, foliage)
#' from gross merchantable volume per hectare, using the Boudewyn et al. (2007)
#' model-based volume-to-biomass conversion framework (Tables 3--7), with updated
#' NFI parameters.
#'
#' This function uses the volume-based proportion models (Appendix 2 Table 6)
#' and caps (Appendix 2 Table 7). Proportion caps are applied after evaluating the
#' Table 6 equations and (optionally) renormalized to sum to 1.
#'
#' @param volume Numeric vector. Gross merchantable volume per hectare.
#' @param species Character vector. NFI species codes (e.g., "PICE.MAR").
#' @param jurisdiction Character vector. Jurisdiction code (e.g., "AB").
#' @param ecozone Numeric vector. Ecozone code (1--15).
#' @param renormalize Logical. If TRUE, renormalize capped proportions to sum to 1.
#' @param clamp_x Logical. If TRUE, clamp \code{volume} to the calibration range
#'   \code{[x_min, x_max]} from Table 7 before evaluating the proportion equations.
#'
#' @details
#' Extrapolation vs clamping.
#' The Table 7 columns \code{x_min} and \code{x_max} describe the approximate range of the
#' independent variable used to calibrate the proportion models. By default (\code{clamp_x = FALSE}),
#' the function uses the input \code{volume} directly. If \code{volume} falls outside the calibration
#' range, the result is an extrapolation; however, proportion caps (Table 7) are still applied to
#' prevent unrealistic proportions.
#'
#' Setting \code{clamp_x = TRUE} replaces \code{volume} with \code{min(max(volume, x_min), x_max)}
#' before computing proportions. This can substantially change results for high-volume stands and
#' may lead to systematic underestimation of total biomass where \code{volume > x_max}.
#' For this reason, clamping is disabled by default and is intended mainly for sensitivity analyses
#' and debugging.
#'
#' @return A tibble with one row per input and computed columns only:
#' \describe{
#'   \item{b_total}{Total aboveground tree biomass (t/ha).}
#'   \item{b_bark}{Bark biomass (t/ha).}
#'   \item{b_branches}{Branch biomass (t/ha).}
#'   \item{b_foliage}{Foliage biomass (t/ha).}
#'   \item{p_sw}{Proportion of total biomass in stem wood.}
#'   \item{p_sb}{Proportion of total biomass in bark.}
#'   \item{p_br}{Proportion of total biomass in branches.}
#'   \item{p_fl}{Proportion of total biomass in foliage.}
#'   \item{b_stem_total}{Total stem wood biomass used to back-calculate \code{b_total} (t/ha).}
#'   \item{volume_used}{Only returned when \code{clamp_x = TRUE}; the volume actually used in Table 6.}
#' }
v2b_biomass_components <- function(
  volume,
  species,
  jurisdiction,
  ecozone,
  offset = 5, # used in log calculations to protect against log(0)
  renormalize = TRUE,
  clamp_x = FALSE
) {
  # keys
  keys <- v2b_build_keys(
    species = species,
    jurisdiction = jurisdiction,
    ecozone = ecozone
  )
  n <- nrow(keys)
  volume <- rep_len(as.numeric(volume), n)

  # stage 1: stem wood biomass (Tables 3–5)
  stem <- v2b_stem_biomass(
    volume = volume,
    species = species,
    jurisdiction = jurisdiction,
    ecozone = ecozone
  )
  b_stem_total <- stem$b_nm + stem$b_s

  # stage 2: proportions (Table 6 vol) + caps (Table 7 vol)
  p6 <- v2b_fetch_params(
    "B6_vol",
    keys,
    allow_variety_fallback = TRUE,
    allow_species_spp_fallback = FALSE
  )
  p7 <- v2b_fetch_params(
    "B7_vol",
    keys,
    allow_variety_fallback = TRUE,
    allow_species_spp_fallback = FALSE
  )

  # warn on extrapolation (do not modify volume)
  outside <- (volume < p7$x_min) | (volume > p7$x_max)

  if (!clamp_x && any(outside, na.rm = TRUE)) {
    rlang::warn(
      paste0(
        "Some volumes are outside the calibration range of the Boudewyn ",
        "proportion models (Table 7). Results are extrapolations. ",
        "Set clamp_x = TRUE to clamp volume to [x_min, x_max]."
      ),
      class = "ctae_v2b_extrapolation"
    )
  }

  volume_used <- if (clamp_x) pmin(pmax(volume, p7$x_min), p7$x_max) else volume

  props <- v2b_props_from_table6(
    x = volume_used,
    a1 = p6$a1,
    a2 = p6$a2,
    a3 = p6$a3,
    b1 = p6$b1,
    b2 = p6$b2,
    b3 = p6$b3,
    c1 = p6$c1,
    c2 = p6$c2,
    c3 = p6$c3,
    offset = offset
  )

  props <- v2b_apply_caps_table7(props, caps = p7, renormalize = renormalize)

  # convert stem wood -> total biomass
  b_total <- b_stem_total / props$p_sw

  out <- tibble::tibble(
    b_total = b_total,
    b_bark = b_total * props$p_sb,
    b_branches = b_total * props$p_br,
    b_foliage = b_total * props$p_fl,
    p_sw = props$p_sw,
    p_sb = props$p_sb,
    p_br = props$p_br,
    p_fl = props$p_fl,
    b_stem_total = b_stem_total
  )

  if (clamp_x) {
    out <- dplyr::mutate(out, volume_used = volume_used)
  }

  out
}


#' Convert volume to aboveground biomass components (Boudewyn et al. 2007)
#'
#' High-level wrapper for the Boudewyn et al. (2007) model-based volume-to-biomass
#' conversion for live trees. Given gross merchantable volume per hectare, the
#' function:
#' \enumerate{
#'   \item converts volume to stem wood biomass components (Tables 3--5; Eqs. 1--3),
#'   \item computes biomass proportions using the volume-based multinomial-logit
#'         models (Table 6; Eqs. 4--7),
#'   \item applies the Table 7 proportion bounds (caps) to prevent unrealistic
#'         proportions when extrapolating,
#'   \item returns total aboveground biomass and components (bark, branches, foliage).
#' }
#'
#' The function is vectorized over all inputs and is designed to work naturally
#' inside \code{dplyr::mutate()} (typically followed by \code{tidyr::unnest()}).
#'
#' @param volume Numeric vector. Gross merchantable volume per hectare.
#' @param species Character vector. NFI species codes (e.g., \code{"PICE.MAR"}).
#'   Genus-level codes (\code{"PICE.SPP"}) and variety-level codes
#'   (\code{"PICE.MAR.AAA"}) are supported.
#' @param jurisdiction Character vector. Jurisdiction code (e.g., \code{"AB"}).
#' @param ecozone Numeric vector. Ecozone code (1--15).
#' @param include_props Logical. If TRUE, include biomass proportions
#'   (\code{p_sw}, \code{p_sb}, \code{p_br}, \code{p_fl}) in the output.
#' @param include_intermediates Logical. If TRUE, include intermediate stem-biomass
#'   quantities (\code{b_m}, \code{b_n}, \code{b_nm}, \code{b_s}) and factors
#'   (\code{f_nm}, \code{f_s}, \code{has_sapling}, \code{b_stem_total}).
#' @param renormalize Logical. If TRUE (default), renormalize capped proportions
#'   to sum to 1. This can be helpful when multiple components hit bounds.
#' @param clamp_x Logical. If TRUE, clamp \code{volume} to the calibration range
#'   \code{[x_min, x_max]} from Table 7 before evaluating the proportion equations.
#'   Disabled by default.
#'
#' @details
#' Proportion caps (Table 7).
#' The Table 6 proportion models can yield extreme values when extrapolated beyond
#' the calibration range. This implementation applies Table 7 bounds (low/high
#' caps) to each proportion to keep outputs within plausible limits.
#'
#' Extrapolation vs clamping.
#' By default (\code{clamp_x = FALSE}), the input \code{volume} is used directly.
#' If \code{volume} falls outside the calibration range (\code{x_min/x_max}) reported
#' in Table 7, results are extrapolations, although Table 7 proportion caps are still
#' applied. Setting \code{clamp_x = TRUE} replaces \code{volume} with
#' \code{min(max(volume, x_min), x_max)} before computing proportions; this can
#' materially change results for high-volume stands and may lead to systematic
#' underestimation where \code{volume > x_max}. For this reason, clamping is disabled
#' by default.
#'
#' @return A tibble with one row per input observation. By default includes:
#' \describe{
#'   \item{b_total}{Total aboveground tree biomass (t/ha).}
#'   \item{b_bark}{Bark biomass (t/ha).}
#'   \item{b_branches}{Branch biomass (t/ha).}
#'   \item{b_foliage}{Foliage biomass (t/ha).}
#' }
#' Additional columns are included when \code{include_props} and/or
#' \code{include_intermediates} are TRUE.
#'
#' @examples
#' # Single case (one stand)
#' v2b(
#'   volume = 350,
#'   species = "PSEU.MEN",
#'   jurisdiction = "BC",
#'   ecozone = 13
#' )
#'
#' # Include proportions and intermediate quantities used in the calculation
#' v2b(
#'   volume = 350,
#'   species = "PSEU.MEN",
#'   jurisdiction = "BC",
#'   ecozone = 13
#'   include_props = TRUE,
#'   include_intermediates = TRUE
#' )
#'
#' # Tidyverse workflow: compute biomass for multiple stands
#' # (one row per stand) and bind results as new columns.
#' library(dplyr)
#' library(tidyr)
#'
#' stands <- tibble::tibble(
#'   stand_id = 1:3,
#'   volume = c(120, 350, 300),
#'   species = c("PICE.MAR", "PSEU.MEN", "PINU.BAN"),
#'   jurisdiction = c("AB", "BC", "ON"),
#'   ecozone = c(5, 13, 6)
#' )
#'
#' stands |>
#'   mutate(
#'     v2b = v2b(
#'       volume = volume,
#'       species = species,
#'       jurisdiction = jurisdiction,
#'       ecozone = ecozone,
#'       include_props = TRUE
#'     )
#'   ) |>
#'   unnest(v2b)
#'
#' @export
v2b <- function(
  volume,
  species,
  jurisdiction,
  ecozone,
  include_props = FALSE,
  include_intermediates = FALSE,
  renormalize = TRUE,
  clamp_x = FALSE
) {
  offset <- 5

  # compute stem biomass (Tables 3–5)
  stem <- v2b_stem_biomass(
    volume = volume,
    species = species,
    jurisdiction = jurisdiction,
    ecozone = ecozone
  )

  # compute total biomass + components (Tables 6–7 vol)
  comp <- v2b_biomass_components(
    volume = volume,
    species = species,
    jurisdiction = jurisdiction,
    ecozone = ecozone,
    offset = offset,
    renormalize = renormalize,
    clamp_x = clamp_x
  )

  out <- dplyr::bind_cols(stem, comp)

  # minimal default output
  keep <- c("b_total", "b_bark", "b_branches", "b_foliage")

  if (isTRUE(include_props)) {
    keep <- c(keep, "p_sw", "p_sb", "p_br", "p_fl")
  }

  if (isTRUE(include_intermediates)) {
    keep <- c(
      keep,
      "b_m",
      "b_n",
      "b_nm",
      "b_s",
      "f_nm",
      "f_s",
      "has_sapling",
      "b_stem_total"
    )

    # optionally include volume_used if clamping is enabled and the column exists
    if ("volume_used" %in% names(out)) keep <- c(keep, "volume_used")
  }

  out %>%
    dplyr::select(dplyr::all_of(unique(keep)))
}
