#' Estimate tree volume for Newfoundland
#'
#' Implements the Newfoundland individual-tree volume equations used in the
#' Open Stand Model (OSM) API, based on Honer’s refitted formulations.
#'
#' Model structure:
#' - District-level equations (NX-242) are used only for District 2 and 4–18
#'   and only for balsam fir (`ABIE.BAL`) and black spruce (`PICE.MAR`).
#' - All other districts (including District 19 and 1–24 outside 2, 4–18)
#'   fall back to province-wide species equations (NX-122 total + NX-67 merchantable).
#'
#' The original OSM implementation excluded District 19
#' because it produced unstable or unrealistic results. This function reproduces
#' that behavior for consistency.
#'
#' Net merchantable volume:
#' - Net merchantable volume equations are available only for NX-242 district models.
#' - When available, net merchantable volume volume is constrained to be between
#'   95% and 100% of gross merchantable volume, matching the original OSM implementation
#' - By default, the function returns gross merchantable volume to remain
#'   consistent with other volume models in the package.
#'
#'
#' @param DBH Numeric vector. Diameter at breast height (cm).
#' @param height Numeric vector. Total tree height (m).
#' @param species Character vector of NFI species codes (e.g., `"PICE.MAR"`).
#' @param subregion Character or numeric vector. Either:
#'   - `"Province"` (default; uses province-wide equations), or
#'   - District IDs `1-24`.
#'   Only District `2` and `4-18` use district-level equations;
#'   all others fall back to province-wide equations.
#' @param keep_net Logical. If `TRUE`, additional columns
#'   `vol_merchantable_gross` and `vol_merchantable_net`
#'   are returned.
#'
#' @return A tibble with:
#'   - `vol_total`: Total stem volume inside bark (m³).
#'   - `vol_merchantable`: Gross merchantable volume inside bark (m³).
#'
#'   If `keep_net = TRUE`, also returns:
#'   - `vol_merchantable_gross`
#'   - `vol_merchantable_net`
#'
#' @details
#' Merchantable volume is computed between jurisdictional stump height
#' and top diameter limits obtained from `get_merch_criteria("NL")`.
#' Trees below the minimum DBH threshold return zero merchantable volume,
#' but total volume is still computed.
#'
#'
#' @examples
#' # Province-wide model (default)
#' vol_nl(20, 20, "PICE.MAR")
#'
#' # District-level model (NX-242 used for District 2)
#' vol_nl(20, 20, "PICE.MAR", subregion = 2)
#'
#' # Return net merchantable volume (if available)
#' vol_nl(20, 20, "PICE.MAR", subregion = 2, keep_net = TRUE)
#'
#' # Tidyverse workflow example
#' trees <- tibble::tibble(
#'   DBH = c(15, 20, 25, 30),
#'   height = c(15, 20, 21, 25),
#'   species = c("PICE.MAR", "ABIE.BAL","PICE.MAR", "BETU.PAP"),
#'   subregion = c("Province", 2, 17, "Province")
#' )
#'
#'trees |>
#'   dplyr::mutate(vol = vol_nl(DBH, height, species, subregion)) |>
#'   tidyr::unnest(vol)
#'
#' @references
#' Honer, T.G. (1967). Standard volume tables and merchantable conversion factors.
#' Canadian Forest Service Information Report N-X-67.
#'
#' Ker, M.F. 1974. Metric Tree Volume Tables for Newfoundland. Newfoundland Forest Research Centre, St. Johns, NF. Information Report N-X-122. https://ostrnrcan-dostrncan.canada.ca/handle/1845/238893
#'
#' Warren, G.R. & Meades, J.P. (1986). Wood defect and density studies II:
#' Total and net volume equations for Newfoundland’s forest management units.
#' Canadian Forest Service Information Report N-X-242.
#'
#' @export

vol_nl <- function(
  DBH,
  height,
  species,
  subregion = "Province",
  keep_net = FALSE
) {
  n <- length(DBH)

  # ---- recycle subregion if length 1 ----
  if (length(subregion) == 1L) {
    subregion <- rep(subregion, n)
  }

  if (length(height) != n || length(species) != n || length(subregion) != n) {
    rlang::abort(
      "DBH, height, species, and subregion must have the same length (or subregion length 1)."
    )
  }

  # Internal policy (from OSM NL implementation):
  # If net merchantable volume is available, constrain it to be no less than 95%
  # of gross merchantable volume (and no greater than gross). This guards against
  # known problematic net-volume equations in some NL parameter sets.
  constrain_net_volume <- TRUE

  species_std <- standardize_species_code(species)

  # Map default "Province" -> parameter key "ALL"
  sub_key <- as.character(subregion)
  sub_key <- dplyr::if_else(
    is.na(sub_key),
    NA_character_,
    stringr::str_squish(sub_key)
  )

  sub_key <- dplyr::case_when(
    is.na(sub_key) ~ NA_character_,
    stringr::str_to_upper(sub_key) %in%
      c("PROVINCE", "PROV", "NL", "ALL") ~ "ALL",
    TRUE ~ sub_key
  )

  # For non-ALL keys, normalize numeric districts like "19.0" -> "19"
  is_numeric_like <- !is.na(sub_key) &
    sub_key != "ALL" &
    stringr::str_detect(sub_key, "^[0-9]+(\\.[0-9]+)?$")

  sub_key[is_numeric_like] <-
    as.character(as.integer(as.numeric(sub_key[is_numeric_like])))

  # Validate user-supplied subregion:
  # Users can provide "Province" (-> "ALL") or district IDs 1–24.
  is_all <- !is.na(sub_key) & sub_key == "ALL"
  is_numeric <- !is.na(sub_key) &
    sub_key != "ALL" &
    stringr::str_detect(sub_key, "^[0-9]+$")

  district_num <- rep(NA_integer_, length(sub_key))
  district_num[is_numeric] <- as.integer(sub_key[is_numeric])

  is_district_1_24 <- is_numeric &
    district_num >= 1L &
    district_num <= 24L

  if (any(!is_all & !is_district_1_24, na.rm = TRUE)) {
    bad <- unique(sub_key[!is_all & !is_district_1_24])
    rlang::abort(paste0(
      "Invalid NL subregion value(s): ",
      paste(bad, collapse = ", "),
      ". Use 'Province' (or 'ALL') or district IDs 1-24."
    ))
  }

  # Mimic original C# behavior:
  # The district-level NX-242 equations are only used for District 2 and 4–18.
  # District 19 *exists* as a management district, but the OSM implementation
  # explicitly avoids using the NX-242 district parameters there because it
  # produced "very strange results" (see C# comment). Therefore:
  # - If user supplies District 19 (or any district not in {2,4–18}),
  #   we fall back to province-wide parameters (Subregion = "ALL").
  use_nx242_districts <- !is.na(sub_key) &
    sub_key %in% c("2", as.character(4:18))
  sub_key_model <- dplyr::if_else(is_all | use_nx242_districts, sub_key, "ALL")

  abort_i <- function(i, msg) {
    rlang::abort(paste0(
      "vol_nl() failed for row ",
      i,
      " (species=",
      species_std[i],
      ", subregion=",
      sub_key[i],
      ", DBH_cm=",
      DBH[i],
      ", ht_m=",
      height[i],
      "): ",
      msg
    ))
  }

  # ---- merch criteria (NL-wide, species = ALL) ----
  mc <- get_merch_criteria(jurisdiction = "NL", verbose = FALSE) |>
    dplyr::slice(1)

  need_mc <- c("stumpht_m", "topdbh_cm", "mindbh_cm")
  miss_mc <- setdiff(need_mc, names(mc))
  if (length(miss_mc) > 0) {
    rlang::abort(paste0(
      "get_merch_criteria('NL') missing columns: ",
      paste(miss_mc, collapse = ", ")
    ))
  }

  stumpHt_default <- mc$stumpht_m[[1]]
  topDIB_default <- mc$topdbh_cm[[1]]
  minDBH <- mc$mindbh_cm[[1]]

  if (!is.finite(stumpHt_default) || stumpHt_default < 0) {
    rlang::abort("Invalid stumpht_m from get_merch_criteria('NL').")
  }
  if (!is.finite(topDIB_default) || topDIB_default <= 0) {
    rlang::abort("Invalid topdbh_cm from get_merch_criteria('NL').")
  }
  if (!is.finite(minDBH) || minDBH < 0) {
    rlang::abort("Invalid mindbh_cm from get_merch_criteria('NL').")
  }

  # ---- cache params once per species x *model subregion* ----
  # Note: we use `sub_key_model` (with fallback-to-ALL applied) to match C# logic.
  key_tbl <- dplyr::tibble(.sp = species_std, .sub = sub_key_model) |>
    dplyr::distinct()

  params_cache <- key_tbl |>
    dplyr::mutate(
      p = purrr::map2(
        .data$.sp,
        .data$.sub,
        ~ get_volume_params(
          model_id = "regional_NL",
          species = .x,
          subregion = .y
        )
      )
    ) |>
    tidyr::unnest(p)

  need_p <- c(
    "param_set",
    "Species",
    "Subregion",
    "t",
    "a",
    "b",
    "c",
    "d",
    "e",
    "nv_a",
    "nv_b",
    "nv_c"
  )
  miss_p <- setdiff(need_p, names(params_cache))
  if (length(miss_p) > 0) {
    rlang::abort(paste0(
      "get_volume_params('regional_NL', ...) missing columns: ",
      paste(miss_p, collapse = ", ")
    ))
  }

  # ---- outputs ----
  vol_total <- numeric(n)
  vol_merch_gross <- numeric(n)
  vol_merch_net <- numeric(n)

  for (i in seq_len(n)) {
    dbh <- DBH[i]
    ht <- height[i]

    if (is.na(dbh) || is.na(ht) || !is.finite(dbh) || !is.finite(ht)) {
      abort_i(i, "DBH and height must be finite numeric values (not NA/Inf).")
    }
    if (dbh <= 0) {
      abort_i(i, "DBH must be > 0.")
    }
    if (ht <= 0) {
      abort_i(i, "height must be > 0.")
    }

    allow_merch <- (dbh >= minDBH)

    stumpHt <- stumpHt_default
    topDIB <- topDIB_default

    p <- params_cache |>
      dplyr::filter(
        .data$.sp == species_std[i],
        .data$.sub == sub_key_model[i]
      ) |>
      dplyr::slice(1)

    if (nrow(p) == 0) {
      abort_i(i, "No NL parameters returned for this species x subregion.")
    }

    use_district_formula <- p$param_set[[1]] %in%
      c("NX242_BF_DISTRICT", "NX242_BS_DISTRICT")

    tv <- NA_real_
    merch_gross <- NA_real_
    merch_net <- NA_real_

    if (use_district_formula) {
      # NX-242 (district-level; only used for District 2 and 4–18 in this R port,
      # mirroring the original OSM/C# logic).
      tv <- (dbh^2) / (p$a[[1]] + p$b[[1]] / ht)

      Y <- (topDIB / dbh)^2 * (1 + stumpHt / ht)
      merch_gross <- tv * (p$c[[1]] + p$d[[1]] * Y + p$e[[1]] * (Y * Y))

      if (is.finite(p$nv_a[[1]]) && p$nv_a[[1]] != -999) {
        merch_net <- tv *
          (p$nv_a[[1]] + p$nv_b[[1]] * Y + p$nv_c[[1]] * (Y * Y))
      } else {
        merch_net <- NA_real_
      }
    } else {
      # NX-122 total + NX-67 merch (province-wide/species)
      tcoef <- p$t[[1]]
      if (!is.finite(tcoef)) {
        abort_i(i, "Parameter 't' is not finite.")
      }

      denom <- (p$a[[1]] + (0.3048 * p$b[[1]] / ht))
      tv <- 0.00439 * dbh * dbh * (1 - 0.04365 * tcoef)^2 / denom

      W <- (topDIB / (dbh * (1 - 0.04365 * tcoef)))^2 * (1 + stumpHt / ht)
      merch_gross <- tv * (p$c[[1]] + p$d[[1]] * W + p$e[[1]] * (W * W))

      merch_net <- NA_real_ # not available for these species in C#
    }

    if (!is.finite(tv)) {
      abort_i(i, "Computed total volume is non-finite.")
    }
    if (!is.finite(merch_gross)) {
      abort_i(i, "Computed merchantable volume is non-finite.")
    }

    # Borderline negatives => 0
    if (merch_gross < 0) {
      merch_gross <- 0
    }
    # Merch cannot exceed total
    if (merch_gross > tv) {
      merch_gross <- tv
    }

    # Net handling (C# logic)
    if (!is.finite(merch_net)) {
      merch_net <- merch_gross
    } else {
      if (merch_net > merch_gross) {
        merch_net <- merch_gross
      }
      if (isTRUE(constrain_net_volume) && merch_net < merch_gross * 0.95) {
        merch_net <- merch_gross * 0.95
      }
    }

    # Minimum DBH affects merchantable only (not total)
    if (!allow_merch) {
      merch_gross <- 0
      merch_net <- 0
    }

    vol_total[i] <- tv
    vol_merch_gross[i] <- merch_gross
    vol_merch_net[i] <- merch_net
  }

  out <- dplyr::tibble(
    vol_total = vol_total,
    vol_merchantable = vol_merch_gross
  )

  if (isTRUE(keep_net)) {
    out <- dplyr::mutate(
      out,
      vol_merchantable_gross = vol_merch_gross,
      vol_merchantable_net = vol_merch_net
    )
  }

  out
}
