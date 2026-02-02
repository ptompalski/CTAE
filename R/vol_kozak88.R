# Models for AB (Huang), SK (Gal&Bella), MN (Klos et al) all use the same taper equation (Kozak88).
# The code below includes internal kozak88 that is then used in three separate functions for
# each jurisdictions.

# ------------------------------------------------------------------------------
# Internal Kozak (1988) variable-exponent volume engine
# ------------------------------------------------------------------------------

#' @keywords internal
vol_kozak88_engine <- function(
  DBH,
  height,
  species,
  subregion = "Province",
  jurisdiction,
  model_id,
  fallback_subregion = "Province",
  # optional: named character vector, e.g. c("UB" = "LBH")
  subregion_map = NULL
) {
  # ---- recycle scalars (common in usage) ----
  n <- max(length(DBH), length(height), length(species), length(subregion))
  if (
    length(DBH) %in%
      c(1L, n) &&
      length(height) %in% c(1L, n) &&
      length(species) %in% c(1L, n) &&
      length(subregion) %in% c(1L, n)
  ) {
    DBH <- rep(DBH, length.out = n)
    height <- rep(height, length.out = n)
    species <- rep(species, length.out = n)
    subregion <- rep(subregion, length.out = n)
  } else {
    rlang::abort(
      "DBH, height, species, and subregion must have compatible lengths (each length 1 or the same length)."
    )
  }

  # ---- standardize inputs ----
  jurisdiction_std <- standardize_jurisdiction_code(jurisdiction)
  species_std <- standardize_species_code(species)
  subr_std <- stringr::str_squish(stringr::str_to_upper(subregion))

  if (!is.null(subregion_map)) {
    # match on standardized subregions
    map_std <- stats::setNames(
      stringr::str_squish(stringr::str_to_upper(unname(subregion_map))),
      stringr::str_squish(stringr::str_to_upper(names(subregion_map)))
    )
    idx <- subr_std %in% names(map_std)
    subr_std[idx] <- unname(map_std[subr_std[idx]])
  }

  # ---- constants ----
  cons <- 0.00007854

  # ---- internal: structured abort with context ----
  abort_i <- function(i, msg) {
    rlang::abort(paste0(
      "vol_kozak88_engine() failed for row ",
      i,
      " (model_id=",
      model_id,
      ", jurisdiction=",
      jurisdiction_std,
      ", species=",
      species_std[i],
      ", subregion=",
      subr_std[i],
      ", DBH_cm=",
      DBH[i],
      ", ht_m=",
      height[i],
      "): ",
      msg
    ))
  }

  # ---- merchantability criteria (cache once; species-specific if supported) ----
  mc0 <- get_merch_criteria(jurisdiction_std, species = "ALL", verbose = FALSE)
  if (!is.null(mc0) && nrow(mc0) > 0) {
    mc_all <- mc0 |> dplyr::slice(1)
  } else {
    mc_all <- NULL
  }

  req_mc <- c("stumpht_m", "topdbh_cm", "mindbh_cm")

  get_mc_row <- function(sp) {
    mc <- get_merch_criteria(jurisdiction_std, species = sp, verbose = FALSE)
    if (!is.null(mc) && nrow(mc) > 0) {
      mc <- mc |> dplyr::slice(1)
    } else {
      mc <- NULL
    }

    if (is.null(mc)) {
      mc <- mc_all
    }

    if (is.null(mc)) {
      rlang::abort(paste0(
        "get_merch_criteria('",
        jurisdiction_std,
        "') returned no rows (species='",
        sp,
        "')."
      ))
    }

    miss_mc <- setdiff(req_mc, names(mc))
    if (length(miss_mc) > 0) {
      rlang::abort(paste0(
        "get_merch_criteria('",
        jurisdiction_std,
        "') missing columns: ",
        paste(miss_mc, collapse = ", ")
      ))
    }

    if (any(!is.finite(unlist(mc[req_mc])))) {
      rlang::abort(paste0(
        "get_merch_criteria('",
        jurisdiction_std,
        "') returned non-finite values in: ",
        paste(req_mc, collapse = ", ")
      ))
    }

    mc
  }

  # ---- internal: check wide params row (now supports optional p) ----
  check_params_wide <- function(p_w) {
    need <- c("a0", "a1", "a2", "b1", "b2", "b3", "b4", "b5")
    if (nrow(p_w) != 1) {
      return(NULL)
    }
    if (!all(need %in% names(p_w))) {
      return(NULL)
    }

    # p is optional in your tables; default to Kozak88 constant if absent
    p_val <- 0.225
    if ("p" %in% names(p_w)) {
      p_val <- p_w$p[[1]]
    }

    p <- list(
      a0 = p_w$a0[[1]],
      a1 = p_w$a1[[1]],
      a2 = p_w$a2[[1]],
      b1 = p_w$b1[[1]],
      b2 = p_w$b2[[1]],
      b3 = p_w$b3[[1]],
      b4 = p_w$b4[[1]],
      b5 = p_w$b5[[1]],
      p = p_val
    )

    if (any(!is.finite(unlist(p)))) {
      return(NULL)
    }
    if (p$p <= 0 || p$p >= 1) {
      return(NULL)
    }
    p
  }

  # ---- internal: DIB at height h (m), inside bark (cm) ----
  kozak88_dib <- function(h, DBH, HT, p) {
    if (!is.finite(h) || !is.finite(DBH) || !is.finite(HT)) {
      return(NA_real_)
    }
    if (HT <= 0) {
      return(NA_real_)
    }

    z <- h / HT
    if (!is.finite(z) || z <= 0) {
      return(NA_real_)
    }
    if (z > 1) {
      z <- 1
    }

    per <- (1.0 - sqrt(p$p))
    if (!is.finite(per) || per <= 0) {
      return(NA_real_)
    }

    x <- (1 - sqrt(z)) / per
    if (!is.finite(x) || x < 0) {
      x <- 0
    }

    cc <- p$b1 *
      (z^2) +
      p$b2 * log(z + 0.001) +
      p$b3 * sqrt(z) +
      p$b4 * exp(z) +
      p$b5 * (DBH / HT)

    ff <- p$a0 * (DBH^p$a1) * (p$a2^DBH)
    if (!is.finite(ff) || ff <= 0) {
      return(NA_real_)
    }

    dib <- ff * (x^cc)
    if (!is.finite(dib)) {
      return(NA_real_)
    }
    dib
  }

  # ---- internal: colleague-compatible solver for g (with reset-to-0.9 behavior) ----
  solve_g_colleague <- function(DBH, HT, p, topdbh) {
    g0 <- 0.9
    g1 <- 0
    maxiter <- 500
    tol <- 1e-9
    per <- (1.0 - sqrt(p$p))

    for (iii in seq_len(maxiter)) {
      cee <- p$b1 *
        (g0^2) +
        p$b2 * log(g0 + 0.001) +
        p$b3 * sqrt(g0) +
        p$b4 * exp(g0) +
        p$b5 * (DBH / HT)

      ff <- p$a0 * (DBH^p$a1) * (p$a2^DBH)

      # If things go odd, break and reset later
      if (!is.finite(cee) || cee == 0 || !is.finite(ff) || ff <= 0) {
        g0 <- NA_real_
        break
      }

      g1 <- (1 - ((topdbh / ff)^(1 / cee)) * per)^2
      g0 <- (g0 + g1) / 2

      if (is.nan(g0) || is.infinite(g0)) {
        break
      }
      if (abs(g0 - g1) < tol) break
    }

    # Reset behavior exactly like colleague
    if (!is.finite(g0) || is.nan(g0) || is.infinite(g0) || g0 > 1) {
      g0 <- 0.9
    }
    if (g0 < 0) {
      g0 <- 0.9
    }

    g0
  }

  # ---- internal: Simpson integration (20 sub-sections => 10 Simpson segments) ----
  vol_integrate_simpson <- function(stumpht, upper_h, dbh, HT, p) {
    if (upper_h <= stumpht) {
      return(0)
    }

    mlen <- (1:20) * (upper_h - stumpht) / 20 + stumpht

    dib0 <- kozak88_dib(stumpht, dbh, HT, p)
    if (!is.finite(dib0) || dib0 < 0) {
      return(NA_real_)
    }

    dibx <- vapply(mlen, kozak88_dib, numeric(1), DBH = dbh, HT = HT, p = p)
    dibm <- c(dib0, dibx)

    if (length(dibm) != 21) {
      return(NA_real_)
    }
    if (any(!is.finite(dibm)) || any(dibm < 0)) {
      return(NA_real_)
    }

    k <- cons * (((upper_h - stumpht) / 10) / 6)

    v <- 0
    for (seg in 1:10) {
      idx0 <- (seg - 1) * 2 + 1
      idxm <- idx0 + 1
      idx1 <- idx0 + 2
      v <- v + k * (dibm[idx0]^2 + 4 * dibm[idxm]^2 + dibm[idx1]^2)
    }
    v
  }

  # ---- outputs ----
  vol_total <- numeric(n)
  vol_merch <- numeric(n)

  for (i in seq_len(n)) {
    dbh <- DBH[i]
    HT <- height[i]

    if (is.na(dbh) || is.na(HT) || !is.finite(dbh) || !is.finite(HT)) {
      abort_i(i, "DBH and height must be finite numeric values (not NA/Inf).")
    }
    if (dbh <= 0) {
      abort_i(i, "DBH must be > 0.")
    }
    if (HT <= 0) {
      abort_i(i, "height must be > 0.")
    }
    if (HT < 1.3) {
      HT <- 1.3
    }

    # merch criteria (can be species-specific)
    mc <- get_mc_row(species_std[i])
    stumpht <- mc$stumpht_m[[1]]
    topdbh <- mc$topdbh_cm[[1]]
    mindbh <- mc$mindbh_cm[[1]]

    # ---- parameters: try requested subregion; fallback ----
    subr_req <- subr_std[i]
    subr_used <- subr_req

    p_w <- get_volume_params(
      model_id = model_id,
      species = species_std[i],
      subregion = subr_used
    )

    if (nrow(p_w) == 0) {
      if (!identical(subr_req, stringr::str_to_upper(fallback_subregion))) {
        rlang::warn(paste0(
          "vol_kozak88_engine(): no parameters for (model_id=",
          model_id,
          ", species=",
          species_std[i],
          ", subregion=",
          subr_req,
          "); using ",
          fallback_subregion,
          " parameters instead (row ",
          i,
          ")."
        ))
      }

      subr_used <- fallback_subregion
      p_w <- get_volume_params(
        model_id = model_id,
        species = species_std[i],
        subregion = subr_used
      )
    }

    if (nrow(p_w) == 0) {
      abort_i(
        i,
        paste0(
          "No parameters found (neither subregion='",
          subr_req,
          "' nor fallback_subregion='",
          fallback_subregion,
          "')."
        )
      )
    }

    p <- check_params_wide(p_w)
    if (is.null(p)) {
      abort_i(
        i,
        "Returned parameter row is missing required coefficients (a0,a1,a2,b1..b5[,p]) or contains NA/Inf."
      )
    }

    # ---- stump volume (same as colleague) ----
    dib_stump <- kozak88_dib(stumpht, dbh, HT, p)
    if (!is.finite(dib_stump) || dib_stump < 0) {
      abort_i(i, "Failed computing DIB at stump height.")
    }
    stpvol <- cons * dib_stump^2 * stumpht
    if (!is.finite(stpvol) || stpvol < 0) {
      abort_i(i, "Computed stump volume is invalid.")
    }

    # ---- colleague logic: compute hi via solver (with reset behavior), then totvol = section + tip + stump ----
    g0 <- solve_g_colleague(dbh, HT, p, topdbh)
    hi <- g0 * HT

    # guard: if hi ends up below stump, clamp (prevents negative integration lengths)
    if (!is.finite(hi)) {
      hi <- 0.9 * HT
    }
    if (hi < stumpht) {
      hi <- stumpht
    }

    # section volume from stump -> hi (this is what colleague calls mvol before mindbh filter)
    section_vol <- vol_integrate_simpson(stumpht, hi, dbh, HT, p)
    if (!is.finite(section_vol) || section_vol < 0) {
      section_vol <- 0
    }

    # tip volume cone uses DIB at hi (colleague uses dibm[[20]] which is DIB at hi)
    dib_hi <- kozak88_dib(hi, dbh, HT, p)
    if (!is.finite(dib_hi) || dib_hi < 0) {
      dib_hi <- 0
    }
    tipvol <- cons * dib_hi^2 * (HT - hi) / 3
    if (!is.finite(tipvol) || tipvol < 0) {
      tipvol <- 0
    }

    totvol <- section_vol + tipvol + stpvol
    if (!is.finite(totvol) || totvol < 0) {
      totvol <- 0
    }

    # ---- merchantable volume output (match colleague): 0 if dbh < mindbh ----
    mvol_out <- if (dbh < mindbh) 0 else section_vol
    if (!is.finite(mvol_out) || mvol_out < 0) {
      mvol_out <- 0
    }

    vol_total[i] <- totvol
    vol_merch[i] <- mvol_out
  }

  dplyr::tibble(
    vol_total = vol_total,
    vol_merchantable = vol_merch
  )
}


# ------------------------------------------------------------------------------
# Exported wrappers (AB / MB / SK) that reuse the same Kozak88 engine
# ------------------------------------------------------------------------------

#' Estimate tree volume in Alberta using Kozak (1988) model form with Huang (1994) parameters
#'
#' @param DBH Numeric vector. Diameter at breast height (cm).
#' @param height Numeric vector. Total height (m).
#' @param species Character vector. Species code (e.g., "PICE.MAR").
#' @param subregion Character vector. Alberta subregion name; defaults to "Province".
#'
#' @return A tibble with volumes (m^3): total and merchantable.
#'
#' @references
#' Huang, S. (1994). Ecologically Based Individual Tree Volume Estimation for Major Alberta Tree Species. Report 1 - Individual tree volume estimation procedures for Alberta: Methods of Formulation and Statistical Foundations. Alberta Environmental Protection, Land and Forest Service, Forest Management Division, Edmonton, AB.
#'
#' Kozak, A. (1988). A variable-exponent taper equation. \emph{Canadian Journal of Forest Research}, 18, 1363–1368.
#'
#' @export
vol_huang94 <- function(DBH, height, species, subregion = "Province") {
  # Huang 1994 note: UB treated as LBH
  vol_kozak88_engine(
    DBH = DBH,
    height = height,
    species = species,
    subregion = subregion,
    jurisdiction = "AB",
    model_id = "regional_huang94",
    fallback_subregion = "Province",
    subregion_map = c("UB" = "LBH")
  )
}

#' Estimate tree volume in Manitoba using Kozak (1988) form with Klos (2004) parameters
#'
#' @inheritParams vol_huang94
#' @param subregion Optional Ecozone or Ecoregion identifier
#'
#' @references
#' Klos, R. J., Wang, G. G., Dang, Q.-L., & East, E. W. (2007).
#' \emph{Taper equations for five major commercial tree species in Manitoba, Canada}.
#' Western Journal of Applied Forestry, 22(3), 163–170.
#'
#' Kozak, A. (1988). A variable-exponent taper equation. \emph{Canadian Journal of Forest Research}, 18, 1363–1368.
#'
#' @export
vol_klos2007 <- function(DBH, height, species, subregion = "Province") {
  vol_kozak88_engine(
    DBH = DBH,
    height = height,
    species = species,
    subregion = subregion,
    jurisdiction = "MB",
    model_id = "regional_klos2007",
    fallback_subregion = "Province"
  )
}

#' Estimate tree volume in Saskatchewan using Kozak (1988) form with Gal & Bella (1994) parameters
#'
#' @inheritParams vol_huang94
#'
#'
#' @references
#' Gal, J., & Bella, I.E. (1994). \emph{New stem taper functions for 12 Saskatchewan timber species}. Natural Resources Canada, Canadian Forest Service, Northwest Region, Information Report NOR-X-338. Table 5.

#' Kozak, A. (1988). A variable-exponent taper equation.\emph{Canadian Journal of Forest Research}, 18, 1363–1368.
#'
#' @export
vol_galbella94 <- function(DBH, height, species) {
  vol_kozak88_engine(
    DBH = DBH,
    height = height,
    species = species,
    subregion = "Province",
    jurisdiction = "SK",
    model_id = "regional_galbella94",
    fallback_subregion = "Province"
  )
}
