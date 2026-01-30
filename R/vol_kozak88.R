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
  jurisdiction_std <- CTAE:::standardize_jurisdiction_code(jurisdiction)
  species_std <- CTAE:::standardize_species_code(species)
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

  # ---- constants (as in your legacy implementation) ----
  cons <- 0.00007854
  per <- (1.0 - sqrt(0.225))

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
  # If get_merch_criteria() supports species-specific rules for that jurisdiction,
  # passing species_std here will pick them up.
  mc0 <- get_merch_criteria(jurisdiction_std, species = "ALL", verbose = FALSE)
  if (!is.null(mc0) && nrow(mc0) > 0) {
    # keep it for fallback if species-specific not present
    mc_all <- mc0 %>% dplyr::slice(1)
  } else {
    mc_all <- NULL
  }

  req_mc <- c("stumpht_m", "topdbh_cm", "mindbh_cm")

  get_mc_row <- function(sp) {
    mc <- get_merch_criteria(jurisdiction_std, species = sp, verbose = FALSE)
    if (!is.null(mc) && nrow(mc) > 0) {
      mc <- mc %>% dplyr::slice(1)
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

  # ---- internal: check wide params row ----
  check_params_wide <- function(p_w) {
    need <- c("a0", "a1", "a2", "b1", "b2", "b3", "b4", "b5")
    if (nrow(p_w) != 1) {
      return(NULL)
    }
    if (!all(need %in% names(p_w))) {
      return(NULL)
    }

    p <- list(
      a0 = p_w$a0[[1]],
      a1 = p_w$a1[[1]],
      a2 = p_w$a2[[1]],
      b1 = p_w$b1[[1]],
      b2 = p_w$b2[[1]],
      b3 = p_w$b3[[1]],
      b4 = p_w$b4[[1]],
      b5 = p_w$b5[[1]]
    )
    if (any(!is.finite(unlist(p)))) {
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

  # ---- internal: solve g (relative merch height) such that DIB == topdbh ----
  solve_g <- function(DBH, HT, p, topdbh) {
    g0 <- 0.9
    g1 <- 0
    maxiter <- 1000
    tol <- 1e-8
    iii <- 0

    while (abs(g0 - g1) > tol && iii <= maxiter) {
      cc <- p$b1 *
        (g0^2) +
        p$b2 * log(g0 + 0.001) +
        p$b3 * sqrt(g0) +
        p$b4 * exp(g0) +
        p$b5 * (DBH / HT)

      ff <- p$a0 * (DBH^p$a1) * (p$a2^DBH)
      if (!is.finite(cc) || cc == 0 || !is.finite(ff) || ff <= 0) {
        return(NA_real_)
      }

      g1 <- (1 - ((topdbh / ff)^(1 / cc)) * per)^2
      if (!is.finite(g1)) {
        return(NA_real_)
      }

      g0 <- (g0 + g1) / 2
      g0 <- max(min(g0, 1), 0)
      iii <- iii + 1
    }

    if (iii > maxiter) {
      return(NA_real_)
    }
    g0
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

    if (dbh < mindbh) {
      vol_total[i] <- 0
      vol_merch[i] <- 0
      next
    }

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
        "Returned parameter row is missing required coefficients (a0,a1,a2,b1..b5) or contains NA/Inf."
      )
    }

    # ---- solve merchantable relative height g and merchantable height hi ----
    g <- solve_g(dbh, HT, p, topdbh)
    if (!is.finite(g)) {
      abort_i(
        i,
        "Merchantable height solver failed to converge or produced non-finite value."
      )
    }

    hi <- g * HT
    if (!is.finite(hi)) {
      abort_i(i, "Computed merchantable height is non-finite.")
    }

    # if merch height <= stump => merch vol = 0, total = stump + tip
    if (hi <= stumpht) {
      dib_stump <- kozak88_dib(stumpht, dbh, HT, p)
      if (!is.finite(dib_stump) || dib_stump < 0) {
        abort_i(i, "Failed computing DIB at stump height.")
      }

      volstp <- cons * dib_stump^2 * stumpht
      if (!is.finite(volstp) || volstp < 0) {
        abort_i(i, "Computed stump volume is invalid.")
      }

      tipvol <- cons * (topdbh^2) * (HT - hi) / 3
      if (!is.finite(tipvol) || tipvol < 0) {
        abort_i(i, "Computed tip volume is invalid.")
      }

      vol_merch[i] <- 0
      vol_total[i] <- volstp + tipvol
      next
    }

    # ---- Newton integration points (20 sub-sections => 21 diameters) ----
    mlen <- (1:20) * (hi - stumpht) / 20 + stumpht
    z <- mlen / HT
    x <- (1 - sqrt(z)) / per

    dib_stump <- kozak88_dib(stumpht, dbh, HT, p)
    if (!is.finite(dib_stump) || dib_stump < 0) {
      abort_i(i, "Failed computing DIB at stump height.")
    }

    ff <- p$a0 * (dbh^p$a1) * (p$a2^dbh)
    if (!is.finite(ff) || ff <= 0) {
      abort_i(i, "Form factor is non-finite or <= 0 (check a0/a1/a2).")
    }

    cc_vec <- p$b1 *
      (z^2) +
      p$b2 * log(z + 0.001) +
      p$b3 * sqrt(z) +
      p$b4 * exp(z) +
      p$b5 * (dbh / HT)

    dibx <- ff * (x^cc_vec)
    dibm <- c(dib_stump, dibx)

    if (length(dibm) != 21) {
      abort_i(
        i,
        "Internal error: expected 21 diameters for Newton integration."
      )
    }
    if (any(!is.finite(dibm)) || any(dibm < 0)) {
      abort_i(i, "One or more predicted DIB values are non-finite/negative.")
    }

    # ---- Newton's formula for merchantable volume to topdbh ----
    k <- cons * (((hi - stumpht) / 10) / 6)

    mvol <- 0
    for (seg in 1:10) {
      idx0 <- (seg - 1) * 2 + 1
      idxm <- idx0 + 1
      idx1 <- idx0 + 2
      mvol <- mvol + k * (dibm[idx0]^2 + 4 * dibm[idxm]^2 + dibm[idx1]^2)
    }
    if (!is.finite(mvol) || mvol < 0) {
      abort_i(i, "Computed merchantable volume is non-finite/negative.")
    }

    volstp <- cons * dibm[1]^2 * stumpht
    if (!is.finite(volstp) || volstp < 0) {
      abort_i(i, "Computed stump volume is non-finite/negative.")
    }

    tipvol <- cons * (topdbh^2) * (HT - hi) / 3
    if (!is.finite(tipvol) || tipvol < 0) {
      abort_i(i, "Computed tip volume is non-finite/negative.")
    }

    vol_merch[i] <- mvol
    vol_total[i] <- mvol + tipvol + volstp
    if (!is.finite(vol_total[i]) || vol_total[i] < 0) {
      abort_i(i, "Computed total volume is non-finite/negative.")
    }
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
    model_id = "regional_klos2004",
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
