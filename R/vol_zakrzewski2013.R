#' Estimate tree volume using Zakrzewski (2013) taper model (Ontario)
#'
#'
#' @param DBH Numeric vector of diameter at breast height (cm).
#' @param height Numeric vector of total tree height (m).
#' @param species Character vector of species codes
#'
#' @return A tibble with volumes (m^3): total and merchantable.
#'
#' @note
#' Implementation is based on an original R script provided by
#' Margaret Penner, refactored for CTAE
#' (vectorized inputs, standardized species codes, and merchantability
#' criteria via \code{\link{get_merch_criteria}}).
#'
#' @source
#' Zakrzewski, W. T., Penner, M. (2013). \emph{A Comparison of Tree Stem Taper Models
#' for Use in Ontario}. Ontario Forest Research Institute, Report 176.
#'
#'
#' @examples
#' # Single tree
#' vol_zakrzewski2013(
#'   DBH = 20,
#'   height = 22,
#'   species = "PICE.MAR"
#' )
#'
#' # Vectorized input (multiple trees)
#' vol_zakrzewski2013(
#'   DBH = c(20, 30, 40),
#'   height = c(18, 22, 28),
#'   species = c("PICE.MAR", "PINU.STR", "THUJ.OCC")
#' )
#'
#' @export
vol_zakrzewski2013 <- function(DBH, height, species) {
  n <- length(DBH)
  if (length(height) != n || length(species) != n) {
    rlang::abort("DBH, height, and species must have the same length.")
  }

  # Standardize to CTAE/NFI species codes (e.g., "PICE.MAR")
  species_std <- standardize_species_code(species)

  # ---- constants ----
  BH <- 1.3
  cons <- 0.00007854

  # ---- internal: structured abort with context ----
  abort_i <- function(i, msg) {
    rlang::abort(paste0(
      "vol_zakrzewski2013() failed for row ",
      i,
      " (species=",
      species_std[i],
      ", DBH_cm=",
      DBH[i],
      ", ht_m=",
      height[i],
      "): ",
      msg
    ))
  }

  # ---- internal: identify deciduous vs conifer from parameter availability ----
  zak_is_deciduous_chi <- function(chi) {
    is.finite(chi) && chi > 0
  }

  # ---- internal: compute s(H/DBH) using Table 1 mapping (conifers) ----
  zak_compute_s_conifer_1 <- function(HDR, rho, species_nfi) {
    if (!is.finite(HDR) || HDR <= 0) {
      return(NA_real_)
    }
    if (!is.finite(rho)) {
      return(NA_real_)
    }

    s_form <- dplyr::case_when(
      species_nfi %in%
        c("ABIE.BAL", "PINU.BAN", "PINU.RES", "PICE.MAR") ~ "2 + rho*log(HDR)",
      species_nfi %in% c("THUJ.OCC") ~ "1 + rho*HDR",
      species_nfi %in% c("PINU.STR") ~ "1 + rho*exp(HDR)",
      species_nfi %in% c("PICE.GLA") ~ "1 + HDR^rho",
      TRUE ~ NA_character_
    )

    if (is.na(s_form)) {
      s <- 1 + HDR^1.31
    } else if (s_form == "2 + rho*log(HDR)") {
      s <- 2 + rho * log(HDR)
    } else if (s_form == "1 + rho*HDR") {
      s <- 1 + rho * HDR
    } else if (s_form == "1 + rho*exp(HDR)") {
      s <- 1 + rho * exp(HDR)
    } else if (s_form == "1 + HDR^rho") {
      s <- 1 + HDR^rho
    } else {
      s <- NA_real_
    }

    if (is.finite(s) && s > 3) {
      s <- 2
    }
    if (is.finite(s) && s < 1) {
      s <- 1.2
    }

    s
  }

  # ---- internal: bark fraction (scalar) ----
  zak_bark_fraction_1 <- function(H, delta, nu, chi) {
    if (zak_is_deciduous_chi(chi)) {
      barkf <- 1 - exp(1 - (H / BH)^chi)
    } else {
      barkf <- delta + nu * (BH / H)
    }

    if (is.finite(barkf) && barkf > 1) {
      barkf <- 0.984
    }
    barkf
  }

  # ---- internal: section volume (scalar) from B to U (m) using closed-form TT * K ----
  zak_section_volume_1 <- function(H, B, U, a2, B2, G2, dib_cm) {
    U2 <- if (U > H) H else U
    B2m <- B

    H2 <- H * H
    H3 <- H2 * H
    H4 <- H3 * H

    z0 <- 1 - BH / H
    y11 <- (z0 - a2)
    y22 <- (z0^2 + B2 * z0^3 + G2 * z0^4)
    Bl2 <- y11 / y22
    K <- cons * dib_cm^2 * Bl2

    TT <- {
      (-1 / 12) *
        (12 *
          H4 *
          a2^3 *
          log(-H + U2 + a2 * H) *
          B2 +
          3 * G2 * U2^4 +
          6 * H^2 * G2 * a2^2 * U2^2 -
          12 * H * G2 * U2^3 -
          12 * H3 * G2 * a2 * U2 -
          4 * B2 * H * U2^3 -
          12 * H3 * a2 * B2 * U2 +
          12 * B2 * H^2 * U2^2 -
          12 * H3 * a2^2 * G2 * U2 +
          18 * G2 * H^2 * U2^2 -
          12 * H3 * a2^2 * B2 * U2 +
          12 * H4 * a2^4 * log(-H + U2 + a2 * H) * G2 -
          12 * H3 * a2^3 * G2 * U2 -
          4 * H * G2 * a2 * U2^3 -
          12 * H3 * G2 * U2 +
          6 * H2 * B2 * a2 * U2^2 -
          12 * H3 * a2 * U2 +
          12 * H4 * a2^2 * log(-H + U2 + a2 * H) -
          12 * U2 * H3 -
          12 * H3 * B2 * U2 +
          12 * H2 * G2 * a2 * U2^2 +
          6 * H2 * U2^2) /
        H3 +
        (1 / 12) *
          (-12 *
            H3 *
            B2 *
            B2m -
            4 * H * G2 * a2 * B2m^3 +
            12 * H4 * a2^3 * log(-H + B2m + a2 * H) * B2 -
            12 * H3 * a2 * B2 * B2m -
            12 * H3 * a2^2 * G2 * B2m -
            12 * H3 * a2^2 * B2 * B2m -
            12 * H3 * a2^3 * G2 * B2m -
            12 * H3 * G2 * a2 * B2m -
            12 * H * G2 * B2m^3 -
            4 * B2 * H * B2m^3 +
            12 * H2 * G2 * a2 * B2m^2 +
            18 * G2 * H2 * B2m^2 +
            6 * H2 * B2 * a2 * B2m^2 +
            12 * H4 * a2^4 * log(-H + B2m + a2 * H) * G2 +
            6 * H2 * G2 * a2^2 * B2m^2 -
            12 * H3 * a2 * B2m +
            6 * H2 * B2m^2 +
            12 * H^4 * a2^2 * log(-H + B2m + a2 * H) +
            12 * B2 * H2 * B2m^2 +
            3 * G2 * B2m^4 -
            12 * B2m * H3 -
            12 * H3 * G2 * B2m) /
          H^3
    }

    TT * K
  }

  # ---- internal: merchantable height solver for a top diameter (scalar) ----
  zak_merch_height_1 <- function(H, topdbh_cm, a2, B2, G2, dib_cm) {
    ca_h <- cons * topdbh_cm^2
    z0 <- 1 - BH / H
    K <- ((z0 - a2) * cons * dib_cm^2) / (z0^2 + B2 * z0^3 + G2 * z0^4)

    t1 <- 1 / G2
    t3 <- sqrt(3.0)
    t5 <- B2 * B2
    t6 <- t5 * K
    t7 <- K * ca_h
    t9 <- a2 * G2
    t11 <- ca_h * ca_h
    t13 <- a2 * t5
    t15 <- K * K
    t17 <- G2 * G2
    t21 <- K * t11
    t22 <- t5 * B2
    t26 <- ca_h * B2
    t30 <- a2 * a2
    t35 <- a2 * t15 * K
    t43 <- t17 * G2
    t48 <- t15 * ca_h
    t56 <- t5 * t5

    t60 <- 27.0 *
      t11 *
      ca_h *
      t17 -
      ca_h * t5 * t15 -
      4.0 * t21 * t22 +
      4.0 * ca_h * G2 * t15 -
      80 * t26 * t15 * a2 * G2 +
      128 * ca_h * t30 * t17 * t15 -
      16.0 * t35 * G2 +
      6 * t21 * t13 * G2 -
      192 * t21 * B2 * t30 * t17 -
      256 * t21 * t30 * a2 * t43 +
      18 * t21 * B2 * G2 +
      18 * t48 * t22 * a2 -
      144 * t21 * a2 * t17 -
      144 * t48 * t30 * G2 * t5 +
      27 * t48 * t30 * t56 +
      4 * t35 * t5

    if (!is.finite(t60) || t60 < 0.001) {
      t60 <- 0.001
    }

    t62 <- sqrt(ca_h * t60)
    t69 <- ((9 *
      t7 *
      B2 -
      72 * t7 * t9 +
      27 * t11 * G2 +
      27 * t7 * t13 +
      2 * t15 +
      3 * t3 * t62) /
      t15 /
      t43)^(1.0 / 3.0)

    t71 <- K * G2
    t73 <- (54)^(1 / 3)
    t74 <- t73 * t73
    t75 <- t69 * t69
    t79 <- t73 * ca_h
    t84 <- 1 / K
    t86 <- 1 / t69

    t88 <- sqrt(
      (27 *
        t6 *
        t69 -
        72 * t71 * t69 +
        2 * t74 * t75 * K * t17 +
        36 * t79 * B2 +
        144 * t79 * t9 +
        12 * t73 * K) *
        t84 *
        t86
    )

    t90 <- sqrt(6)
    t92 <- t69 * t88
    t99 <- t88 * t73
    t105 <- t3 * t69

    t118 <- sqrt(
      (27 *
        t6 *
        t92 -
        72 * t71 * t92 -
        t88 * t74 * t75 * K * t17 -
        18 * t99 * t26 -
        72 * t99 * ca_h * a2 * G2 -
        6 * t99 * K -
        324 * t105 * B2 * K * G2 -
        648 * t105 * ca_h * t17 +
        81 * t105 * t22 * K) *
        t84 *
        t86 /
        t88
    )

    r <- -B2 * t1 / 4 - t3 * t1 * t88 / 36 + t90 * t1 * t118 / 36
    H * (1 - r)
  }

  # ---- cache merch criteria once per species ----
  mc_cache <- purrr::map_dfr(
    unique(species_std),
    ~ get_merch_criteria("ON", .x) |>
      dplyr::slice(1) |>
      dplyr::mutate(.species_key = .x)
  )

  req_mc2 <- c(".species_key", "stumpht_m", "topdbh_cm", "mindbh_cm")
  miss_mc <- setdiff(req_mc2, names(mc_cache))
  if (length(miss_mc) > 0) {
    rlang::abort(paste0(
      "get_merch_criteria() missing columns: ",
      paste(miss_mc, collapse = ", ")
    ))
  }

  mc_idx <- match(species_std, mc_cache$.species_key)
  if (anyNA(mc_idx)) {
    i_bad <- which(is.na(mc_idx))[1]
    abort_i(i_bad, "No merchantability criteria found for this species in ON.")
  }

  stumpht_vec <- mc_cache$stumpht_m[mc_idx]
  topdbh_vec <- mc_cache$topdbh_cm[mc_idx]
  mindbh_vec <- mc_cache$mindbh_cm[mc_idx]

  # ---- cache zakrzewski parameters once per species ----
  param_cache <- purrr::map_dfr(
    unique(species_std),
    ~ get_volume_params(
      model_id = "regional_zakrzewski2013",
      species = .x
    ) |>
      dplyr::slice(1) |>
      dplyr::mutate(.species_key = .x)
  )

  need_p <- c(".species_key", "delta", "nu", "rho", "beta", "gamma", "chi")
  miss_p <- setdiff(need_p, names(param_cache))
  if (length(miss_p) > 0) {
    rlang::abort(paste0(
      "get_volume_params() missing columns for Zakrzewski2013: ",
      paste(miss_p, collapse = ", ")
    ))
  }

  p_idx <- match(species_std, param_cache$.species_key)
  if (anyNA(p_idx)) {
    i_bad <- which(is.na(p_idx))[1]
    abort_i(i_bad, "No Zakrzewski2013 parameters found for this species.")
  }

  delta_vec <- param_cache$delta[p_idx]
  nu_vec <- param_cache$nu[p_idx]
  rho_vec <- param_cache$rho[p_idx]
  beta_vec <- param_cache$beta[p_idx]
  gamma_vec <- param_cache$gamma[p_idx]
  chi_vec <- param_cache$chi[p_idx]

  # ---- outputs ----
  vol_total <- numeric(n)
  vol_merch <- numeric(n)

  # ---- row-wise evaluation ----
  for (i in seq_len(n)) {
    dbh <- DBH[i]
    H <- height[i]

    # ---- basic input validation ----
    if (is.na(dbh) || is.na(H) || !is.finite(dbh) || !is.finite(H)) {
      abort_i(i, "DBH and height must be finite numeric values (not NA/Inf).")
    }
    if (dbh <= 0) {
      abort_i(i, "DBH must be > 0.")
    }
    if (H <= 0) {
      abort_i(i, "height must be > 0.")
    }
    if (H < BH) {
      H <- BH
    }

    # ---- merchantability criteria ----
    stumpht <- stumpht_vec[i]
    topdbh <- topdbh_vec[i]
    mindbh <- mindbh_vec[i]

    if (!is.finite(stumpht) || stumpht < 0) {
      abort_i(i, "Invalid stumpht_m in merch criteria.")
    }
    if (!is.finite(topdbh) || topdbh <= 0) {
      abort_i(i, "Invalid topdbh_cm in merch criteria.")
    }
    if (!is.finite(mindbh) || mindbh < 0) {
      abort_i(i, "Invalid mindbh_cm in merch criteria.")
    }

    # ---- model parameters ----
    delta <- delta_vec[i]
    nu <- nu_vec[i]
    rho <- rho_vec[i]
    beta <- beta_vec[i]
    gamma <- gamma_vec[i]
    chi <- chi_vec[i]

    if (!is.finite(delta)) {
      abort_i(i, "Parameter 'delta' is not finite (NA/Inf).")
    }
    if (!is.finite(nu)) {
      abort_i(i, "Parameter 'nu' is not finite (NA/Inf).")
    }
    if (!is.finite(rho)) {
      abort_i(i, "Parameter 'rho' is not finite (NA/Inf).")
    }
    if (!is.finite(beta)) {
      abort_i(i, "Parameter 'beta' is not finite (NA/Inf).")
    }
    if (!is.finite(gamma)) {
      abort_i(i, "Parameter 'gamma' is not finite (NA/Inf).")
    }
    if (!is.finite(chi)) {
      abort_i(i, "Parameter 'chi' is not finite (NA/Inf).")
    }

    if (gamma == 0) {
      abort_i(
        i,
        "gamma must be non-zero (division by gamma occurs in merch-height solver)."
      )
    }

    # ---- compute HDR and s ----
    HDR <- H / dbh
    if (!is.finite(HDR) || HDR <= 0) {
      abort_i(i, "Invalid HDR (H/DBH).")
    }

    if (zak_is_deciduous_chi(chi)) {
      s <- 2
    } else {
      s <- zak_compute_s_conifer_1(
        HDR = HDR,
        rho = rho,
        species_nfi = species_std[i]
      )
    }
    if (!is.finite(s) || s <= 0) {
      abort_i(i, "Computed 's' is invalid (non-finite/<=0).")
    }

    # ---- compute bark fraction and DIB at breast height ----
    barkf <- zak_bark_fraction_1(H = H, delta = delta, nu = nu, chi = chi)
    if (!is.finite(barkf) || barkf <= 0) {
      abort_i(i, "Computed bark fraction is invalid (non-finite/<=0).")
    }

    dib <- dbh * barkf
    if (!is.finite(dib) || dib <= 0) {
      abort_i(i, "Computed DIB at BH is invalid (non-finite/<=0).")
    }

    # ---- gross total volume (0..H) ----
    v_total <- zak_section_volume_1(
      H = H,
      B = 0,
      U = H,
      a2 = s,
      B2 = beta,
      G2 = gamma,
      dib_cm = dib
    )
    if (!is.finite(v_total) || v_total < 0) {
      abort_i(i, "Total volume computation produced non-finite/negative value.")
    }

    # ---- gross merchantable volume (stump..merchht) ----
    # mindbh is a merchantability rule: it should NOT affect total volume.
    v_merch <- 0

    if (dbh >= mindbh) {
      if (dib > topdbh && H > 2) {
        merchht <- zak_merch_height_1(
          H = H,
          topdbh_cm = topdbh,
          a2 = s,
          B2 = beta,
          G2 = gamma,
          dib_cm = dib
        )

        if (!is.finite(merchht)) {
          abort_i(i, "Merchantable height solver returned non-finite.")
        }
        if (merchht < 0) {
          abort_i(i, "Merchantable height is negative.")
        }
        if (merchht > H) {
          merchht <- H
        }

        if (merchht < stumpht) {
          v_merch <- 0
        } else {
          v_merch <- zak_section_volume_1(
            H = H,
            B = stumpht,
            U = merchht,
            a2 = s,
            B2 = beta,
            G2 = gamma,
            dib_cm = dib
          )
          if (!is.finite(v_merch) || v_merch < 0) {
            abort_i(
              i,
              "Merchantable volume computation produced non-finite/negative value."
            )
          }
        }
      } else {
        v_merch <- 0
      }
    } else {
      v_merch <- 0
    }

    vol_total[i] <- v_total
    vol_merch[i] <- v_merch
  }

  dplyr::tibble(
    vol_total = vol_total,
    vol_merchantable = vol_merch
  )
}

# vol_zakrzewski2013(DBH = 20, height = 20, species = "PINU.STR")
# vol_zakrzewski2013(DBH = 20, height = 20, species = "ABIE.BAL")
# vol_zakrzewski2013(DBH = 20, height = 20, species = "THUJ.OCC")
