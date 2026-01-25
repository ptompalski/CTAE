#' Canadian national taper model (DBH only)
#'
#' @inheritParams vol_national_dbh_ht
#'
#' @return A tibble with columns `vol_merchantable` and `vol_total`.
#' @references
#' Ung, C.H., Guo, X.J., Fortin, M., 2013. Canadian national taper models. Forestry Chronicle 89, 211–224.
#' https://doi.org/10.5558/tfc2013-040
#' @export
vol_national_dbh <- function(DBH, species, jurisdiction) {
  if (!is.numeric(DBH)) {
    stop("`DBH` must be numeric.")
  }

  # ---- recycle like Honer ----
  n <- max(length(DBH), length(species), length(jurisdiction))
  DBH <- rep(DBH, length.out = n)
  species <- rep(species, length.out = n)
  jurisdiction <- rep(jurisdiction, length.out = n)

  sp_std <- standardize_species_code(species)
  ju_std <- standardize_jurisdiction_code(jurisdiction)

  df <- dplyr::tibble(
    dbh = as.numeric(DBH),
    Species = sp_std,
    jurisdiction = ju_std
  )

  # ---- params per species (wide) ----
  params_tbl <- purrr::map_dfr(
    unique(df$Species),
    function(sp) {
      p <- get_volume_params("national_ung_dbh", species = sp, strict = FALSE)
      if (!"Species" %in% names(p)) {
        p$Species <- sp
      }
      p
    }
  )

  df <- df %>% dplyr::left_join(params_tbl, by = "Species")

  # ---- merch criteria per (jurisdiction, species) ----
  # BC requires species; others ignore it but we can still pass for consistency
  merch_tbl <- df %>%
    dplyr::distinct(jurisdiction, Species) %>%
    dplyr::mutate(
      merch = purrr::pmap(
        list(jurisdiction = jurisdiction, species = Species),
        ~ get_merch_criteria(..1, ..2) %>%
          dplyr::select(
            stumpht_m,
            topdbh_cm,
            mindbh_cm
          )
      )
    ) %>%
    tidyr::unnest(merch)

  df <- df %>% dplyr::left_join(merch_tbl, by = c("jurisdiction", "Species"))

  # ---- compute volumes (rowwise pmap) ----
  vol_one <- function(
    dbh_i,
    stumpht_m,
    topdbh_cm,
    mindbh_cm,
    b0,
    b1,
    b2,
    Di1,
    Dij1,
    Di2,
    Dij2,
    Dijk
  ) {
    if (
      anyNA(c(
        dbh_i,
        stumpht_m,
        topdbh_cm,
        mindbh_cm,
        b0,
        b1,
        b2,
        Di1,
        Dij1,
        Di2,
        Dij2,
        Dijk
      ))
    ) {
      return(c(vol_total = NA_real_, vol_merch = NA_real_))
    }
    if (!is.finite(dbh_i)) {
      return(c(vol_total = NA_real_, vol_merch = NA_real_))
    }

    V1 <- Di1^2 + Dij1^2
    V2 <- Di2^2 + Dij2^2 + Dijk^2

    stp <- 0.1
    Hd <- 1.3

    Htot <- b0 * dbh_i^b1
    if (!is.finite(Htot) || Htot <= stumpht_m || Htot <= Hd) {
      return(c(vol_total = 0, vol_merch = 0))
    }

    H <- seq(stumpht_m, Htot, by = stp)

    cee <- (Htot - H) / (Htot - Hd)
    cee[cee < 0] <- 0

    D2 <- dbh_i^2 * cee * (H / Hd)^(2 - b2)

    Der2 <- D2 * (log(H / Hd))^2

    const1 <- -1 *
      dbh_i^2 *
      (log(dbh_i))^2 *
      b0 *
      dbh_i^b1 *
      (b0 * dbh_i^b1 + Hd) /
      (b0 * dbh_i^b1 - Hd)^3

    Der1 <- const1 * (H / Hd)^(2 - b2) * (H - Hd)

    D2C <- D2 + 0.5 * V1 * Der1 + 0.5 * V2 * Der2

    vols <- .ctae_vol_from_segments(H, D2C, stp, topdbh_cm, mindbh_cm, dbh_i)
    c(vol_total = vols[["vol_total"]], vol_merch = vols[["vol_merch"]])
  }

  out <- purrr::pmap_dfr(
    df %>%
      dplyr::select(
        dbh,
        stumpht_m,
        topdbh_cm,
        mindbh_cm,
        b0,
        b1,
        b2,
        Di1,
        Dij1,
        Di2,
        Dij2,
        Dijk
      ),
    ~ {
      v <- vol_one(
        ..1,
        ..2,
        ..3,
        ..4,
        ..5,
        ..6,
        ..7,
        ..8,
        ..9,
        ..10,
        ..11,
        ..12
      )
      tibble::tibble(
        vol_merchantable = pmax(0, v[["vol_merch"]]),
        vol_total = pmax(0, v[["vol_total"]])
      )
    }
  )

  out
}


#' Canadian national taper model (DBH and total height)
#'
#' @param DBH Numeric vector of DBH (cm).
#' @param H Numeric vector of total height (m). Must be same length as DBH,
#'   or length 1 (recycled).
#' @param species Character. Species code (e.g., "PICE.GLA").
#' @param jurisdiction Character. Jurisdiction code (e.g., "BC", "ON").
#'
#' @references
#' Ung, C.H., Guo, X.J., Fortin, M., 2013. Canadian national taper models. Forestry Chronicle 89, 211–224.
#' https://doi.org/10.5558/tfc2013-040
#' @return A tibble with columns `vol_merchantable` and `vol_total`.
#' @export
vol_national_dbh_ht <- function(DBH, height, species, jurisdiction) {
  if (!is.numeric(DBH)) {
    stop("`DBH` must be numeric.")
  }
  if (!is.numeric(height)) {
    stop("`height` must be numeric (total height in m).")
  }

  H <- height

  n <- max(length(DBH), length(H), length(species), length(jurisdiction))
  DBH <- rep(DBH, length.out = n)
  H <- rep(H, length.out = n)
  species <- rep(species, length.out = n)
  jurisdiction <- rep(jurisdiction, length.out = n)

  sp_std <- standardize_species_code(species)
  ju_std <- standardize_jurisdiction_code(jurisdiction)

  df <- dplyr::tibble(
    dbh = as.numeric(DBH),
    ht = as.numeric(H),
    Species = sp_std,
    jurisdiction = ju_std
  )

  # ---- params per species (wide) ----
  params_tbl <- purrr::map_dfr(
    unique(df$Species),
    function(sp) {
      p <- get_volume_params(
        "national_ung_dbh_ht",
        species = sp,
        strict = FALSE
      )
      if (!"Species" %in% names(p)) {
        p$Species <- sp
      }
      p
    }
  )

  df <- df %>% dplyr::left_join(params_tbl, by = "Species")

  # ---- merch criteria per (jurisdiction, species) ----
  merch_tbl <- df %>%
    dplyr::distinct(jurisdiction, Species) %>%
    dplyr::mutate(
      merch = purrr::pmap(
        list(jurisdiction = jurisdiction, species = Species),
        ~ get_merch_criteria(..1, ..2) %>%
          dplyr::select(
            stumpht_m,
            topdbh_cm,
            mindbh_cm
          )
      )
    ) %>%
    tidyr::unnest(merch)

  df <- df %>% dplyr::left_join(merch_tbl, by = c("jurisdiction", "Species"))

  vol_one <- function(
    dbh_i,
    ht_i,
    stumpht_m,
    topdbh_cm,
    mindbh_cm,
    b2,
    Di2,
    Dij2,
    Dijk
  ) {
    if (
      anyNA(c(
        dbh_i,
        ht_i,
        stumpht_m,
        topdbh_cm,
        mindbh_cm,
        b2,
        Di2,
        Dij2,
        Dijk
      ))
    ) {
      return(c(vol_total = NA_real_, vol_merch = NA_real_))
    }
    if (!is.finite(dbh_i) || !is.finite(ht_i)) {
      return(c(vol_total = NA_real_, vol_merch = NA_real_))
    }

    stp <- 0.1
    Hd <- 1.3
    V2 <- Di2^2 + Dij2^2 + Dijk^2

    # legacy behavior
    Htot <- if (ht_i < stumpht_m) stumpht_m else ht_i

    if (!is.finite(Htot) || Htot <= stumpht_m || Htot <= Hd) {
      return(c(vol_total = 0, vol_merch = 0))
    }

    Hseq <- seq(stumpht_m, Htot, by = stp)

    cee <- (Htot - Hseq) / (Htot - Hd)
    cee[cee < 0] <- 0

    D2 <- dbh_i^2 * cee * (Hseq / Hd)^(2 - b2)

    Der2 <- D2 * (log(Hseq / Hd))^2
    D2C <- D2 + 0.5 * V2 * Der2

    vols <- .ctae_vol_from_segments(Hseq, D2C, stp, topdbh_cm, mindbh_cm, dbh_i)
    c(vol_total = vols[["vol_total"]], vol_merch = vols[["vol_merch"]])
  }

  out <- purrr::pmap_dfr(
    df %>%
      dplyr::select(
        dbh,
        ht,
        stumpht_m,
        topdbh_cm,
        mindbh_cm,
        b2,
        Di2,
        Dij2,
        Dijk
      ),
    ~ {
      v <- vol_one(..1, ..2, ..3, ..4, ..5, ..6, ..7, ..8, ..9)
      tibble::tibble(
        vol_merchantable = pmax(0, v[["vol_merch"]]),
        vol_total = pmax(0, v[["vol_total"]])
      )
    }
  )

  out
}


# ---- internal: integrate segments + apply merch rule ----
# H and D2C must be same length, ordered from stump->top; stp is step size (m)
# Returns c(vol_total=..., vol_merch=...)
.ctae_vol_from_segments <- function(H, D2C, stp, topdbh, mindbh, dbh_i) {
  ok <- is.finite(D2C) & D2C > 0
  if (!any(ok) || sum(ok) < 2L) {
    return(c(vol_total = 0, vol_merch = 0))
  }

  H <- H[ok]
  D2C <- D2C[ok]

  D2_lo <- D2C[-length(D2C)]
  D2_hi <- D2C[-1]

  Vi <- (pi / 80000) * (D2_lo + D2_hi) * stp

  volt <- sum(Vi)
  if (!is.finite(volt) || is.infinite(volt)) {
    volt <- 0
  }

  if (topdbh > 0) {
    D_lo <- sqrt(D2_lo)
    volm <- sum(Vi[D_lo > topdbh])
  } else {
    volm <- volt
  }

  if (dbh_i < mindbh) {
    volm <- 0
  }

  c(vol_total = volt, vol_merch = volm)
}

# # Matches NationalDBHHT() integration + merch rule
# .ctae_vol_from_segments_ung_legacy <- function(
#   H,
#   D2C,
#   stp,
#   topdbh,
#   mindbh,
#   dbh_i
# ) {
#   # Original: clamp negatives to 0 (not drop), keep zeros
#   D2 <- ifelse(is.finite(D2C), pmax(D2C, 0), NA_real_)

#   out <- data.frame(H = H, D2 = D2)
#   out <- out[!is.na(out$D2), , drop = FALSE]
#   out <- out[out$D2 >= 0, , drop = FALSE]
#   if (nrow(out) < 2L) {
#     return(c(vol_total = 0, vol_merch = 0))
#   }

#   # Original: order by decreasing height
#   out <- out[order(-out$H), , drop = FALSE]

#   # Original: lagD2 = next row D2 (i.e., towards stump)
#   out$lagD2 <- c(out$D2[2:nrow(out)], NA_real_)
#   out <- out[!is.na(out$lagD2), , drop = FALSE]

#   out$Vi <- (pi / 80000) * (out$D2 + out$lagD2) * stp
#   out$D <- sqrt(out$D2)

#   volt <- sum(out$Vi)
#   if (is.infinite(volt) || !is.finite(volt)) {
#     volt <- 0
#   }

#   # Original: merch uses D at the row (top-side of each segment after ordering -H)
#   if (topdbh > 0) {
#     volm <- sum(out$Vi[out$D > topdbh])
#   } else {
#     volm <- volt
#   }

#   if (dbh_i < mindbh) {
#     volm <- 0
#   }

#   c(vol_total = volt, vol_merch = volm)
# }
