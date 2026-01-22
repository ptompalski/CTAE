#' Canadian national taper model (DBH only)
#'
#' @inheritParams vol_national_dbh_ht
#' @param params Data frame of national taper parameters. Defaults to
#'   `parameters_NationalTaperModelsDBH` included with the package.
#' @param merchcrit Data frame of merchantability criteria. Defaults to
#'   `merchcrit` included with the package.
#'
#' @return A tibble with columns `vol_merchantable` and `vol_total`.
#' @export
vol_national_dbh <- function(
  DBH,
  species,
  jurisdiction,
  params = NULL,
  merchcrit = NULL
) {
  if (!is.numeric(DBH)) {
    stop("`DBH` must be numeric.")
  }
  if (length(species) != 1L || !is.character(species)) {
    stop("`species` must be a single character string.")
  }
  if (length(jurisdiction) != 1L || !is.character(jurisdiction)) {
    stop("`jurisdiction` must be a single character string.")
  }

  d <- .ctae_defaults(params, merchcrit)
  params <- d$params
  merchcrit <- d$merchcrit

  .ctae_check_cols(
    params,
    c("Species", "b0", "b1", "b2", "Di1", "Dij1", "Di2", "Dij2", "Dijk"),
    "params"
  )
  .ctae_check_cols(
    merchcrit,
    c("Province", "Species", "StumpHT", "TopDBH", "MinDBH"),
    "merchcrit"
  )

  merch <- .ctae_merch_row(merchcrit, jurisdiction, species)
  stumpht <- merch$stumpht
  topdbh <- merch$topdbh
  mindbh <- merch$mindbh

  pidx <- match(species, params$Species)
  n <- length(DBH)

  if (is.na(pidx)) {
    return(tibble::tibble(
      vol_merchantable = rep(NA_real_, n),
      vol_total = rep(NA_real_, n)
    ))
  }

  b0 <- params$b0[pidx]
  b1 <- params$b1[pidx]
  b2 <- params$b2[pidx]
  Di1 <- params$Di1[pidx]
  Dij1 <- params$Dij1[pidx]
  Di2 <- params$Di2[pidx]
  Dij2 <- params$Dij2[pidx]
  Dijk <- params$Dijk[pidx]

  V1 <- Di1^2 + Dij1^2
  V2 <- Di2^2 + Dij2^2 + Dijk^2

  stp <- 0.1
  Hd <- 1.3

  vol_total <- numeric(n)
  vol_merch <- numeric(n)

  for (i in seq_len(n)) {
    dbh_i <- DBH[i]

    if (is.na(dbh_i)) {
      vol_total[i] <- NA_real_
      vol_merch[i] <- NA_real_
      next
    }

    Htot <- b0 * dbh_i^b1

    if (!is.finite(Htot) || Htot <= stumpht || Htot <= Hd) {
      vol_total[i] <- 0
      vol_merch[i] <- 0
      next
    }

    H <- seq(stumpht, Htot, by = stp)

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

    vols <- .ctae_vol_from_segments(H, D2C, stp, topdbh, mindbh, dbh_i)
    vol_total[i] <- vols["vol_total"]
    vol_merch[i] <- vols["vol_merch"]
  }

  tibble::tibble(
    vol_merchantable = pmax(0, vol_merch),
    vol_total = pmax(0, vol_total)
  )
}

#' Canadian national taper model (DBH and total height)
#'
#' @param DBH Numeric vector of DBH (cm).
#' @param H Numeric vector of total height (m). Must be same length as DBH,
#'   or length 1 (recycled).
#' @param species Character. Species code (e.g., "PICE.GLA").
#' @param jurisdiction Character. Jurisdiction code (e.g., "BC", "ON").
#' @param params Data frame of national taper parameters. If NULL, uses
#'   `CTAE::parameters_NationalTaperModelsDBH`.
#' @param merchcrit Data frame of merchantability criteria. If NULL, uses
#'   `CTAE::merchcrit`.
#'
#' @return A data frame with columns: vol_merchantable, vol_total
#' @export
vol_national_dbh_ht <- function(
  DBH,
  H,
  species,
  jurisdiction,
  params = NULL,
  merchcrit = NULL,
  studname = NULL
) {
  if (!is.numeric(DBH)) {
    stop("`DBH` must be numeric.")
  }
  if (!is.numeric(H)) {
    stop("`H` must be numeric (total height in m).")
  }
  if (length(species) != 1L || !is.character(species)) {
    stop("`species` must be a single character string.")
  }
  if (length(jurisdiction) != 1L || !is.character(jurisdiction)) {
    stop("`jurisdiction` must be a single character string.")
  }

  n <- length(DBH)
  if (!(length(H) %in% c(1L, n))) {
    stop("`H` must have length 1 or the same length as `DBH`.")
  }
  if (length(H) == 1L) {
    H <- rep(H, n)
  }

  d <- .ctae_defaults(params, merchcrit)
  params <- d$params
  merchcrit <- d$merchcrit

  .ctae_check_cols(
    params,
    c("Species", "b2", "Di2", "Dij2", "Dijk"),
    "params"
  )
  .ctae_check_cols(
    merchcrit,
    c("Province", "Species", "StumpHT", "TopDBH", "MinDBH"),
    "merchcrit"
  )

  merch <- .ctae_merch_row(merchcrit, jurisdiction, species)
  stumpht <- merch$stumpht
  topdbh <- merch$topdbh
  mindbh <- merch$mindbh

  pidx <- match(species, params$Species)
  if (is.na(pidx)) {
    return(tibble::tibble(
      vol_merchantable = rep(NA_real_, n),
      vol_total = rep(NA_real_, n)
    ))
  }

  b <- params$b2[pidx]
  Di2 <- params$Di2[pidx]
  Dij2 <- params$Dij2[pidx]
  Dijk <- params$Dijk[pidx]

  V2 <- Di2^2 + Dij2^2 + Dijk^2

  stp <- 0.1
  Hd <- 1.3

  vol_total <- numeric(n)
  vol_merch <- numeric(n)

  for (i in seq_len(n)) {
    dbh_i <- DBH[i]
    h_i <- H[i]

    if (is.na(dbh_i) || is.na(h_i)) {
      vol_total[i] <- NA_real_
      vol_merch[i] <- NA_real_
      next
    }

    # enforce minimum height at stump height (legacy behavior)
    Htot <- if (h_i < stumpht) stumpht else h_i

    if (!is.finite(Htot) || Htot <= stumpht || Htot <= Hd) {
      vol_total[i] <- 0
      vol_merch[i] <- 0
      next
    }

    Hseq <- seq(stumpht, Htot, by = stp)

    cee <- (Htot - Hseq) / (Htot - Hd)

    D2 <- dbh_i^2 * cee * (Hseq / Hd)^(2 - b)

    Der2 <- D2 * (log(Hseq / Hd))^2
    D2C <- D2 + 0.5 * V2 * Der2

    vols <- .ctae_vol_from_segments(Hseq, D2C, stp, topdbh, mindbh, dbh_i)
    vol_total[i] <- vols["vol_total"]
    vol_merch[i] <- vols["vol_merch"]
  }

  tibble::tibble(
    vol_merchantable = pmax(0, vol_merch),
    vol_total = pmax(0, vol_total)
  )
}


# ---- internal: resolve defaults safely ----
.ctae_defaults <- function(params, merchcrit) {
  if (is.null(params)) {
    params <- CTAE::parameters_NationalTaperModelsDBH
  }
  if (is.null(merchcrit)) {
    merchcrit <- CTAE::merchcrit
  }
  list(params = params, merchcrit = merchcrit)
}

# ---- internal: validate required columns ----
.ctae_check_cols <- function(x, req, argname) {
  miss <- setdiff(req, names(x))
  if (length(miss) > 0) {
    stop("`", argname, "` is missing columns: ", paste(miss, collapse = ", "))
  }
  invisible(TRUE)
}

# ---- internal: get merchantability criteria row ----
# Returns a list(stumpht, topdbh, mindbh)
.ctae_merch_row <- function(merchcrit, jurisdiction, species) {
  idx_bc_species <- which(
    merchcrit$Province == jurisdiction & merchcrit$Species == species
  )
  idx_prov_all <- which(
    merchcrit$Province == jurisdiction & merchcrit$Species == "ALL"
  )
  idx_prov_any <- which(merchcrit$Province == jurisdiction)

  idx <- integer(0)

  if (jurisdiction == "BC") {
    if (length(idx_bc_species) > 0) {
      idx <- idx_bc_species
    } else if (length(idx_prov_all) > 0) {
      idx <- idx_prov_all
    } else if (length(idx_prov_any) > 0) {
      idx <- idx_prov_any
    }
  } else {
    if (length(idx_prov_all) > 0) {
      idx <- idx_prov_all
    } else if (length(idx_prov_any) > 0) {
      idx <- idx_prov_any
    }
  }

  if (length(idx) == 0L) {
    stop(
      "No merchantability criteria found for jurisdiction = '",
      jurisdiction,
      "'."
    )
  }
  idx <- idx[1]

  list(
    stumpht = merchcrit$StumpHT[idx] / 100, # cm -> m
    topdbh = merchcrit$TopDBH[idx],
    mindbh = merchcrit$MinDBH[idx]
  )
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

  # merch threshold uses lower-end diameter per segment (closest to original behavior)
  if (topdbh > 0) {
    D_lo <- sqrt(D2_lo)
    volm <- sum(Vi[D_lo > topdbh])
  } else {
    # If no topdbh is defined, treat merch as total (same logic as legacy)
    volm <- volt
  }

  if (dbh_i < mindbh) {
    volm <- 0
  }

  c(vol_total = volt, vol_merch = volm)
}
