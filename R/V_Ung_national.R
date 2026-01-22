#' Canadian national taper models (DBH only)
#'
#' @param DBH Numeric vector of DBH (cm).
#' @param species Character. Species code (e.g., "ABIE.BAL").
#' @param jurisdiction Character. Jurisdiction code (e.g., "BC", "ON").
#' @param params Data frame of national taper parameters. Defaults to
#'   `parameters_NationalTaperModelsDBH` included with the package.
#' @param merchcrit Data frame of merchantability criteria. Defaults to
#'   `merchcrit` included with the package.
#'
#' @return A data frame with one row per input DBH and columns:
#' \describe{
#'   \item{vol_merchantable}{Merchantable stem volume}
#'   \item{vol_total}{Total stem volume}
#' }
#' @references
#' Ung, C.H., Guo, X.J., Fortin, M., 2013. Canadian national taper models. Forestry Chronicle 89, 211–224. https://doi.org/10.5558/tfc2013-040
#'
#' @examples
#' vol_national_dbh(
#'   DBH = 20,
#'   species = "PICE.GLA",
#'   jurisdiction = "AB"
#' )
#'
#' # Vectorized over DBH
#' vol_national_dbh(
#'   DBH = c(15, 25, 35),
#'   species = "PSEU.MEN",
#'   jurisdiction = "BC"
#' )
#'
#' @export
#'
vol_national_dbh <- function(
  DBH,
  species,
  jurisdiction,
  params = NULL,
  merchcrit = NULL
) {
  # ---- input checks ----
  if (!is.numeric(DBH)) {
    stop("`DBH` must be numeric.")
  }
  if (length(species) != 1L || !is.character(species)) {
    stop("`species` must be a single character string.")
  }
  if (length(jurisdiction) != 1L || !is.character(jurisdiction)) {
    stop("`jurisdiction` must be a single character string.")
  }

  if (is.null(params)) {
    params <- CTAE::parameters_NationalTaperModelsDBH
  }
  if (is.null(merchcrit)) {
    merchcrit <- CTAE::merchcrit
  }

  req_p <- c("Species", "b0", "b1", "b2", "Di1", "Dij1", "Di2", "Dij2", "Dijk")
  miss_p <- setdiff(req_p, names(params))
  if (length(miss_p) > 0) {
    stop("`params` is missing columns: ", paste(miss_p, collapse = ", "))
  }

  req_m <- c("Province", "Species", "StumpHT", "TopDBH", "MinDBH")
  miss_m <- setdiff(req_m, names(merchcrit))
  if (length(miss_m) > 0) {
    stop("`merchcrit` is missing columns: ", paste(miss_m, collapse = ", "))
  }

  # ---- merchantability criteria ----
  # In your merchcrit, most provinces are "ALL"; BC is typically species-specific in the original logic.
  idx_bc_species <- which(
    merchcrit$Province == jurisdiction & merchcrit$Species == species
  )
  idx_prov_all <- which(
    merchcrit$Province == jurisdiction & merchcrit$Species == "ALL"
  )
  idx_prov_any <- which(merchcrit$Province == jurisdiction)

  idx <- integer(0)

  if (jurisdiction == "BC") {
    # Prefer BC+species; if not present, fall back to BC+ALL; otherwise first BC row.
    if (length(idx_bc_species) > 0) {
      idx <- idx_bc_species
    } else if (length(idx_prov_all) > 0) {
      idx <- idx_prov_all
    } else if (length(idx_prov_any) > 0) {
      idx <- idx_prov_any
    }
  } else {
    # Prefer province+ALL; otherwise first province row.
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

  stumpht <- merchcrit$StumpHT[idx] / 100 # cm -> m
  topdbh <- merchcrit$TopDBH[idx]
  mindbh <- merchcrit$MinDBH[idx]

  # Criteria not used here (kept for parity)
  topht <- -999

  # ---- parameters for species ----
  pidx <- match(species, params$Species)

  if (is.na(pidx)) {
    n <- length(DBH)
    return(data.frame(
      mvol = rep(NA_real_, n),
      stpvol = rep(NA_real_, n),
      tipvol = rep(NA_real_, n),
      totvol = rep(NA_real_, n)
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

  # ---- integration settings ----
  stp <- 0.1
  Hd <- 1.3

  n <- length(DBH)
  tvol <- numeric(n)
  mvol <- numeric(n)
  stpvol <- numeric(n)
  tipvol <- numeric(n)

  for (i in seq_len(n)) {
    dbh_i <- DBH[i]

    if (is.na(dbh_i)) {
      tvol[i] <- NA_real_
      mvol[i] <- NA_real_
      stpvol[i] <- NA_real_
      tipvol[i] <- NA_real_
      next
    }

    # Total height model
    Htot <- b0 * dbh_i^b1

    # guard against nonsensical heights
    if (!is.finite(Htot) || Htot <= stumpht || Htot <= Hd) {
      tvol[i] <- 0
      mvol[i] <- 0
      stpvol[i] <- 0
      tipvol[i] <- 0
      next
    }

    H <- seq(stumpht, Htot, by = stp)

    # cee term, truncated at 0
    cee <- (Htot - H) / (Htot - Hd)
    cee[cee < 0] <- 0

    # squared diameter along stem
    D2 <- dbh_i^2 * cee * (H / Hd)^(2 - b2)

    # correction terms (vectorized)
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

    ok <- is.finite(D2C) & D2C > 0
    if (!any(ok)) {
      tvol[i] <- 0
      mvol[i] <- 0
      stpvol[i] <- 0
      tipvol[i] <- 0
      next
    }

    H <- H[ok]
    D2C <- D2C[ok]

    # trapezoidal integration over height segments
    if (length(D2C) < 2L) {
      volt <- 0
      volm <- 0
    } else {
      D2_lo <- D2C[-length(D2C)]
      D2_hi <- D2C[-1]
      Vi <- (pi / 80000) * (D2_lo + D2_hi) * stp

      # Use lower-end diameter for merchantability threshold (closest to original)
      D_lo <- sqrt(D2_lo)
      H_lo <- H[-length(H)]

      volt <- sum(Vi)

      if (topdbh > 0) {
        volm <- sum(Vi[D_lo > topdbh])
      } else if (topht > 0) {
        volm <- sum(Vi[H_lo < topht])
      } else {
        volm <- volt
      }

      if (dbh_i < mindbh) volm <- 0
    }

    tvol[i] <- volt
    mvol[i] <- volm

    nonm <- max(0, volt - volm)
    stpvol[i] <- nonm / 2
    tipvol[i] <- nonm / 2
  }

  tibble::tibble(
    vol_merchantable = pmax(0, mvol),
    # stpvol = pmax(0, stpvol),
    # tipvol = pmax(0, tipvol),
    vol_total = pmax(0, tvol)
  )
}
