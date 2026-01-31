#' Canadian national taper volume model (Ung et al. 2013)
#'
#' Uses the national Ung et al. (2013) taper model to estimate total and merchantable
#' volume. If `height` is not provided (or is all `NA`), the DBH-only variant is used.
#' If `height` is provided, the DBH+height variant is used.
#'
#' @param DBH Numeric vector of diameter at breast height (cm).
#' @param height Optional numeric vector of total height (m). If `NULL` (default) or all `NA`,
#'   the DBH-only model is used.
#' @param species Character vector of species codes (e.g., "PICE.GLA").
#' @param jurisdiction Character vector of jurisdiction codes (e.g., "BC", "ON"). Required to assign jurisdiction-specific merchantability criteria
#'
#' @return A tibble with columns `vol_merchantable` and `vol_total` (m^3).
#'
#' @references
#' Ung, C.H., Guo, X.J., Fortin, M. (2013). Canadian national taper models.
#' Forestry Chronicle 89, 211–224. https://doi.org/10.5558/tfc2013-040
#'
#' @examples
#' # --- DBH-only model (height not provided) ---
#' vol_ung2013(
#'   DBH = c(20, 30, 40),
#'   species = c("PICE.GLA", "ABIE.BAL", "PICE.GLA"),
#'   jurisdiction = "ON"
#' )
#'
#' # --- DBH + height model ---
#' vol_ung2013(
#'   DBH = c(20, 30, 40),
#'   height = c(18, 24, 30),
#'   species = c("PICE.GLA", "ABIE.BAL", "PICE.GLA"),
#'   jurisdiction = "ON"
#' )
#'
#' # --- Vector recycling (common in dplyr pipelines) ---
#' vol_ung2013(
#'   DBH = c(25, 35),
#'   height = 22,
#'   species = "PICE.GLA",
#'   jurisdiction = "QC"
#' )
#'
#' # --- Using inside mutate(): DBH-only fallback ---
#' trees <- tibble::tibble(
#'   DBH = c(22, 30, 18),
#'   species = c("PICE.GLA", "PICE.GLA", "ABIE.BAL")
#' )
#'
#' trees |>
#'   dplyr::mutate(
#'     vol = vol_ung2013(DBH, species = species, jurisdiction = "NB")
#'   ) %>%
#' unnest(vol)
#'
#' @export
vol_ung2013 <- function(DBH, height = NULL, species, jurisdiction) {
  if (!is.numeric(DBH)) {
    stop("`DBH` must be numeric.")
  }

  use_dbh_only <- is.null(height) || (is.numeric(height) && all(is.na(height)))

  if (use_dbh_only) {
    return(vol_national_dbh_engine(
      DBH = DBH,
      species = species,
      jurisdiction = jurisdiction
    ))
  }

  if (!is.numeric(height)) {
    stop("`height` must be numeric (total height in m), or NULL.")
  }

  vol_national_dbh_ht_engine(
    DBH = DBH,
    height = height,
    species = species,
    jurisdiction = jurisdiction
  )
}


# internal: standardize + recycle + join params + join merch criteria
.ung_prepare_df <- function(DBH, height, species, jurisdiction, model_id) {
  # recycle
  if (is.null(height)) {
    n <- max(length(DBH), length(species), length(jurisdiction))
    DBH <- rep(DBH, length.out = n)
    species <- rep(species, length.out = n)
    jurisdiction <- rep(jurisdiction, length.out = n)

    df <- dplyr::tibble(
      dbh = as.numeric(DBH),
      Species = standardize_species_code(species),
      jurisdiction = standardize_jurisdiction_code(jurisdiction)
    )
  } else {
    n <- max(length(DBH), length(height), length(species), length(jurisdiction))
    DBH <- rep(DBH, length.out = n)
    height <- rep(height, length.out = n)
    species <- rep(species, length.out = n)
    jurisdiction <- rep(jurisdiction, length.out = n)

    df <- dplyr::tibble(
      dbh = as.numeric(DBH),
      ht = as.numeric(height),
      Species = standardize_species_code(species),
      jurisdiction = standardize_jurisdiction_code(jurisdiction)
    )
  }

  # params per species (ensure 1 row per Species to avoid many-to-many surprises)
  params_tbl <- purrr::map_dfr(
    unique(df$Species),
    function(sp) {
      p <- get_volume_params(model_id, species = sp, strict = FALSE)
      if (!"Species" %in% names(p)) {
        p$Species <- sp
      }
      dplyr::as_tibble(p)
    }
  ) %>%
    dplyr::distinct(Species, .keep_all = TRUE)

  df <- dplyr::left_join(df, params_tbl, by = "Species")

  # merch criteria per (jurisdiction, species)
  merch_tbl <- df %>%
    dplyr::distinct(jurisdiction, Species) %>%
    dplyr::mutate(
      merch = purrr::pmap(
        list(jurisdiction = jurisdiction, species = Species),
        ~ get_merch_criteria(..1, ..2) %>%
          dplyr::select(stumpht_m, topdbh_cm, mindbh_cm)
      )
    ) %>%
    tidyr::unnest(merch)

  dplyr::left_join(df, merch_tbl, by = c("jurisdiction", "Species"))
}


# internal
vol_national_dbh_engine <- function(DBH, species, jurisdiction) {
  df <- .ung_prepare_df(
    DBH = DBH,
    height = NULL,
    species = species,
    jurisdiction = jurisdiction,
    model_id = "national_ung_dbh"
  )

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

  purrr::pmap_dfr(
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
}


# internal
vol_national_dbh_ht_engine <- function(DBH, height, species, jurisdiction) {
  df <- .ung_prepare_df(
    DBH = DBH,
    height = height,
    species = species,
    jurisdiction = jurisdiction,
    model_id = "national_ung_dbh_ht"
  )

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

  purrr::pmap_dfr(
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
