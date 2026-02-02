#' Honer stem volume using DBH and total height
#'
#' Estimates total and merchantable stem volume using the
#' Honer et al. (1983) model, developed for central and eastern Canada.
#'
#' @param DBH Tree diameter at breast height (cm)
#' @param height Total height (m)
#' @param species Species code (e.g., "PINU.CON")
#'
#' @return data.frame with vol_total and vol_merchantable
#'
#' @references
#' Honer, T.G.; Ker, M.F.; Alemdag, I.S. 1983. Metric timber tables for the commercial tree species of central and eastern Canada.
#' Environ. Can., Can. For. Serv., Maritimes For. Res. Cent., Fredericton, NB. Inf. Rep. M-X-140.
#'
#'
#' @examples
#' # Single tree
#' vol_honer83(
#'   DBH = 30,
#'   height = 22,
#'   species = "PICE.GLA"
#' )
#'
#' # Multiple trees, mixed species
#' vol_honer83(
#'   DBH = c(25, 32, 18),
#'   height = c(20, 24, 15),
#'   species = c("PICE.GLA", "ABIE.BAL", "PINU.BAN")
#' )
#'
#' # Works with the pipe operator:
#' trees <- tibble::tibble(
#'   tree_id = 1:4,
#'   dbh = c(22, 30, 18, 35),
#'   ht  = c(18, 24, 15, 27),
#'   species = c("PICE.GLA", "PICE.GLA", "ABIE.BAL", "PINU.BAN")
#' )
#' trees |> dplyr::mutate(vol=vol_honer83(dbh, ht, species)) |> tidyr::unnest(vol)
#' @export

vol_honer83 <- function(DBH, height, species) {
  # ---- internal constants (match original Honer implementation intent) ----
  stumpht <- 0.3048 # m  (1 ft)
  topdbh <- 10 # cm (merchantable top diameter; commonly 10 cm)
  mindbh <- 9 # cm (minimum DBH for merchantable volume)

  # ---- input handling / recycling ----
  n <- max(length(DBH), length(height), length(species))
  DBH <- rep(DBH, length.out = n)
  height <- rep(height, length.out = n)
  species <- rep(species, length.out = n)

  sp_std <- standardize_species_code(species)

  df <- dplyr::tibble(
    dbh = as.numeric(DBH),
    ht = as.numeric(height),
    Species = sp_std
  )

  # ---- get params per species (wide format) ----
  # get_volume_params("regional_honer83", species=..., strict=TRUE) returns ONE row
  params_tbl <- purrr::map_dfr(
    unique(df$Species),
    function(sp) {
      p <- get_volume_params("regional_honer83", species = sp, strict = TRUE)
      # ensure join key exists/consistent
      if (!"Species" %in% names(p)) {
        p$Species <- sp
      }
      p
    }
  )

  df <- df |>
    dplyr::left_join(params_tbl, by = "Species")

  # ---- compute volumes (vectorized; b2a/b2b collapsed to b2) ----
  # guard against invalid geometry
  valid <- is.finite(df$dbh) & is.finite(df$ht) & df$dbh > 0 & df$ht > 0

  totvol <- rep(NA_real_, nrow(df))
  mvol <- rep(NA_real_, nrow(df))
  stpvol <- rep(NA_real_, nrow(df))

  # Total volume
  totvol[valid] <-
    0.0043891 *
    df$dbh[valid]^2 *
    (1 - 0.04365 * df$b2[valid])^2 /
    (df$c1[valid] + (0.3048 * df$c2[valid] / df$ht[valid]))

  totvol <- pmax(totvol, 0)

  # Merchantable volume
  x3 <- rep(NA_real_, nrow(df))
  x3[valid] <-
    topdbh^2 *
    df$dbh[valid]^-2 *
    (1 - 0.04365 * df$b2[valid])^-2 *
    (1 + stumpht / df$ht[valid])

  mvol[valid] <- totvol[valid] *
    (df$r1[valid] + df$r2[valid] * x3[valid] + df$r3[valid] * x3[valid]^2)

  # set merch to zero if negative or dbh < mindbh (original logic)
  mvol <- dplyr::if_else(is.na(mvol), NA_real_, pmax(mvol, 0))
  mvol <- dplyr::if_else(!is.na(df$dbh) & df$dbh < mindbh, 0, mvol)

  # Stump volume (truncated cone; original uses inside-bark table params b2..b5)
  rup <- rep(NA_real_, nrow(df))
  rdown <- rep(NA_real_, nrow(df))

  rup[valid] <-
    (df$b3[valid] +
      df$dbh[valid] *
        (1 - 0.04365 * df$b2[valid]) *
        (df$b4[valid] + df$b5[valid] * log(1.6764 / (stumpht + 0.3048)))) /
    200

  rdown[valid] <-
    (df$b3[valid] +
      df$dbh[valid] *
        (1 - 0.04365 * df$b2[valid]) *
        (df$b4[valid] + df$b5[valid] * log(1.6764 / (0 + 0.3048)))) /
    200

  stpvol[valid] <- (1 / 3) *
    pi *
    (rdown[valid]^2 + rup[valid] * rdown[valid] + rup[valid]^2) *
    stumpht
  stpvol <- dplyr::if_else(is.na(stpvol), NA_real_, pmax(stpvol, 0))

  tipvol <- totvol - mvol - stpvol
  tipvol <- dplyr::if_else(is.na(tipvol), NA_real_, pmax(tipvol, 0))

  dplyr::tibble(
    vol_total = totvol,
    vol_merchantable = mvol
    # vol_tip = tipvol,
    # vol_stump = stpvol
  )
}
