#' Sharma (2021) stem volume using DBH and total height
#'
#' Estimates total (inside-bark) and merchantable (inside-bark) stem volume
#' using the Sharma (2021) dimensionally compatible model developed for
#' commercial tree species in central/eastern Canada and the northeastern US.
#'
#' Merchantable volume parameters correspond to Sharma's fixed merchantability
#' definition (0.3 m stump, 7 cm inside-bark top diameter), and are not
#' user-configurable.
#'
#' @param DBH Tree diameter at breast height (cm)
#' @param height Total height (m)
#' @param species Species code (e.g., "PICE.MAR")
#'
#' @return tibble with columns \code{vol_total} and \code{vol_merchantable} (m3)
#'
#' @references
#' Sharma, M. 2021. Total and Merchantable Volume Equations for 25 Commercial
#' Tree Species Grown in Canada and the Northeastern United States. Forests 12:1270.
#'
#' @examples
#' # Single tree
#' vol_sharma2021(
#'   DBH = 30,
#'   height = 22,
#'   species = "PICE.MAR"
#' )
#'
#' # Multiple trees, mixed species
#' vol_sharma2021(
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
#' trees |>
#'   dplyr::mutate(vol = vol_sharma2021(dbh, ht, species)) |>
#'   tidyr::unnest(vol)
#' @export
vol_sharma2021 <- function(DBH, height, species) {
  n <- max(length(DBH), length(height), length(species))
  DBH <- rep(DBH, length.out = n)
  height <- rep(height, length.out = n)
  species <- rep(species, length.out = n)

  sp_std <- standardize_species_code(species)

  df <- dplyr::tibble(
    row_id = seq_len(n),
    Species = sp_std,
    D_m = as.numeric(DBH) / 100, # cm -> m
    H_m = as.numeric(height)
  )

  # params per Species (2 rows: total_ib + merch_ib)
  params_tbl <- purrr::map_dfr(
    unique(df$Species),
    function(sp) {
      p <- get_volume_params("regional_sharma2021", species = sp, strict = TRUE)
      if (!"Species" %in% names(p)) {
        p$Species <- sp
      }

      req <- c("total_inside_bark", "merchantable_inside_bark")

      p <- p |>
        dplyr::filter(volume_type %in% req) |>
        dplyr::select(Species, volume_type, alpha, beta, gamma)

      missing_req <- setdiff(req, unique(p$volume_type))
      if (length(missing_req) > 0) {
        rlang::abort(paste0(
          "vol_sharma2021(): missing parameter row(s) for species=",
          sp,
          " in get_volume_params(model_id='regional_sharma2021'): ",
          paste(missing_req, collapse = ", ")
        ))
      }

      p
    }
  )

  # reshape params to wide so each Species has one row (no join explosion)
  params_wide <- params_tbl |>
    tidyr::pivot_wider(
      names_from = volume_type,
      values_from = c(alpha, beta, gamma)
    )

  df2 <- df |>
    dplyr::left_join(params_wide, by = "Species")

  valid <- is.finite(df2$D_m) & is.finite(df2$H_m) & df2$D_m > 0 & df2$H_m > 0

  # helper
  calc_V <- function(alpha, beta, gamma, D, H) {
    alpha + beta * (D^gamma) * (H^(3 - gamma))
  }

  vol_total <- rep(NA_real_, nrow(df2))
  vol_merch <- rep(NA_real_, nrow(df2))

  # total inside bark
  vol_total[valid] <- calc_V(
    alpha = df2$alpha_total_inside_bark[valid],
    beta = df2$beta_total_inside_bark[valid],
    gamma = df2$gamma_total_inside_bark[valid],
    D = df2$D_m[valid],
    H = df2$H_m[valid]
  )

  # merchantable inside bark (Sharma fixed merch definition)
  vol_merch[valid] <- calc_V(
    alpha = df2$alpha_merchantable_inside_bark[valid],
    beta = df2$beta_merchantable_inside_bark[valid],
    gamma = df2$gamma_merchantable_inside_bark[valid],
    D = df2$D_m[valid],
    H = df2$H_m[valid]
  )

  # clamp negatives (merchantable intercept is negative)
  vol_total <- dplyr::if_else(is.na(vol_total), NA_real_, pmax(vol_total, 0))
  vol_merch <- dplyr::if_else(is.na(vol_merch), NA_real_, pmax(vol_merch, 0))

  dplyr::tibble(
    vol_total = vol_total,
    vol_merchantable = vol_merch
  )
}
