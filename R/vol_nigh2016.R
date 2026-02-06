#' Calculate tree total and merchantable volume using Nigh (2016) equations (BC)
#'
#' Implementation of the British Columbia volume equations developed by Nigh (2016).
#' The model calculates total stem volume and merchantable stem volume as a function of diameter at
#' breast height (DBH) and total tree height.
#'
#' Parameters are stratified either by region (\code{"Coast"}, \code{"Interior"})
#' or by BEC zone (e.g., \code{"CWH"}, \code{"ICH"}, \code{"BWBS"}), depending on
#' which parameter row is requested via \code{subregion}.
#'
#' Model form (Nigh 2016):
#' \deqn{V = \exp(b_0) \cdot DBH^{b_1} \cdot H^{b_2}}
#' where \eqn{V} is volume (m\eqn{^3}), \eqn{DBH} is in cm, and \eqn{H} is in m.
#'
#' Merchantable volume in Nigh (2016) is defined as total volume minus stump
#' (0.3 m) and top volume above a 4 cm inside-bark diameter threshold (fixed
#' definition in the original report).
#'
#' @param DBH Numeric vector of diameter at breast height (cm).
#' @param height Numeric vector of total tree height (m).
#' @param species Character vector of species codes
#' @param subregion Character scalar or vector defining the spatial stratum for
#'   the coefficients. Can be \code{"Coast"} / \code{"Interior"} or a BEC zone code
#'   (e.g., \code{"CWH"}, \code{"ICH"}, \code{"BWBS"}). Must have length 1 or the
#'   same length as \code{DBH}.
#'
#' @return A tibble with volumes (m\eqn{^3}):
#' \describe{
#'   \item{vol_total}{Numeric. Total stem volume (m\eqn{^3}), under bark.}
#'   \item{vol_merchantable}{Numeric. Merchantable stem volume (m\eqn{^3}), under bark.}
#' }
#'
#' @source
#' Nigh, G.D. (2016). \emph{Total and merchantable volume equations for common tree
#' species in British Columbia: by region and biogeoclimatic zone}. Province of
#' British Columbia, Technical Report 106.
#'
#'
#' @examples
#' # Single tree, region coefficients
#' vol_nigh2016(
#'   DBH = 35,
#'   height = 25,
#'   species = "POPU.TRI",
#'   subregion = "Interior"
#' )
#'
#' # Multiple trees, vectorized (mixed subregions allowed)
#' vol_nigh2016(
#'   DBH = c(22, 30, 45),
#'   height = c(18, 24, 33),
#'   species = c("PSEU.MEN", "THUJ.PLI", "POPU.TRI"),
#'   subregion = c("Coast", "ICH", "Interior")
#' )
#'
#' @export
vol_nigh2016 <- function(DBH, height, species, subregion) {
  n <- length(DBH)

  if (length(height) != n || length(species) != n) {
    rlang::abort("DBH, height, and species must have the same length.")
  }
  if (missing(subregion)) {
    rlang::abort(
      "subregion is required for vol_nigh2016() (e.g., 'Coast', 'Interior', or a BEC zone)."
    )
  }
  if (!(length(subregion) %in% c(1, n))) {
    rlang::abort("subregion must have length 1 or the same length as DBH.")
  }

  # recycle scalar subregion
  if (length(subregion) == 1) {
    subregion <- rep(subregion, n)
  }

  species_std <- standardize_species_code(species)

  # ---- internal: structured abort with context ----
  abort_i <- function(i, msg) {
    rlang::abort(paste0(
      "vol_nigh2016() failed for row ",
      i,
      " (species=",
      species_std[i],
      ", subregion=",
      subregion[i],
      ", DBH_cm=",
      DBH[i],
      ", ht_m=",
      height[i],
      "): ",
      msg
    ))
  }

  # ---- outputs ----
  vol_total <- numeric(n)
  vol_merch <- numeric(n)

  # ---- cache params once per (species, subregion) ----
  key_tbl <- dplyr::tibble(
    .species_key = species_std,
    .subregion_key = subregion
  ) |>
    dplyr::distinct()

  params_tbl <- purrr::pmap_dfr(
    list(key_tbl$.species_key, key_tbl$.subregion_key),
    function(sp, subr) {
      get_volume_params(
        model_id = "regional_nigh2016",
        species = sp,
        subregion = subr
      ) |>
        dplyr::mutate(.species_key = sp, .subregion_key = subr)
    }
  )

  if (nrow(params_tbl) == 0) {
    rlang::abort(
      "No parameters returned by get_volume_params(model_id='regional_nigh2016')."
    )
  }

  need_p <- c(".species_key", ".subregion_key", "volume_type", "b0", "b1", "b2")
  miss_p <- setdiff(need_p, names(params_tbl))
  if (length(miss_p) > 0) {
    rlang::abort(paste0(
      "Nigh2016 parameter table is missing required columns: ",
      paste(miss_p, collapse = ", ")
    ))
  }

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
    if (ht < 1.3) {
      abort_i(i, "height must be >= 1.3 m (model requirement).")
    }

    p <- params_tbl |>
      dplyr::filter(
        .data$.species_key == species_std[i],
        .data$.subregion_key == subregion[i]
      )

    if (nrow(p) == 0) {
      abort_i(
        i,
        "No Nigh2016 parameters found for this species + subregion (regional_nigh2016)."
      )
    }

    # Expect exactly one row per volume_type for the given key
    p_tot <- p |> dplyr::filter(.data$volume_type %in% c("total"))
    p_mer <- p |>
      dplyr::filter(.data$volume_type %in% c("merchantable", "merch"))

    if (nrow(p_tot) != 1) {
      abort_i(
        i,
        "Expected exactly one 'total' parameter row for this species + subregion."
      )
    }
    if (nrow(p_mer) != 1) {
      abort_i(
        i,
        "Expected exactly one 'merchantable' parameter row for this species + subregion."
      )
    }

    # validate finite params
    check_params <- function(row, label) {
      for (nm in c("b0", "b1", "b2")) {
        v <- row[[nm]][[1]]
        if (!is.finite(v)) {
          abort_i(
            i,
            paste0(
              "Parameter '",
              nm,
              "' for ",
              label,
              " volume is not finite (NA/Inf)."
            )
          )
        }
      }
    }
    check_params(p_tot, "total")
    check_params(p_mer, "merchantable")

    # predict volumes (m^3)
    pred_fun <- function(b0, b1, b2) {
      out <- exp(b0) * (dbh^b1) * (ht^b2)
      out
    }

    vt <- pred_fun(p_tot$b0[[1]], p_tot$b1[[1]], p_tot$b2[[1]])
    vm <- pred_fun(p_mer$b0[[1]], p_mer$b1[[1]], p_mer$b2[[1]])

    if (!is.finite(vt) || !is.finite(vm)) {
      abort_i(i, "Prediction is not finite.")
    }
    if (vt < 0 || vm < 0) {
      abort_i(i, "Prediction is negative (check inputs / parameters).")
    }

    vol_total[i] <- vt
    vol_merch[i] <- vm
  }

  dplyr::tibble(
    vol_total = vol_total,
    vol_merchantable = vol_merch
  )
}
