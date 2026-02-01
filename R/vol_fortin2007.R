#' Estimate tree merchantable volume using Fortin et al. (2007) model (QC)
#'
#' Deterministic (fixed-effects only) implementation of the Fortin et al.
#' general merchantable volume model developed for Québec forests.
#'
#' This function returns merchantable stem volume (under bark) at the
#' individual-tree level as a function of diameter at breast height (DBH)
#' and total tree height. Only the fixed-effects (mean) component of the
#' original mixed-effects formulation is implemented in CTAE; stochastic
#' components (random effects and residual variance) are intentionally
#' omitted.
#'
#' Merchantable volume is defined according to Québec provincial inventory
#' standards. Trees with DBH < 9.1 cm are considered non-merchantable and
#' have merchantable volume equal to zero.
#'
#' Users requiring the complete mixed-effects model formulation and full
#' functionality (e.g., stochastic predictions and uncertainty propagation)
#' are directed to the official Java implementation available in the
#' \emph{CFSForestTools} repository:
#' \url{https://github.com/CWFC-CCFB/CFSForestTools}
#'
#' @param DBH Numeric vector of diameter at breast height (cm).
#' @param height Numeric vector of total tree height (m).
#' @param species Character vector of species codes (standardized with
#'   `standardize_species_code()`).
#'
#' @return A tibble with volumes (m^3):
#' \describe{
#'   \item{vol_total}{Numeric. Total volume (m\eqn{^3}). Currently returned as
#'     \code{NA} as a placeholder; total volume will be handled later via
#'     expansion factors.}
#'   \item{vol_merchantable}{Numeric. Merchantable stem volume (m\eqn{^3},
#'     under bark).}
#' }
#'
#' @source
#' Fortin, M., DeBlois, J., Bédard, S., Meunier, S. (2007).
#' \emph{Mise au point d’un tarif de cubage général pour les forêts québécoises :
#' une approche pour mieux tenir compte des effets de la dimension des arbres.}
#' Gouvernement du Québec, Ministère des Ressources naturelles et de la Faune.
#'
#' Full reference implementation:
#' \url{https://github.com/CWFC-CCFB/CFSForestTools}
#'
#' @seealso
#' \code{\link{parameters_fortin2007}}
#' @examples
#' # Single tree
#' vol_fortin2007(
#'   DBH = 20,
#'   height = 18,
#'   species = "PICE.MAR"
#' )
#'
#' # Multiple trees, vectorized
#' vol_fortin2007(
#'   DBH = c(18, 25, 32),
#'   height = c(15, 20, 24),
#'   species = c("ABIE.BAL", "PICE.GLA", "PINU.BAN")
#' )
#'
#'
#' @export
vol_fortin2007 <- function(DBH, height, species) {
  n <- length(DBH)
  if (length(height) != n || length(species) != n) {
    rlang::abort("DBH, height, and species must have the same length.")
  }

  species_std <- standardize_species_code(species)

  # ---- internal: structured abort with context ----
  abort_i <- function(i, msg) {
    rlang::abort(paste0(
      "vol_fortin2007() failed for row ",
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

  # ---- outputs ----
  vol_merch <- numeric(n)
  vol_total <- rep(NA_real_, n) # placeholder for now

  # ---- cache params once per species ----
  sp_unique <- unique(species_std)

  params_tbl <- purrr::map_dfr(
    sp_unique,
    ~ get_volume_params("regional_fortin2007", species = .x) %>%
      dplyr::mutate(.species_key = .x)
  )

  if (nrow(params_tbl) == 0) {
    rlang::abort(
      "No parameters returned by get_volume_params(model_id='regional_fortin2007')."
    )
  }

  need_p <- c(".species_key", "b1", "b2", "b3")
  miss_p <- setdiff(need_p, names(params_tbl))
  if (length(miss_p) > 0) {
    rlang::abort(paste0(
      "Fortin2007 parameter table is missing required columns: ",
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
    if (ht < 1.3) {
      abort_i(i, "height must be >= 1.3 m (model requirement).")
    }

    # merchantability threshold (as per QC implementation)
    if (dbh < 9.1) {
      vol_merch[i] <- 0
      next
    }

    p <- params_tbl %>%
      dplyr::filter(.data$.species_key == species_std[i])

    if (nrow(p) == 0) {
      abort_i(
        i,
        "No Fortin2007 parameters found for this species (regional_fortin2007)."
      )
    }
    if (nrow(p) > 1) {
      abort_i(
        i,
        "Multiple Fortin2007 parameter rows returned; expected exactly one."
      )
    }

    # validate finite params
    for (nm in c("b1", "b2", "b3")) {
      if (!is.finite(p[[nm]][[1]])) {
        abort_i(i, paste0("Parameter '", nm, "' is not finite (NA/Inf)."))
      }
    }

    b1 <- p$b1[[1]]
    b2 <- p$b2[[1]]
    b3 <- p$b3[[1]]

    # Java implementation uses:
    # cyl = pi * dbh^2 * ht / 40  (dbh cm, ht m) => dm^3
    cyl_dm3 <- pi * (dbh^2) * ht / 40
    if (!is.finite(cyl_dm3) || cyl_dm3 <= 0) {
      abort_i(i, "Computed cylinder term is non-finite or <= 0.")
    }

    pred_dm3 <- b1 * (ht / dbh) + b2 * cyl_dm3 + b3 * (cyl_dm3 * dbh)

    if (!is.finite(pred_dm3)) {
      abort_i(i, "Prediction is not finite.")
    }
    if (pred_dm3 < 0) {
      abort_i(i, "Prediction is negative (check inputs / parameters).")
    }

    # convert dm^3 -> m^3
    vol_merch[i] <- pred_dm3 / 1000
  }

  dplyr::tibble(
    vol_total = vol_total,
    vol_merchantable = vol_merch
  )
}
