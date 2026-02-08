# Lambert & Ung AGB (CTAE-style refactor, vectorized + cached coefficients)
# -------------------------------------------------------------------------
# Changes requested:
#   1) Assume `standardize_species_code()` exists (CTAE-wide helper) and use it directly
#   2) Allow `height` to be NA in validation (needed for equation_set = "auto" or "dbh")
#
# Core behavior:
#   - equation_set = "auto": per-tree choice
#       * if height is valid (>0, finite, non-NA) -> DBHHT
#       * else                                   -> DBH
#   - equation_set = "dbh":        always DBH
#   - equation_set = "dbh_height": always DBHHT (requires valid height for all rows)
#
# Performance:
#   - vectorized across species and equation set, grouped by (model_id, species)
#   - coefficients cached in a closure (built once per session)

# ---- internal: validate numeric inputs and recycling rules --------------

# Validate DBH and height.
# DBH: must be numeric, non-NA, > 0
# height: must be numeric; NA is allowed (auto/dbh), but any *non-NA* values must be > 0
# (We validate "height required" separately when equation_set forces DBHHT.)
.lu_validate_inputs <- function(DBH, height = NULL) {
  # DBH checks
  if (!is.numeric(DBH)) {
    rlang::abort("`DBH` must be numeric.")
  }
  if (anyNA(DBH)) {
    rlang::abort("`DBH` contains NA values.")
  }
  if (any(DBH <= 0)) {
    rlang::abort("`DBH` must be > 0.")
  }

  # height checks (if supplied)
  if (!is.null(height)) {
    if (!is.numeric(height)) {
      rlang::abort("`height` must be numeric.")
    }

    # height may be NA (e.g., equation_set = "auto"), but non-NA values must be > 0
    h_ok <- is.na(height) | (is.finite(height) & height > 0)
    if (any(!h_ok)) {
      rlang::abort("`height` must be > 0 and finite (or NA when allowed).")
    }

    # Recycling sanity: DBH and height must be same length, or one of them length 1
    n1 <- length(DBH)
    n2 <- length(height)
    if (!(n1 == n2 || n1 == 1L || n2 == 1L)) {
      rlang::abort(
        "`DBH` and `height` must have the same length, or one must be length 1."
      )
    }
  }

  invisible(TRUE)
}


# ---- internal: standardize species code (CTAE helper) --------------------

# CTAE assumption: `standardize_species_code()` exists and returns NFI-style codes.
# We still handle NA/blank by mapping to UNKN.SPP (safe fallback).
.lu_standardize_species <- function(species) {
  if (!is.character(species)) {
    species <- as.character(species)
  }

  species <- vapply(species, standardize_species_code, character(1))

  species[is.na(species) | !nzchar(species)] <- "UNKN.SPP"
  species
}


# ---- internal: cached coefficient map -----------------------------------

.lu_coef_map <- local({
  cache <- NULL

  function(params = parameters_LambertUng, refresh = FALSE) {
    if (!isTRUE(refresh) && !is.null(cache)) {
      return(cache)
    }

    params2 <- params |>
      dplyr::mutate(
        model = stringr::str_trim(.data$model),
        species = stringr::str_trim(toupper(.data$species)),
        parameter = stringr::str_trim(.data$parameter)
      )

    # cache[[model_id]][[species]] -> named numeric vector of coefficients
    cache <<- params2 |>
      dplyr::group_by(.data$model, .data$species) |>
      dplyr::summarise(
        coefs = list(stats::setNames(.data$estimate, .data$parameter)),
        .groups = "drop"
      ) |>
      dplyr::nest_by(.data$model) |>
      dplyr::mutate(
        species_map = list(stats::setNames(data$coefs, data$species))
      ) |>
      dplyr::select("model", "species_map") |>
      tibble::deframe()

    cache
  }
})

.lu_get_coefs_cached <- function(model_id, species) {
  m <- .lu_coef_map()

  if (!model_id %in% names(m)) {
    rlang::abort(paste0(
      "No coefficients found for model_id = `",
      model_id,
      "`."
    ))
  }
  smap <- m[[model_id]]
  if (!species %in% names(smap)) {
    rlang::abort(paste0(
      "Unknown species `",
      species,
      "` for model_id = `",
      model_id,
      "`."
    ))
  }

  smap[[species]]
}


# ---- internal: compute AGB components -----------------------------------

.lu_agb_components_dbh <- function(DBH, coefs) {
  with(as.list(coefs), {
    Ywood <- bwood1 * DBH^bwood2
    Ybark <- bbark1 * DBH^bbark2
    Yfoliage <- bfoliage1 * DBH^bfoliage2
    Ybranches <- bbranches1 * DBH^bbranches2

    tibble::tibble(
      Bwood = Ywood,
      Bbark = Ybark,
      Bstem = Ywood + Ybark,
      Bfoliage = Yfoliage,
      Bbranches = Ybranches,
      Bcrown = Yfoliage + Ybranches,
      Btotal = Ywood + Ybark + Yfoliage + Ybranches
    )
  })
}

.lu_agb_components_dbhht <- function(DBH, height, coefs) {
  with(as.list(coefs), {
    Ywood <- bwood1 * DBH^bwood2 * height^bwood3
    Ybark <- bbark1 * DBH^bbark2 * height^bbark3
    Yfoliage <- bfoliage1 * DBH^bfoliage2 * height^bfoliage3
    Ybranches <- bbranches1 * DBH^bbranches2 * height^bbranches3

    tibble::tibble(
      Bwood = Ywood,
      Bbark = Ybark,
      Bstem = Ywood + Ybark,
      Bfoliage = Yfoliage,
      Bbranches = Ybranches,
      Bcrown = Yfoliage + Ybranches,
      Btotal = Ywood + Ybark + Yfoliage + Ybranches
    )
  })
}


# ---- public: agb_lambert_ung() ------------------------------------------

#' Calculate tree-level AGB using Lambert & Ung national biomass equations
#'
#' @description
#' Unified interface for the Lambert et al. (2005) and Ung et al. (2008) Canadian
#' national tree aboveground biomass equations. Two equation sets are available:
#' dbh-only and dbh+height. By default (`equation_set = "auto"`), the dbh+height
#' set is used when valid `height` is provided, otherwise the dbh-only set is used.
#'
#' @param DBH Tree DBH (cm). Numeric vector.
#' @param height Tree height (m). Numeric vector. Optional for the dbh-only equation set.
#' @param species Tree species code in the NFI standard (e.g. `"POPU.TRE"`). Can be a
#'   scalar (recycled) or a vector matching `DBH`.
#' @param equation_set Which Lambert & Ung equation set to use. One of `"auto"`, `"dbh"`,
#'   `"dbh_height"`. `"auto"` makes a per-tree choice based on whether `height` is valid.
#' @param keep_model_id Logical. If `TRUE`, include a `model_id` column indicating which
#'   equation set was used (`"DBH"` or `"DBHHT"`).
#'
#' @return A tibble with AGB components: `Bwood`, `Bbark`, `Bstem`, `Bfoliage`,
#'   `Bbranches`, `Bcrown`, `Btotal`. If `keep_model_id = TRUE`, includes `model_id`.
#'
#' @export
#' @examples
#'
#' # Auto-selection (equation_set = "auto" by default):
#' # height missing -> DBH-only equations
#' agb_lambert_ung(
#'   DBH = 20,
#'   species = "PICE.MAR"
#' )
#'
#' # height provided -> DBH + height equations
#' agb_lambert_ung(
#'   DBH = 20,
#'   height = 17,
#'   species = "PICE.MAR"
#' )
#'
#' # Show which equation set was used (model_id appended as the last column)
#' agb_lambert_ung(
#'   DBH = 20,
#'   species = "PICE.MAR",
#'   keep_model_id = TRUE
#' )
#'
#' # Force a specific equation set
#' agb_lambert_ung(
#'   DBH = 20,
#'   height = 17,
#'   species = "PICE.MAR",
#'   equation_set = "dbh"
#' )
#'
#' # Multiple rows
#' trees <- tibble::tibble(
#'  tree_id = 1:4,
#'  DBH = c(22, 30, 18, 35),
#'  height = c(18, 24, NA, 27),
#'  species = c("PICE.GLA", "PICE.GLA", "ABIE.BAL", "PINU.BAN")
#' )
#'
#'trees |>
#'  dplyr::mutate(
#'    agb = agb_lambert_ung(
#'      DBH = DBH,
#'      height = height,
#'      species = species,
#'      keep_model_id = TRUE
#'    )
#'  ) |>
#'  unnest(agb)
agb_lambert_ung <- function(
  DBH,
  height = NA_real_,
  species = NA,
  equation_set = c("auto", "dbh", "dbh_height"),
  keep_model_id = FALSE
) {
  equation_set <- match.arg(equation_set)

  # Validate DBH and height *shape* + numeric sanity.
  # Height is allowed to be NA here (auto/dbh); "height required" is enforced later.
  .lu_validate_inputs(DBH, height)

  # Recycle DBH/height/species to a common length
  if (!is.character(species)) {
    species <- as.character(species)
  }
  n <- max(length(DBH), length(height), length(species))

  ok_len <- function(x) length(x) %in% c(1L, n)
  if (!ok_len(DBH) || !ok_len(height) || !ok_len(species)) {
    rlang::abort(
      "`DBH`, `height`, and `species` must have the same length, or be length 1."
    )
  }

  DBH <- rep_len(DBH, n)
  height <- rep_len(height, n)
  species <- rep_len(species, n)

  # Standardize species codes (vectorized; CTAE helper)
  species <- .lu_standardize_species(species)

  # Height is "usable" only if finite and > 0
  has_ht <- !is.na(height) & is.finite(height) & height > 0

  # Map user-facing equation_set to per-row model_id
  model_id <- switch(
    equation_set,
    "auto" = ifelse(has_ht, "DBHHT", "DBH"),
    "dbh" = rep("DBH", n),
    "dbh_height" = rep("DBHHT", n)
  )

  # If user forces height-based equations, require valid height for all rows
  if (identical(equation_set, "dbh_height") && any(!has_ht)) {
    rlang::abort(
      "`height` must be provided (>0 and finite) for equation_set = \"dbh_height\"."
    )
  }

  # Working tibble used for grouping; grouping ensures one coef lookup per (model_id, species)
  df <- tibble::tibble(
    .row_id = seq_len(n),
    DBH = DBH,
    height = height,
    species = species,
    model_id = model_id
  )

  out <- df |>
    dplyr::group_by(.data$model_id, .data$species) |>
    dplyr::group_modify(function(.x, .g) {
      mid <- .g$model_id[[1]]
      spp <- .g$species[[1]]

      coefs <- .lu_get_coefs_cached(model_id = mid, species = spp)

      comps <- if (identical(mid, "DBH")) {
        .lu_agb_components_dbh(DBH = .x$DBH, coefs = coefs)
      } else {
        .lu_agb_components_dbhht(
          DBH = .x$DBH,
          height = .x$height,
          coefs = coefs
        )
      }

      # keep row id so we can restore original order after grouping
      dplyr::bind_cols(.x[".row_id"], comps)
    }) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$.row_id)

  # Drop grouping keys added by group_modify (species always; model_id depends on keep_model_id)
  if (!isTRUE(keep_model_id)) {
    out <- out |>
      dplyr::select(-"model_id", -"species", -".row_id")
  } else {
    out <- out |>
      dplyr::select(-"species", -".row_id") |>
      dplyr::relocate("model_id", .after = dplyr::last_col())
  }

  out
}
