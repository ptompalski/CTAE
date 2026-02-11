#' Get volume-model parameters from the CanadaForestAllometry registry
#'
#' Retrieves the parameter table for a given volume model using
#' \code{\link{volume_model_registry}} and the parameter objects bundled with the
#' CanadaForestAllometry package (e.g., \code{parameters_NationalTaperModelsDBH}).
#'
#' The function then applies **context filters** (when relevant / available in the
#' parameter table), typically by \code{species}, and for regional models also by
#' \code{province} and \code{subregion}.
#'
#' This function does **not** refactor or run any volume engines; it only returns
#' the parameter rows needed for a given model/context.
#'
#' @param model_id A single model id from \code{volume_model_registry()$model_id},
#'   e.g. \code{"national_ung_dbh"} or \code{"regional_kozak88"}.
#' @param species Species code (e.g., \code{"POPU.TRE"}). Required for all current models.
#' @param province Province/jurisdiction code (e.g., \code{"BC"}, \code{"AB"}).
#'   Used for regional models when the parameter table includes a \code{Province} column.
#' @param subregion Subregion identifier (BEC zone in BC).
#'   Used for regional models when the parameter table includes a \code{Subregion} column.
#' @param strict Logical. If \code{TRUE}, throw an error when no matching parameters
#'   are found. If \code{FALSE} (default), returns an empty tibble.
#'
#' @return A tibble containing the filtered parameter rows for the requested model.
#'   If no rows match and \code{strict = FALSE}, returns an empty tibble with the
#'   same columns as the underlying parameter table.
#'
#' @examples
#' # National DBH-only (Ung et al.)
#' # get_volume_params("national_ung_dbh", species = "POPU.TRE")
#'
#' # Regional Kozak 88 (requires province/subregion in most parameter tables)
#' # get_volume_params("regional_kozak88", species = "POPU.TRE", province = "AB", subregion = "ALL")
#'
#' # BC Kozak 94 (BEC zone as subregion)
#' # get_volume_params("regional_kozak94", species = "PSEU.MEN", province = "BC", subregion = "SBS")
#'
#' @export
get_volume_params <- function(
  model_id,
  species,
  province = NA_character_,
  subregion = "ALL",
  strict = FALSE
) {
  stopifnot(length(model_id) == 1, is.character(model_id), nzchar(model_id))
  stopifnot(length(species) == 1, is.character(species), nzchar(species))

  species_std <- standardize_species_code(species)

  # ---- Helpers ----
  species_to_spp <- function(sp) {
    if (
      is.na(sp) || !nzchar(sp) || identical(sp, "ALL") || grepl("\\.SPP$", sp)
    ) {
      return(NA_character_)
    }
    if (!grepl("^[A-Z]{4}\\.[A-Z]{3}$", sp)) {
      return(NA_character_)
    }
    paste0(substr(sp, 1, 4), ".SPP")
  }

  reg <- volume_model_registry()
  if (!model_id %in% reg$model_id) {
    stop(
      "Unknown `model_id`: '",
      model_id,
      "'. Valid values are: ",
      paste(reg$model_id, collapse = ", ")
    )
  }

  row <- reg[reg$model_id == model_id, , drop = FALSE]
  params_key <- row$params_key[[1]]

  params <- get(params_key, envir = environment(), inherits = TRUE)

  if (!is.data.frame(params)) {
    stop("Parameter object '", params_key, "' is not a data.frame/tibble.")
  }

  # Start filtering
  out <- params

  # ---- Species filter with fallback ----
  if (!"Species" %in% names(out)) {
    stop(
      "Parameter object '",
      params_key,
      "' must contain a `Species` column. Found: ",
      paste(names(out), collapse = ", ")
    )
  }

  spp <- species_to_spp(species_std)
  candidates <- unique(stats::na.omit(c(species_std, spp)))

  out0 <- out[0, , drop = FALSE]
  for (cand in candidates) {
    tmp <- out[out$Species == cand, , drop = FALSE]
    if (nrow(tmp) > 0) {
      out0 <- tmp
      break
    }
  }
  out <- out0

  # Province filter (only if present & provided)
  if ("Province" %in% names(out) && !is.na(province) && nzchar(province)) {
    out <- out[out$Province == province, , drop = FALSE]
  }

  # Subregion filter (only if present & provided)
  if (
    "Subregion" %in%
      names(out) &&
      !is.null(subregion) &&
      !is.na(subregion) &&
      nzchar(subregion)
  ) {
    out <- out[out$Subregion == subregion, , drop = FALSE]
  }

  out <- tibble::as_tibble(out)

  if (nrow(out) == 0) {
    if (isTRUE(strict)) {
      msg <- paste0(
        "No parameters found for model_id='",
        model_id,
        "', species='",
        species_std,
        "'",
        if (!is.na(province) && nzchar(province)) {
          paste0(", province='", province, "'")
        } else {
          ""
        },
        if (!is.null(subregion) && !is.na(subregion) && nzchar(subregion)) {
          paste0(", subregion='", subregion, "'")
        } else {
          ""
        },
        ". Tried species candidates: ",
        paste(candidates, collapse = ", "),
        ". (params_key='",
        params_key,
        "')"
      )
      stop(msg)
    }

    return(tibble::as_tibble(params[0, , drop = FALSE]))
  }

  out
}

# get_volume_params("national_ung_dbh", species = "POPU.TRE")
# get_volume_params("regional_honer", species = "PICE.GLA", strict = T)
# get_volume_params("regional_kozak88", species = "POPU.TRE", province = "AB", subregion = "ALL")
# get_volume_params("regional_kozak94", species = "PSEU.MEN", province = "BC", subregion = "CWH")
