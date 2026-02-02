#' Calculate tree volume: choose best available model or run all candidate models
#'
#' `vol()` is the main convenience wrapper for estimating total and
#' merchantable stem volume from diameter and height. It automatically
#' selects and runs one (or more) of the volume models implemented in CTAE,
#' based on the model registry returned by [volume_model_registry()].
#'
#' @section Model selection logic:
#' Model selection is driven by the model registry and occurs per input tree:
#' \enumerate{
#'   \item Geographic applicability: models are filtered to those whose
#'     `province_scope` includes the provided `jurisdiction` (or `"ALL"`).
#'   \item Species coverage: models are filtered to those that include the
#'     provided `species` in their parameter tables. Some models may include
#'     genus/group codes (e.g., `"PICE.SPP"`); these are treated
#'     as covering all matching species within that genus/group.
#'   \item Required inputs: models that require total height are excluded
#'     when `height` is missing. Models that require a subregion (e.g., BEC zone
#'     for certain provincial models) are excluded when `subregion` is missing.
#'   \item Model ranking: if multiple eligible models remain, `pick_best = TRUE`
#'     selects the highest-ranked model (`rank`) for each tree.
#' }
#'
#' @section Fallback behavior:
#' Availability is evaluated at the species level, not only by jurisdiction.
#' This means a regional/provincial model may exist for a jurisdiction but may
#' not include coefficients for the provided species. In such cases, `vol()`
#' automatically falls back to another eligible model (e.g., a multi-province
#' regional model or a national model) that *does* cover the species and required
#' inputs.
#'
#' @section Important limitations and recommended use:
#' \itemize{
#'   \item This is an automatic wrapper. Because selection is registry-driven,
#'     `vol()` may use a model that is not the one you intended (e.g., a national
#'     model instead of a regional model, or a different regional model than you
#'     expected), especially when the preferred model is not available for the
#'     input species or missing required inputs.
#'   \item Model outputs differ. Different models will generally return
#'     slightly different volume estimates for the same DBH/height/species due to
#'     differences in model form, fitted data, geographic scope, and
#'     merchantability assumptions. These differences are expected.
#'   \item If you require a specific model, do not use `vol()`. Instead, call
#'     the relevant model function directly (e.g., `vol_huang94()`, `vol_kozak94()`,
#'     `vol_ung2013()`, etc.) to ensure full control and reproducibility.
#'   \item Investigate before operational use. It is strongly recommended to
#'     run `vol()` with `pick_best = FALSE` during exploratory analyses to inspect
#'     which models are eligible and how sensitive results are to model choice.
#'     When `pick_best = FALSE`, `vol()` returns a list-column (one element per
#'     tree) containing a tibble of candidate model outputs; this can be unnested
#'     to compare models.
#' }
#'
#' @param DBH Numeric vector. Diameter at breast height (cm).
#' @param height Numeric vector. Total height (m). Can be `NA` to use the DBH-only models (currently only `vol_ung2013()`).
#' @param species Character vector. Species codes (e.g., "PICE.GLA").
#' @param jurisdiction Character scalar or vector. Province/territory code (e.g., "BC", "QC").
#' @param subregion Optional character vector. Subregion identifier (e.g., BEC zone for `vol_kozak94()`).
#' @param pick_best Logical. If `TRUE` (default), pick the single best model per tree using `rank`.
#'   If `FALSE`, compute all candidate model outputs per tree and return a list-column
#'   (one tibble per tree) that can be unnested for comparison.
#' @param keep_model_id Logical. If `TRUE`, include a `vol_model` column with the engine function name used.
#'
#' @return
#' If `pick_best = TRUE`, a tibble with one row per tree and columns:
#' `vol_total`, `vol_merchantable`, and optionally `vol_model`.
#'
#' If `pick_best = FALSE`, a list (suitable for use as a list-column in `dplyr::mutate()`)
#' with one element per tree. Each element is a tibble of candidate model outputs
#' (`vol_total`, `vol_merchantable`, and optionally `vol_model`).
#' @examples
#' ## --- Basic usage: pick best available model (default) ---
#' vol(
#'   DBH = 20,
#'   height = 20,
#'   species = "PICE.MAR",
#'   jurisdiction = "ON"
#' )
#'
#' ## --- Return all applicable models (one row per engine) ---
#' ## Note: national Ung DBH-only and DBH+HT variants are collapsed
#' ## into a single result (preferring the height-based variant).
#' vol(
#'   DBH = 20,
#'   height = 20,
#'   species = "PICE.MAR",
#'   jurisdiction = "ON",
#'   pick_best = FALSE,
#'   keep_model_id = TRUE
#' )
#'
#' ## --- Missing height: height-dependent models are skipped ---
#' ## Falls back to DBH-only models where available (e.g. national Ung).
#' vol(
#'   DBH = 25,
#'   species = "BETU.PAP",
#'   jurisdiction = "QC"
#' )
#'
#' ## --- Regional model with required subregion (BC example) ---
#' ## Kozak (1994) requires a BEC zone; if provided, it is preferred.
#' vol(
#'   DBH = 30,
#'   height = 22,
#'   species = "THUJ.PLI",
#'   jurisdiction = "BC",
#'   subregion = "CWH"
#' )
#'
#' ## --- Automatic fallback when a regional model does not cover species ---
#' ## If no provincial model covers the species, a suitable national or
#' ## multi-province regional model is used instead.
#' vol(
#'   DBH = 18,
#'   height = 15,
#'   species = "ACER.SAC",
#'   jurisdiction = "MB"
#' )
#'
#' ## --- Vectorized input ---
#' vol(
#'   DBH = c(18, 25, 32),
#'   height = c(15, 20, 25),
#'   species = c("PICE.MAR", "BETU.PAP", "ACER.RUB"),
#'   jurisdiction = "ON",
#'   keep_model_id = TRUE
#' )
#'
#' #' ## --- Tidyverse workflow with multiple species and jurisdictions ---
#' library(dplyr)
#'
#' trees <- tibble::tibble(
#'   DBH = c(18, 22, 30, 26),
#'   height = c(15, 18, 22, 20),
#'   species = c("PICE.MAR", "BETU.PAP", "POPU.TRE", "PSEU.MEN"),
#'   jurisdiction =  c("AB", "ON", "QC", "BC"),
#'   subregion = c(NA, NA, NA, "CWH")
#' )
#'
#' trees |>
#'   dplyr::mutate(
#'     vol(
#'       DBH = DBH,
#'       height = height,
#'       species = species,
#'       jurisdiction = jurisdiction,
#'       subregion = subregion,
#'       keep_model_id = TRUE
#'     )
#'   )
#'
#' ## --- Same data, return all applicable volume models per tree ---
#' trees |>
#'   dplyr::mutate(
#'     vol(
#'       DBH = DBH,
#'       height = height,
#'       species = species,
#'       jurisdiction = jurisdiction,
#'       subregion = subregion,
#'       pick_best = FALSE,
#'       keep_model_id = TRUE
#'     )
#'   )
#'
#'
#'
#' @export
vol <- function(
  DBH,
  height = NA_real_,
  species,
  jurisdiction,
  subregion = NA_character_,
  pick_best = TRUE,
  keep_model_id = TRUE
) {
  # ---- base recycling helper ----
  recycle_to <- function(x, n, arg = deparse(substitute(x))) {
    if (length(x) == 1L) {
      return(rep(x, n))
    }
    if (length(x) == n) {
      return(x)
    }
    stop(
      sprintf("`%s` must have length 1 or %d (got %d).", arg, n, length(x)),
      call. = FALSE
    )
  }

  n <- length(DBH)
  if (n == 0L) {
    stop("`DBH` must have length > 0.", call. = FALSE)
  }

  height <- recycle_to(height, n, "height")
  species <- recycle_to(species, n, "species")
  jurisdiction <- recycle_to(jurisdiction, n, "jurisdiction")
  subregion <- recycle_to(subregion, n, "subregion")

  # ---- standardize inputs ----
  jurisdiction_std <- purrr::map_chr(
    jurisdiction,
    standardize_jurisdiction_code
  )
  species_std <- purrr::map_chr(species, standardize_species_code)

  x <- tibble::tibble(
    .row_id = seq_len(n),
    DBH = DBH,
    height = height,
    species_in = species_std,
    jurisdiction = jurisdiction_std,
    subregion = subregion
  )

  reg <- volume_model_registry_species() |>
    dplyr::mutate(
      species_cov = .data$species, # rename to avoid clash after crossing
      species = NULL
    )

  # ---- ensure operational subregion columns exist (backward compatible) ----
  if (!"subregion_required" %in% names(reg)) {
    reg <- reg |>
      dplyr::mutate(
        subregion_required = dplyr::if_else(
          "subregion_type" %in%
            names(reg) &
            !is.na(.data$subregion_type) &
            grepl("required", .data$subregion_type, ignore.case = TRUE),
          TRUE,
          FALSE
        )
      )
  }
  if (!"subregion_arg" %in% names(reg)) {
    reg <- reg |> dplyr::mutate(subregion_arg = NA_character_)
  }

  # ---- helpers ----
  species_is_covered <- function(sp, reg_species_vec) {
    if (length(reg_species_vec) == 0) {
      return(FALSE)
    }
    if (sp %in% reg_species_vec) {
      return(TRUE)
    }

    genus <- sub("\\..*$", "", sp)
    any(c(paste0(genus, ".SPP"), paste0(genus, ".ALL")) %in% reg_species_vec)
  }

  province_is_covered <- function(juris, prov_scope) {
    if (length(prov_scope) == 0) {
      return(FALSE)
    }
    if ("ALL" %in% prov_scope) {
      return(TRUE)
    }
    juris %in% prov_scope
  }

  subregion_ok <- function(subregion_value, subregion_required) {
    if (!isTRUE(subregion_required)) {
      return(TRUE)
    }
    !is.na(subregion_value) && nzchar(subregion_value)
  }

  # Only pass subregion if user provided it; otherwise let engine defaults apply
  build_engine_args <- function(
    subregion_arg,
    DBH,
    height,
    species,
    jurisdiction,
    subregion
  ) {
    args <- list(
      DBH = DBH,
      height = height,
      species = species,
      jurisdiction = jurisdiction
    )

    if (
      !is.na(subregion_arg) &&
        nzchar(subregion_arg) &&
        !is.na(subregion) &&
        nzchar(subregion)
    ) {
      args[[subregion_arg]] <- subregion
    }

    args
  }

  safe_call_engine_one <- function(engine, args) {
    fn <- get(engine, envir = parent.frame(), inherits = TRUE)

    keep <- intersect(names(args), names(formals(fn)))
    args2 <- args[keep]

    tryCatch(
      {
        out <- do.call(fn, args2)
        out <- dplyr::as_tibble(out)

        if (!all(c("vol_total", "vol_merchantable") %in% names(out))) {
          stop(sprintf("%s returned unexpected columns.", engine))
        }

        out
      },
      error = function(e) {
        # For pick_best=FALSE we will simply drop failing models;
        # for pick_best=TRUE failures are handled by trying other models.
        tibble::tibble(
          vol_total = NA_real_,
          vol_merchantable = NA_real_,
          .failed = TRUE
        )
      }
    )
  }

  # ---- candidates ----
  cand <- tidyr::crossing(x, reg) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .ok_prov = province_is_covered(.data$jurisdiction, .data$province_scope),
      .ok_sp = species_is_covered(.data$species_in, .data$species_cov),
      .ok_ht = !.data$requires_ht ||
        (!is.na(.data$height) & is.finite(.data$height)),
      .ok_sub = subregion_ok(.data$subregion, .data$subregion_required)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$.ok_prov, .data$.ok_sp, .data$.ok_ht, .data$.ok_sub) |>
    dplyr::select(
      .row_id,
      DBH,
      height,
      species_in,
      jurisdiction,
      subregion,
      model_id,
      engine,
      requires_ht,
      rank,
      subregion_arg
    )

  if (nrow(cand) == 0) {
    stop(
      "No eligible volume model found for the provided inputs.\n",
      "Check: species coverage, jurisdiction/province scope, and whether height/subregion are required.",
      call. = FALSE
    )
  }

  # Collapse multiple registry variants mapping to the same engine
  cand <- cand |>
    dplyr::group_by(.data$.row_id, .data$engine) |>
    dplyr::arrange(dplyr::desc(.data$requires_ht), dplyr::desc(.data$rank)) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  # Build args + evaluate
  res <- cand |>
    dplyr::mutate(
      .args = purrr::pmap(
        list(
          .data$subregion_arg,
          .data$DBH,
          .data$height,
          .data$species_in,
          .data$jurisdiction,
          .data$subregion
        ),
        build_engine_args
      ),
      .out = purrr::map2(.data$engine, .data$.args, safe_call_engine_one)
    ) |>
    tidyr::unnest(.out)

  # Drop failures (NA rows produced by safe_call_engine_one)
  if (".failed" %in% names(res)) {
    res <- res |>
      dplyr::filter(is.na(.data$.failed) | .data$.failed != TRUE) |>
      dplyr::select(-dplyr::any_of(".failed"))
  }

  if (isTRUE(pick_best)) {
    # choose best successful model per row
    best <- res |>
      dplyr::group_by(.data$.row_id) |>
      dplyr::slice_max(order_by = .data$rank, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        .data$.row_id,
        vol_total = .data$vol_total,
        vol_merchantable = .data$vol_merchantable,
        vol_model = if (isTRUE(keep_model_id)) .data$engine else NULL
      )

    # Ensure every row has a successful model
    missing <- setdiff(x$.row_id, best$.row_id)
    if (length(missing) > 0) {
      stop(
        "No volume model succeeded for at least one input row. ",
        "Try providing `height` and any required `subregion` (e.g., BEC zone in BC).",
        call. = FALSE
      )
    }

    out <- x |>
      dplyr::select(.row_id) |>
      dplyr::left_join(best, by = ".row_id") |>
      dplyr::arrange(.data$.row_id) |>
      dplyr::select(-.row_id)

    return(out)
  }

  # ---- pick_best = FALSE: return list-column-ready output ----
  # one tibble per tree (row), containing all successful model results for that tree
  out_list <- vector("list", n)
  split_res <- split(res, res$.row_id)

  for (i in seq_len(n)) {
    df <- split_res[[as.character(i)]]
    if (is.null(df) || nrow(df) == 0) {
      out_list[[i]] <- tibble::tibble(
        vol_total = numeric(0),
        vol_merchantable = numeric(0),
        vol_model = character(0)
      )[0, ]
    } else {
      out_list[[i]] <- df |>
        dplyr::transmute(
          vol_total = .data$vol_total,
          vol_merchantable = .data$vol_merchantable,
          vol_model = if (isTRUE(keep_model_id)) .data$engine else NULL
        )
    }
  }

  out_list
}
