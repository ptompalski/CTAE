#' Get merchantability criteria for a jurisdiction/species (BEC-aware for BC)
#'
#' Convenience helper to fetch CBM-style merchantability criteria (stump height,
#' top diameter, and minimum DBH) from the internal `merchcrit` lookup table.
#'
#' Before lookup, inputs are standardized using internal helpers:
#' * `standardize_jurisdiction_code()` for `jurisdiction`
#' * `standardize_species_code()` for `species` (keeps `"ALL"` as-is)
#'
#' Rules:
#' * For British Columbia (`"BC"`), `species` is OPTIONAL:
#'   - If `species` is missing, the function falls back to `Species == "ALL"` and warns.
#'   - If `BEC_zone` is provided and can be mapped, criteria are looked up using
#'     (Province + Species + BEC_group).
#'   - If `BEC_zone` is missing/unknown, criteria fall back to a conservative BC
#'     BEC-independent layer using `BEC_group == "UNKNOWN"` (species-specific if present,
#'     otherwise `Species == "ALL"`).
#'   - If no exact species match is found, the function falls back to genus-level rules
#'     (e.g., `PICE.GLA` -> `PICE.SPP`) and then to `Species == "ALL"`.
#' * For other jurisdictions, criteria are jurisdiction-level (Province) only and
#'   species/BEC are ignored.
#'
#' Stump height is returned in metres (converted from centimetres).
#'
#' @param jurisdiction Province/Territory code (e.g., `"BC"`, `"AB"`).
#' @param species Species code. Optional for `"BC"`; if missing, falls back to `"ALL"` with a warning.
#' @param BEC_zone Optional BEC zone code (e.g., `"CWH"`, `"ICH"`). If missing for `"BC"`,
#'   conservative criteria are taken from `BEC_group == "UNKNOWN"`.
#' @param verbose If TRUE, warns when BC falls back to genus-level, UNKNOWN BEC, or ALL.
#'
#' @return A one-row tibble with columns:
#'   `jurisdiction`, `species`, `stumpht_m`, `topdbh_cm`, `mindbh_cm`.
#'
#' @examples
#' get_merch_criteria("ON")
#' get_merch_criteria("PEI")                        # standardized to "PE"
#' get_merch_criteria("BC", "PSEU.MEN", "CWH")       # BEC-aware
#' get_merch_criteria("BC", "PICE.GLA", "CWH")       # genus fallback to PICE.SPP if present
#' get_merch_criteria("BC")                          # falls back to BC ALL + UNKNOWN (warns)
#'
#' @export
get_merch_criteria <- function(
  jurisdiction,
  species = NA_character_,
  BEC_zone = NA_character_,
  verbose = TRUE
) {
  stopifnot(
    is.character(jurisdiction),
    length(jurisdiction) == 1,
    nzchar(jurisdiction)
  )

  jurisdiction_std <- standardize_jurisdiction_code(jurisdiction)

  species_std <- species
  if (!is.na(species_std) && nzchar(species_std)) {
    species_std <- standardize_species_code(species_std)
  }

  # ---- internal data ----
  merchcrit <- .get_merchcrit_tbl()

  required <- c(
    "Province",
    "Species",
    "BEC_group",
    "StumpHT",
    "TopDBH",
    "MinDBH"
  )
  missing_cols <- setdiff(required, names(merchcrit))
  if (length(missing_cols) > 0) {
    stop(
      "Internal dataset `merchcrit` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  is_bc <- identical(jurisdiction_std, "BC")

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

  bec_to_group <- function(z) {
    if (is.na(z) || !nzchar(z)) {
      return(NA_character_)
    }
    z <- toupper(trimws(z))

    if (z %in% c("CWH", "MH")) {
      return("Coast_wet")
    }
    if (z %in% c("CDF")) {
      return("Coast_dry")
    }
    if (z %in% c("ICH", "SBS")) {
      return("Interior_wet")
    }
    if (z %in% c("IDF", "PP", "BG", "MS")) {
      return("Interior_dry")
    }
    if (z %in% c("ESSF", "AT", "SWB", "IMA", "CMA")) {
      return("High_elevation")
    }

    # unknown code -> NA; will trigger UNKNOWN fallback in BC logic
    NA_character_
  }

  # ---- Non-BC ----
  if (!is_bc) {
    has_species_rows <- any(
      merchcrit$Province == jurisdiction_std &
        !is.na(merchcrit$Species) &
        merchcrit$Species != "ALL",
      na.rm = TRUE
    )

    spp <- species_to_spp(species_std)

    candidates <- if (!is.na(species_std) && nzchar(species_std)) {
      unique(stats::na.omit(c(species_std, spp, "ALL")))
    } else {
      "ALL"
    }

    if (!has_species_rows) {
      candidates <- "ALL"
    }

    hits <- merchcrit[0, , drop = FALSE]
    hit_cand <- NA_character_

    for (cand in candidates) {
      tmp <- merchcrit[
        merchcrit$Province == jurisdiction_std & merchcrit$Species == cand,
        ,
        drop = FALSE
      ]
      if (nrow(tmp) > 0) {
        hits <- tmp
        hit_cand <- cand
        break
      }
    }

    # Legacy-friendly fallback
    if (nrow(hits) == 0) {
      hits <- merchcrit[merchcrit$Province == jurisdiction_std, , drop = FALSE]
      hit_cand <- NA_character_
    }

    if (nrow(hits) == 0) {
      stop(
        sprintf(
          "No merchantability criteria found for jurisdiction '%s'.",
          jurisdiction_std
        ),
        call. = FALSE
      )
    }

    if (nrow(hits) > 1) {
      stop(
        sprintf(
          "Multiple merchantability rows found for '%s' (match='%s'). Make `merchcrit` unique.",
          jurisdiction_std,
          ifelse(is.na(hit_cand), "<province-only>", hit_cand)
        ),
        call. = FALSE
      )
    }

    # Warnings (tuned): warn only if we fell back to ALL (not genus)
    if (
      isTRUE(verbose) &&
        has_species_rows &&
        !is.na(species_std) &&
        nzchar(species_std) &&
        identical(hit_cand, "ALL")
    ) {
      warning(
        sprintf(
          "No species/genus match for '%s' in %s; using fallback Species='ALL'.",
          species_std,
          jurisdiction_std
        ),
        call. = FALSE
      )
    }

    hit <- hits[1, , drop = FALSE]

    return(tibble::tibble(
      jurisdiction = jurisdiction_std,
      species = if (!is.na(species_std) && nzchar(species_std)) {
        species_std
      } else {
        "ALL"
      },
      stumpht_m = as.numeric(hit$StumpHT) / 100,
      topdbh_cm = as.numeric(hit$TopDBH),
      mindbh_cm = as.numeric(hit$MinDBH)
    ))
  }

  # ---- BC ----
  bec_group <- bec_to_group(BEC_zone)
  bec_group_eff <- if (!is.na(bec_group) && nzchar(bec_group)) {
    bec_group
  } else {
    "UNKNOWN"
  }

  # If species missing in BC, fall back to ALL (warn)
  if (is.na(species_std) || !nzchar(species_std)) {
    if (isTRUE(verbose)) {
      warning(
        "For jurisdiction 'BC', `species` was not provided; using BC fallback Species='ALL'.",
        call. = FALSE
      )
    }
    species_std <- "ALL"
  }

  spp <- species_to_spp(species_std)
  candidates <- unique(stats::na.omit(c(species_std, spp, "ALL")))

  lookup_once <- function(bg) {
    for (cand in candidates) {
      hit <- merchcrit[
        merchcrit$Province == "BC" &
          merchcrit$BEC_group == bg &
          merchcrit$Species == cand,
        ,
        drop = FALSE
      ]
      if (nrow(hit) > 0) return(list(hit = hit, bg = bg, cand = cand))
    }
    list(hit = merchcrit[0, , drop = FALSE], bg = bg, cand = NA_character_)
  }

  res <- lookup_once(bec_group_eff)

  # If mapped BEC group failed, try UNKNOWN last
  if (nrow(res$hit) == 0 && !identical(bec_group_eff, "UNKNOWN")) {
    res2 <- lookup_once("UNKNOWN")
    if (nrow(res2$hit) > 0) res <- res2
  }

  if (nrow(res$hit) == 0) {
    stop(
      sprintf(
        paste0(
          "No merchantability criteria found for BC with species='%s' and BEC_group='%s'. ",
          "Tried candidates: %s. Ensure `merchcrit` includes at least ('BC','ALL','UNKNOWN')."
        ),
        species_std,
        bec_group_eff,
        paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (nrow(res$hit) > 1) {
    stop(
      sprintf(
        "Multiple merchantability rows found for BC (BEC_group='%s', match='%s'). Make `merchcrit` unique.",
        res$bg,
        res$cand
      ),
      call. = FALSE
    )
  }

  hit <- res$hit[1, , drop = FALSE]

  # Warnings (tuned): warn on UNKNOWN BEC, and on ALL fallback (not genus)
  if (isTRUE(verbose)) {
    msgs <- character(0)

    # BEC warnings: only when UNKNOWN is used
    if (identical(res$bg, "UNKNOWN")) {
      if (is.na(BEC_zone) || !nzchar(BEC_zone)) {
        msgs <- c(
          msgs,
          "BEC_zone missing; using conservative BC 'UNKNOWN' utilization."
        )
      } else {
        msgs <- c(
          msgs,
          sprintf(
            "BEC_zone '%s' could not be mapped; using conservative BC 'UNKNOWN' utilization.",
            BEC_zone
          )
        )
      }
    }

    # ALL fallback warning (genus fallback is intentionally silent)
    if (identical(res$cand, "ALL") && !identical(species_std, "ALL")) {
      msgs <- c(
        msgs,
        sprintf(
          "No species/genus match for '%s'; using BC fallback Species='ALL'.",
          species_std
        )
      )
    }

    if (length(msgs) > 0) warning(paste(msgs, collapse = " "), call. = FALSE)
  }

  tibble::tibble(
    jurisdiction = "BC",
    species = species_std,
    stumpht_m = as.numeric(hit$StumpHT) / 100,
    topdbh_cm = as.numeric(hit$TopDBH),
    mindbh_cm = as.numeric(hit$MinDBH)
  )
}

# Internal helper to access merch criteria table from namespace/data.
.get_merchcrit_tbl <- function() {
  ns <- asNamespace("CanadaForestAllometry")
  if (exists("merchcrit", envir = ns, inherits = FALSE)) {
    return(get("merchcrit", envir = ns, inherits = FALSE))
  }
  CanadaForestAllometry::merchcrit
}
