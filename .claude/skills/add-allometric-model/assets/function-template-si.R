#' <Author> (<Year>) site index model for <region/species>
#'
#' Unified, vectorized implementation of the site index / height model in
#' <full citation>.
#'
#' \strong{Model scope (species coverage):} <list NFI codes>.
#'
#' \strong{Age definition note:} `age` is <breast-height | total> age (years).
#'
#' \strong{Base-age note:} site index is referenced to height at <base age>.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' @param age Numeric vector. <Breast-height | total> age (years).
#' @param height Optional numeric vector. Height (m). If provided, `si` is predicted.
#' @param si Optional numeric vector. Site index (m, base age <X>). If provided,
#'   `height` is predicted.
#' @param species Character vector of NFI species codes (e.g., `"PINU.CON"`).
#'
#' @return A tibble with columns `height` and/or `si` (m).
#'
#' @references
#' <full citation>
#'
#' @examples
#' <fn_name>(age = c(25, 50), height = c(10, 20), species = c("PINU.CON", "PICE.GLA"))
#'
#' @export
<fn_name> <- function(age, height = NULL, si = NULL, species) {
  # Exactly one of height / si must be supplied.
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"

  # --- validate + recycle inputs to a common length ---
  # (see si_thrower1994.R for a full .prepare() helper; standardize species codes
  #  via standardize_species_code(), join parameters, error on incompatible lengths)

  # --- compute per element from the transcribed model form ---
  # Keep the numerical form faithful to the source. Use internal .<model>_*_one()
  # helpers for clarity where the math is involved. Abort on non-finite / negative
  # predictions with a cli::cli_abort message that names the function.

  # --- return a snake_case tibble ---
  if (mode == "predict_height") {
    dplyr::tibble(height = numeric()) # replace with computed vector
  } else {
    dplyr::tibble(si = numeric()) # replace with computed vector
  }
}
