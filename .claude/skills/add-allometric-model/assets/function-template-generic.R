# Generic model template (agb_, ytbh_, or a new family).
#
# Adapt the shape to the model's actual inputs/outputs. Keep the conventions:
# vectorized with length recycling, standardized species/jurisdiction codes,
# early cli/rlang validation, snake_case tibble output, full roxygen block.

#' <One-line title: Author (Year) <what it estimates> for <region/species>>
#'
#' <Description of the model and its scope.>
#'
#' @param <input1> Numeric vector. <meaning> (<units>).
#' @param species Character vector of NFI species codes (e.g., `"PICE.MAR"`).
#' @param <other args> ...
#'
#' @return A tibble with column(s) <name(s)> (<units>).
#'
#' @references
#' <full citation>
#'
#' @examples
#' <fn_name>(<...>)
#'
#' @export
<fn_name> <- function(<input1>, species, ...) {
  # ---- validate + recycle to common length ----
  # n <- max(lengths...); check compatibility; rep(..., length.out = n);
  # else cli::cli_abort("... must have compatible lengths ...")

  # ---- standardize species / jurisdiction codes ----
  # species_std <- standardize_species_code(species)

  # ---- fetch parameters from internal data; join per species/variant ----

  # ---- evaluate the model form faithfully to the source ----
  # Abort on non-finite / out-of-domain results with an informative message.

  # ---- return a snake_case tibble ----
  dplyr::tibble(<output> = numeric()) # replace with computed vector
}
