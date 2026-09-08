#' Carmean and Hahn (1981) site index model
#'
#' Unified, vectorized implementation of the Carmean and Hahn (1981)
#' site-index equations for Lake States balsam fir and white spruce.
#'
#' \strong{Model scope (species coverage):} this implementation includes
#' parameter sets for 2 species: \code{ABIE.BAL, PICE.GLA}.
#'
#' \strong{Geographic use:} this model was fit to revised Lake States
#' harmonized curves and should be used with caution outside that domain.
#'
#' \strong{Age definition note:} `age` is \emph{total age} (years), not
#' breast-height age.
#'
#' \strong{Base-age note:} site index in this model is total height at
#' 50 years total age.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' Inputs/outputs are metric; the original equations are in imperial units, so
#' the function converts internally.
#'
#' @param age Numeric vector. Total age (years).
#' @param height Optional numeric vector. Total tree height (m). If provided, `si` is predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at total age). If provided, `height` is predicted.
#' @param species Character vector of species codes (e.g., `"ABIE.BAL"`).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Carmean, W. H., & Hahn, J. T. (1981). Revised site index curves for balsam
#' fir and white spruce in the Lake States. U.S. Department of Agriculture,
#' Forest Service, North Central Forest Experiment Station.
#'
#' @examples
#' # Predict site index from age + height
#' si_carmeanhahn1981(
#'   age = c(30, 50, 70),
#'   height = c(8, 15, 22),
#'   species = c("ABIE.BAL", "ABIE.BAL", "PICE.GLA")
#' )
#'
#' # Predict height from age + site index
#' si_carmeanhahn1981(
#'   age = c(30, 50, 70),
#'   si = c(12, 16, 20),
#'   species = c("ABIE.BAL", "ABIE.BAL", "PICE.GLA")
#' )
#'
#' @export
si_carmeanhahn1981 <- function(age, height = NULL, si = NULL, species) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .carmeanhahn1981_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name
  )

  if (mode == "predict_height") {
    h_ft <- with(
      df,
      height_b1 *
        (si_ft^height_b2) *
        (1 - exp(height_b3 * age))^(height_b4 * (si_ft^height_b5))
    )

    if (any(!is.finite(h_ft))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_carmeanhahn1981}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    # Unreachable with validated domain: all multiplicative terms are positive,
    # so predicted height cannot be negative.
    # nocov start
    if (any(h_ft < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_carmeanhahn1981}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    # nocov end

    return(dplyr::tibble(height = h_ft / 3.28084))
  }

  si_ft <- with(
    df,
    si_b1 *
      (height_ft^si_b2) *
      (1 - exp(si_b3 * age))^(si_b4 * (height_ft^si_b5))
  )

  if (any(!is.finite(si_ft))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_carmeanhahn1981}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  # Unreachable with validated domain: all multiplicative terms are positive,
  # so predicted site index cannot be negative.
  # nocov start
  if (any(si_ft < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_carmeanhahn1981}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  # nocov end

  dplyr::tibble(si = si_ft / 3.28084)
}

# internal
.carmeanhahn1981_prepare <- function(age, x, species, x_name) {
  n <- max(length(age), length(x), length(species))
  if (n == 0L) {
    cli::cli_abort("{.arg age} must have length > 0.")
  }

  recycled <- assert_len_compat(
    age = age,
    x = x,
    species = species,
    .n = n,
    .recycle = TRUE
  )
  age <- recycled$age
  x <- recycled$x
  species <- recycled$species

  assert_numeric_vec(age, "age", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 0, allow_na = FALSE)

  species_std <- standardize_species_code(species)

  pars <- .carmeanhahn1981_parameters() |>
    dplyr::distinct(.data$Species, .keep_all = TRUE)

  req <- c(
    "Species",
    "height_b1",
    "height_b2",
    "height_b3",
    "height_b4",
    "height_b5",
    "si_b1",
    "si_b2",
    "si_b3",
    "si_b4",
    "si_b5"
  )
  assert_required_cols(pars, req, object = "CarmeanHahn1981_parameters")

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$height_b1) || anyNA(out$si_b1)) {
    bad <- unique(out$Species[is.na(out$height_b1) | is.na(out$si_b1)])
    cli::cli_abort(
      "No CarmeanHahn1981 parameters found for species: {paste(bad, collapse = ', ')}."
    )
  }

  if (identical(x_name, "height")) {
    out$height_ft <- out$x * 3.28084
  } else {
    out$si_ft <- out$x * 3.28084
  }

  out
}

# internal
.carmeanhahn1981_parameters <- function() {
  pars <- .get_internal_data("parameters_CarmeanHahn1981") |>
    dplyr::as_tibble()

  req <- c(
    "Species",
    "height_b1",
    "height_b2",
    "height_b3",
    "height_b4",
    "height_b5",
    "si_b1",
    "si_b2",
    "si_b3",
    "si_b4",
    "si_b5"
  )
  assert_required_cols(pars, req, object = "parameters_CarmeanHahn1981")

  pars
}
