#' Sharma (2022) site index model for black spruce and trembling aspen
#'
#' Implementation of the fixed-effects component of the no-climate
#' McDill-Amateis mixed-effects stand height model published by Sharma (2022)
#' for black spruce (\code{PICE.MAR}) and trembling aspen (\code{POPU.TRE})
#' growing in natural-origin mixed stands in Ontario.
#'
#' \strong{Species coverage:} \code{PICE.MAR}, \code{POPU.TRE}.
#'
#' \strong{Geographic use:} Ontario natural-origin mixed stands.
#'
#' \strong{Age definition note:} `age` is breast-height age (years).
#'
#' \strong{Height definition note:} the source model uses stand height
#' (top height) in metres.
#'
#' \strong{Base-age note:} the paper defines site index as stand height at
#' 50 years breast-height age. The underlying dynamic equation is base-age
#' invariant, so any positive `base_age` can be supplied; the default remains
#' 50 to match the source definition.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' This implementation uses the fixed-effects component of Sharma's
#' no-climate mixed-effects model (Equation 2), with stand-level random effects
#' set to zero.
#'
#' @param age Numeric vector. Breast-height age (years), with `age > 0`.
#' @param height Optional numeric vector. Stand height (m). If provided, `si`
#'   is predicted.
#' @param si Optional numeric vector. Site index (m) at `base_age` years
#'   breast-height age. If provided, `height` is predicted.
#' @param species Character vector of species codes (e.g., `"PICE.MAR"` or
#'   `"POPU.TRE"`).
#' @param base_age Positive numeric scalar. Site-index base age (years at
#'   breast height). Defaults to `50`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted stand height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Sharma, M. (2022). Climate effects on black spruce and trembling aspen
#' productivity in natural origin mixed stands. Forests, 13(3), 430.
#' https://doi.org/10.3390/f13030430
#'
#' @examples
#' # Predict site index from age + height
#' si_sharma2022(
#'   age = c(40, 60),
#'   height = c(13, 18),
#'   species = c("PICE.MAR", "POPU.TRE")
#' )
#'
#' # Predict height from age + site index
#' si_sharma2022(
#'   age = c(40, 60),
#'   si = c(15, 18),
#'   species = c("PICE.MAR", "POPU.TRE")
#' )
#'
#' @export
si_sharma2022 <- function(
  age,
  height = NULL,
  si = NULL,
  species,
  base_age = 50
) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }
  if (
    !is.numeric(base_age) ||
      length(base_age) != 1L ||
      is.na(base_age) ||
      !is.finite(base_age) ||
      base_age <= 0
  ) {
    cli::cli_abort("{.arg base_age} must be a single finite numeric value > 0.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .sharma2022_prepare(
    age = age,
    x = x,
    x_name = x_name,
    species = species,
    base_age = as.numeric(base_age)
  )

  if (mode == "predict_height") {
    h <- .mcdill_amateis_height(
      age = df$age,
      si = df$si,
      base_age = df$base_age,
      a0 = df$a0,
      a1 = df$a1
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_sharma2022}.",
        "i" = "Check inputs and model domain."
      ))
    }
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_sharma2022}.",
        "i" = "Check inputs and model domain."
      ))
    }

    return(dplyr::tibble(height = h))
  }

  si_est <- .mcdill_amateis_si(
    age = df$age,
    height = df$height,
    base_age = df$base_age,
    a0 = df$a0,
    a1 = df$a1
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_sharma2022}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_sharma2022}.",
      "i" = "Check inputs and model domain."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.sharma2022_prepare <- function(age, x, x_name, species, base_age) {
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

  pars <- .sharma2022_parameters()

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std,
    base_age = base_age
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$a0) || anyNA(out$a1)) {
    bad <- unique(out$Species[is.na(out$a0) | is.na(out$a1)])
    cli::cli_abort(
      paste0(
        "No Sharma2022 parameters found for species: ",
        paste(bad, collapse = ", "),
        "."
      )
    )
  }

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
  }

  out
}


# internal
.sharma2022_parameters <- function() {
  pars <- .get_internal_data("parameters_Sharma2022") |>
    dplyr::as_tibble()

  req <- c("Species", "a0", "a1")
  assert_required_cols(pars, req, object = "parameters_Sharma2022")

  pars
}
