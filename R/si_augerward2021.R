#' Auger and Ward (2021) site index model for Quebec plantations
#'
#' Implementation of the dominant-height difference equations reported by Auger
#' and Ward (2021) for jack pine plantations (\code{PINU.BAN}) and black
#' spruce plantations (\code{PICE.MAR}) in Quebec.
#'
#' \strong{Species coverage:} \code{PINU.BAN}, \code{PICE.MAR}.
#'
#' \strong{Geographic use:} Quebec plantations.
#'
#' \strong{Age definition note:} `age` is plantation age (years since planting),
#' not breast-height age.
#'
#' \strong{Height definition note:} the model uses dominant height (m) based on
#' the mean height of the 100 tallest planted trees per hectare.
#'
#' \strong{Base-age note:} the source document defines `IQS` at plantation age
#' 25 years. The equations are written as difference equations, so other
#' positive `base_age` values can also be used.
#'
#' \strong{Domain note:} the source recommends limiting use to plantation ages
#' up to 100 years. This implementation warns when `age > 100` and returns
#' extrapolated values.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' Inputs and outputs are metric and follow the source model scale directly.
#'
#' @param age Numeric vector. Plantation age (years), with `age > 0`. A warning
#'   is emitted when `age > 100`, because that exceeds the recommended range in
#'   the source document.
#' @param height Optional numeric vector. Dominant height (m). If provided,
#'   `si` is predicted.
#' @param si Optional numeric vector. Site index (m) at `base_age` years. If
#'   provided, `height` is predicted.
#' @param species Character vector of species codes (e.g., `"PINU.BAN"` or
#'   `"PICE.MAR"`).
#' @param base_age Positive numeric scalar. Site-index base age (years since
#'   planting). Defaults to `25`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted dominant height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Auger, I., & Ward, C. (2021). \emph{Tables de rendement pour les plantations
#' d'epinette noire et les plantations de pin gris au Quebec}. Avis technique
#' SSS-06. Gouvernement du Quebec. Version corrigee diffusee en 2024.
#'
#' @examples
#' si_augerward2021(
#'   age = c(20, 30),
#'   si = c(9, 12),
#'   species = c("PICE.MAR", "PINU.BAN")
#' )
#'
#' si_augerward2021(
#'   age = c(20, 30),
#'   height = c(7, 10),
#'   species = c("PICE.MAR", "PINU.BAN")
#' )
#'
#' @export
si_augerward2021 <- function(
  age,
  height = NULL,
  si = NULL,
  species,
  base_age = 25
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

  df <- .augerward2021_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name,
    base_age = as.numeric(base_age)
  )

  if (mode == "predict_height") {
    h <- .augerward2021_height(
      age = df$age,
      si = df$si,
      base_age = df$base_age,
      Species = df$Species
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_augerward2021}.",
        "i" = "Check inputs and model domain."
      ))
    }
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_augerward2021}.",
        "i" = "Check inputs and model domain."
      ))
    }

    return(dplyr::tibble(height = h))
  }

  si_est <- .augerward2021_si(
    age = df$age,
    height = df$height,
    base_age = df$base_age,
    Species = df$Species
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_augerward2021}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_augerward2021}.",
      "i" = "Check inputs and model domain."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.augerward2021_prepare <- function(age, x, species, x_name, base_age) {
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
  if (any(age > 100)) {
    cli::cli_warn(c(
      "{.arg age} contains values > 100 for {.fn si_augerward2021}.",
      "i" = "Predictions beyond 100 years are extrapolations outside the recommended plantation-age range."
    ))
  }

  species_std <- standardize_species_code(species)

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std,
    base_age = base_age
  )

  pars <- .augerward2021_parameters()
  out <- out |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$beta0) || anyNA(out$beta2) || anyNA(out$form)) {
    bad <- unique(out$Species[
      is.na(out$beta0) | is.na(out$beta2) | is.na(out$form)
    ])
    cli::cli_abort(
      "No AugerWard2021 parameters found for species: {paste(bad, collapse = ', ')}."
    )
  }

  asymptote_species <- out$Species == "PICE.MAR"
  if (any(asymptote_species & out$x >= out$beta0)) {
    cli::cli_abort(
      "{.arg {x_name}} must contain values < the species-specific asymptote for black spruce ({.code PICE.MAR})."
    )
  }
  if (any(!asymptote_species & out$x >= out$beta0)) {
    cli::cli_abort(
      "{.arg {x_name}} must contain values < the species-specific asymptote for jack pine ({.code PINU.BAN})."
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
.augerward2021_parameters <- function() {
  pars <- .get_internal_data("parameters_AugerWard2021") |>
    dplyr::as_tibble()

  req <- c("Species", "beta0", "beta2", "form")
  assert_required_cols(pars, req, object = "parameters_AugerWard2021")

  pars
}


# internal
.augerward2021_height <- function(age, si, base_age, Species) {
  pars <- .augerward2021_parameters()

  dplyr::tibble(
    age = age,
    si = si,
    base_age = base_age,
    Species = Species
  ) |>
    dplyr::left_join(pars, by = "Species") |>
    dplyr::mutate(
      height = dplyr::case_when(
        .data$form == "black_spruce" ~ .data$beta0 /
          (1 -
            (1 - .data$beta0 / .data$si) *
              (.data$base_age / .data$age)^.data$beta2),
        .data$form == "jack_pine" ~ .data$beta0 -
          .data$beta0 *
            (1 - .data$si / .data$beta0)^((.data$age /
              .data$base_age)^.data$beta2),
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::pull(.data$height)
}


# internal
.augerward2021_si <- function(age, height, base_age, Species) {
  pars <- .augerward2021_parameters()

  dplyr::tibble(
    age = age,
    height = height,
    base_age = base_age,
    Species = Species
  ) |>
    dplyr::left_join(pars, by = "Species") |>
    dplyr::mutate(
      si = dplyr::case_when(
        .data$form == "black_spruce" ~ .data$beta0 /
          (1 -
            (1 - .data$beta0 / .data$height) *
              (.data$age / .data$base_age)^.data$beta2),
        .data$form == "jack_pine" ~ .data$beta0 -
          .data$beta0 *
            (1 - .data$height / .data$beta0)^((.data$base_age /
              .data$age)^.data$beta2),
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::pull(.data$si)
}
