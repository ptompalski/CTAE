#' Carmean, Hazenberg and Deschamps (2006) polymorphic site index models for northwest Ontario
#'
#' Unified, vectorized implementation of the Newnham (1988) constrained
#' polymorphic height-age (site index) model published in Carmean, Hazenberg
#' and Deschamps (2006) for black spruce (\emph{Picea mariana}) and trembling
#' aspen (\emph{Populus tremuloides}) in northwest Ontario.
#'
#' \strong{Model scope (species coverage):} black spruce (\code{PICE.MAR}) and
#' trembling aspen (\code{POPU.TRE}).
#'
#' \strong{Age definition note:} `age` is breast-height age (years). Curves
#' start at breast height (0 years at BH) and the model is constrained so that
#' `height = si` exactly at breast-height age 50.
#'
#' \strong{Base-age note:} site index is height (m) at 50 years breast-height
#' age.
#'
#' \strong{Domain note:} the curves were fitted to data 100 years and less
#' breast-height age; the source notes they may be extended to about 150 years
#' with reduced precision.
#'
#' \strong{Source legibility caveat:} the published PDF renders equations 1 and
#' 2 (p. 7) as low-quality raster images, and the printed coefficient exponents
#' are partly degraded. For black spruce the exponent \eqn{b_2} could not be
#' read unambiguously from the source raster and is transcribed as its best
#' reading, \eqn{0.1136}. Treat the black spruce coefficients (in particular
#' \eqn{b_2}) as provisional.
#'
#' The model form is
#' \deqn{\hat{H} = 1.3 + b_1 (S - 1.3)^{b_2}
#'   \left[1 - k^{Age/50}\right]^{b_3 (S - 1.3)^{b_4}}}
#' with
#' \deqn{k = 1 - \left[\frac{S - 1.3}{b_1 (S - 1.3)^{b_2}}\right]^
#'   {1 / (b_3 (S - 1.3)^{b_4})}.}
#' Because \eqn{S} appears in several nonlinear positions the model has no
#' closed-form inverse in \eqn{S}; when predicting site index the equation is
#' solved numerically.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `si` is provided, the function predicts `height`.
#'   \item If `height` is provided, the function predicts `si`.
#' }
#'
#' @param age Numeric vector. Breast-height age (years), with `age > 0`.
#' @param height Optional numeric vector. Site height (m). If provided, `si` is
#'   predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at breast
#'   height). If provided, `height` is predicted.
#' @param species Character vector of NFI species codes: `"PICE.MAR"` (black
#'   spruce) or `"POPU.TRE"` (trembling aspen).
#'
#' @return A tibble with a single column:
#' \describe{
#'   \item{height}{Predicted site height (m), returned when `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when `height` is provided.}
#' }
#'
#' @references
#' Carmean, W.H., Hazenberg, G., and Deschamps, K.C. (2006). Polymorphic site
#' index curves for black spruce and trembling aspen in northwest Ontario. The
#' Forestry Chronicle 82(2): 213--231.
#'
#' Newnham, R.M. (1988). A modification of the Ek-Payandeh nonlinear regression
#' model for site index curves. Canadian Journal of Forest Research 18:
#' 115--120.
#'
#' @examples
#' # Predict height from age + site index
#' si_carmean2006(
#'   age = c(25, 50, 80),
#'   si = c(12, 17, 20),
#'   species = c("PICE.MAR", "PICE.MAR", "POPU.TRE")
#' )
#'
#' # Predict site index from age + height
#' si_carmean2006(
#'   age = c(25, 50, 80),
#'   height = c(8, 17, 24),
#'   species = c("PICE.MAR", "PICE.MAR", "POPU.TRE")
#' )
#'
#' @export
si_carmean2006 <- function(age, height = NULL, si = NULL, species) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .carmean2006_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name
  )

  if (mode == "predict_height") {
    h <- vapply(
      seq_len(nrow(df)),
      function(i) {
        .carmean_newnham_height_one(
          age = df$age[[i]],
          si = df$si[[i]],
          pars = df[i, , drop = FALSE],
          fn = "si_carmean2006"
        )
      },
      numeric(1)
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_carmean2006}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }

    return(dplyr::tibble(height = h))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .carmean_newnham_si_one(
        age = df$age[[i]],
        height = df$height[[i]],
        pars = df[i, , drop = FALSE],
        fn = "si_carmean2006"
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_carmean2006}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.carmean2006_prepare <- function(age, x, species, x_name) {
  n <- max(length(age), length(x), length(species))
  if (n == 0L) {
    # nocov start
    # Defensive: unreachable via the public API, which requires `age` plus one
    # of `height`/`si` and `species` (all length > 0).
    cli::cli_abort("{.arg age} must have length > 0.")
    # nocov end
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

  pars <- .carmean2006_parameters()

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$b1)) {
    bad <- unique(out$Species[is.na(out$b1)])
    cli::cli_abort(
      "No Carmean2006 parameters found for species: {paste(bad, collapse = ', ')}."
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
.carmean2006_parameters <- function() {
  pars <- .get_internal_data("parameters_Carmean2006") |>
    dplyr::as_tibble()

  req <- c("Species", "b1", "b2", "b3", "b4", "base_age")
  assert_required_cols(pars, req, object = "parameters_Carmean2006")

  pars
}
