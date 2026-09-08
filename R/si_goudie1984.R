#' Goudie (1984) lodgepole pine and white spruce height-age (site index) model
#'
#' Unified, vectorized implementation of the Goudie (1984) logistic height-age
#' (site index) curves for lodgepole pine (\emph{Pinus contorta}) and white
#' spruce (\emph{Picea glauca}) in British Columbia.
#'
#' \strong{Model scope (species coverage):} lodgepole pine, NFI code
#' \code{PINU.CON}; white spruce, NFI code \code{PICE.GLA}.
#'
#' \strong{Age definition note:} `age` is breast-height age (BHA, years). The
#' curve is conditioned so that `height = si` at BHA 50.
#'
#' \strong{Base-age note:} site index is site height at breast-height age 50.
#'
#' \strong{Implementation basis (differs from the original publication):} the
#' coefficients and functional form here incorporate two later modifications,
#' not the equations as originally printed in Goudie (1984):
#' \itemize{
#'   \item \strong{Half-year age shift.} A 2004 modification subtracts 0.5 year
#'     from both the age and the base age inside the logistic (using
#'     \eqn{\ln(\mathrm{BHA} - 0.5)} and \eqn{\ln(49.5)}), so that height equals
#'     1.3 m at BHA 0.5 years. The original paper uses \eqn{\ln(\mathrm{BHA})}
#'     and \eqn{\ln(50)}.
#'   \item \strong{Lodgepole pine: dry-site coefficients only.} Goudie (1984)
#'     eq. 7 gives habitat-specific pine coefficients (dry vs. wet site). This
#'     implementation uses only the \emph{dry-site} coefficients, which the
#'     author recommends when no ecological information is available. There is
#'     no habitat argument; white spruce is unaffected.
#' }
#'
#' The height-age curve is
#' \deqn{HT = 1.3 + (SI - 1.3) \times
#'   \frac{1 + e^{b_1 + b_2 \ln(49.5) - b_3 \ln(SI - 1.3)}}
#'        {1 + e^{b_1 + b_2 \ln(BHA - 0.5) - b_3 \ln(SI - 1.3)}}}
#' with per-species coefficients \eqn{b_1, b_2, b_3}. Because \eqn{SI} appears
#' both as a multiplier and inside a logarithm, the curve has no closed-form
#' inverse in \eqn{SI}; when predicting site index the equation is solved
#' numerically.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `si` is provided, the function predicts `height`.
#'   \item If `height` is provided, the function predicts `si`.
#' }
#'
#' @param age Numeric vector. Breast-height age (years).
#' @param height Optional numeric vector. Site height (m). If provided, `si` is
#'   predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at breast
#'   height). If provided, `height` is predicted.
#' @param species Character vector of species codes. One of `"PINU.CON"`
#'   (lodgepole pine) or `"PICE.GLA"` (white spruce); compact and jurisdiction
#'   codes are standardized.
#'
#' @return A tibble with a single column:
#' \describe{
#'   \item{height}{Predicted site height (m), returned when `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when `height` is provided.}
#' }
#'
#' @references
#' Goudie, J.W. (1984). Height Growth and Site Index Curves for Lodgepole Pine
#' and White Spruce and Interim Managed Stand Yield Tables for Lodgepole Pine in
#' British Columbia. Final Report FY-1983-84. Research Branch, British Columbia
#' Ministry of Forests, Victoria, B.C.
#'
#' @examples
#' # Predict height from age + site index
#' si_goudie1984(age = c(25, 50, 80), si = c(12, 18, 24),
#'               species = c("PINU.CON", "PICE.GLA", "PINU.CON"))
#'
#' # Predict site index from age + height
#' si_goudie1984(age = c(25, 50, 80), height = c(8, 18, 26),
#'               species = "PICE.GLA")
#'
#' @export
si_goudie1984 <- function(age, height = NULL, si = NULL, species) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .goudie1984_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name
  )

  if (mode == "predict_height") {
    out <- .goudie1984_height(bha = df$age, si = df$si, df)
    if (any(!is.finite(out))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_goudie1984}.",
        "i" = "Check inputs and model coefficients."
      ))
    }
    return(dplyr::tibble(height = out))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .goudie1984_si_from_height_one(
        bha = df$age[[i]],
        height = df$height[[i]],
        pars = df[i, , drop = FALSE]
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_goudie1984}.",
      "i" = "Check inputs and model coefficients."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.goudie1984_prepare <- function(age, x, species, x_name) {
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

  pars <- .goudie1984_parameters()

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$b1)) {
    bad <- unique(out$Species[is.na(out$b1)])
    cli::cli_abort(c(
      "No Goudie1984 parameters found for species: {paste(bad, collapse = ', ')}.",
      "i" = "Supported species: PINU.CON (lodgepole pine), PICE.GLA (white spruce)."
    ))
  }

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
  }

  out
}


# internal
# Site height at BHA given site index. Vectorized over inputs sharing a common
# length; `pars` supplies b1/b2/b3 columns. The base age (50) and BHA are
# shifted by -0.5 inside the logs.
.goudie1984_height <- function(bha, si, pars) {
  b1 <- pars$b1
  b2 <- pars$b2
  b3 <- pars$b3
  z <- log(si - 1.3)
  num <- 1 + exp(b1 + b2 * log(49.5) - b3 * z)
  den <- 1 + exp(b1 + b2 * log(bha - 0.5) - b3 * z)
  1.3 + (si - 1.3) * num / den
}


# internal
# Solve site index from an observed (BHA, height) pair by root-finding on
# height(BHA, SI) - height = 0. SI must exceed 1.3 m (breast height).
.goudie1984_si_from_height_one <- function(bha, height, pars) {
  if (!is.finite(bha) || !is.finite(height) || bha <= 0.5 || height <= 1.3) {
    return(NaN)
  }
  # At BHA 50 the model is conditioned so height == si exactly.
  if (isTRUE(all.equal(bha, 50))) {
    return(height)
  }
  f <- function(s) .goudie1984_height(bha = bha, si = s, pars = pars) - height
  lo <- 1.3 + 1e-6
  hi <- 200
  if (!is.finite(f(lo)) || !is.finite(f(hi)) || f(lo) * f(hi) > 0) {
    # nocov start
    # Defensive: for a valid (BHA, height) pair with height in (1.3, 200) the
    # monotone height curve brackets a unique site index.
    return(NaN)
    # nocov end
  }
  stats::uniroot(f, c(lo, hi), tol = .Machine$double.eps^0.5)$root
}


# internal
.goudie1984_parameters <- function() {
  pars <- .get_internal_data("parameters_Goudie1984") |>
    dplyr::as_tibble()

  req <- c("Species", "index_age", "b1", "b2", "b3")
  assert_required_cols(pars, req, object = "parameters_Goudie1984")

  pars
}
