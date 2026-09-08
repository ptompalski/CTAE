#' Nigh (2017) lodgepole pine height-age (site index) model for British Columbia
#'
#' Unified, vectorized implementation of the grounded-Generalized Algebraic
#' Difference Approach (g-GADA) height-age (site index) model in Nigh (2017) for
#' lodgepole pine (\emph{Pinus contorta} var. \emph{latifolia}) in British
#' Columbia.
#'
#' \strong{Model scope (species coverage):} lodgepole pine, NFI code
#' \code{PINU.CON}.
#'
#' \strong{Age definition note:} `age` is breast-height age (BHA, years).
#'
#' \strong{Base-age note:} site index is site height at breast-height age 50.
#'
#' \strong{Model form:} the fitted g-GADA model (eq. 4) is
#' \deqn{HT = 1.3 + \beta_0 \left(1 - e^{(\beta_{10} + \beta_{11}\beta_0)
#'   (BHA - 0.5)}\right)^{\beta_{20} + \beta_{21}\beta_0}}
#' where \eqn{\beta_0} is a tree-specific parameter that localizes the curve and
#' \eqn{\beta_{10}, \beta_{11}, \beta_{20}, \beta_{21}} are the fitted global
#' parameters (Table 2).
#'
#' The g-GADA localizes on the asymptote-like parameter \eqn{\beta_0}, not
#' directly on site index. This implementation follows the paper's recommended
#' base-age-50 workflow:
#' \itemize{
#'   \item When `si` is supplied, \eqn{\beta_0} is obtained from the paper's
#'     cubic SI-to-\eqn{\beta_0} conversion (Discussion, p. 18), then height is
#'     evaluated with eq. 4. The cubic is valid for site index 5--30 m and has a
#'     stated maximum \eqn{\beta_0} error of 16 cm, so `height` equals `si` at
#'     BHA 50 only to within that tolerance (not exactly).
#'   \item When `height` is supplied, \eqn{\beta_0} is calibrated numerically to
#'     the (BHA, height) pair, then site index is the height predicted at BHA 50.
#' }
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
#'   height). If provided, `height` is predicted. The paper's SI-to-parameter
#'   conversion is intended for site index in the range 5--30 m.
#'
#' @return A tibble with a single column:
#' \describe{
#'   \item{height}{Predicted site height (m), returned when `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when `height` is provided.}
#' }
#'
#' @references
#' Nigh, G.D. 2017. Development of a lodgepole pine site index model with the
#' grounded-Generalized Algebraic Difference Approach (g-GADA). Prov. B.C.,
#' Victoria, B.C. Res. Rep. 31.
#'
#' @examples
#' # Predict height from age + site index
#' si_nigh2017(age = c(25, 50, 80), si = c(12, 18, 24))
#'
#' # Predict site index from age + height
#' si_nigh2017(age = c(25, 50, 80), height = c(8, 18, 26))
#'
#' @export
si_nigh2017 <- function(age, height = NULL, si = NULL) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .nigh2017_prepare(age = age, x = x, x_name = x_name)

  if (mode == "predict_height") {
    b0 <- .nigh2017_b0_from_si(df$si, df)
    out <- .nigh2017_height(bha = df$age, b0 = b0, pars = df)
    if (any(!is.finite(out))) {
      # nocov start
      # Defensive: for finite positive `si` the cubic gives a finite beta0, and
      # the power base 1 - exp((b10 + b11*b0)(bha - 0.5)) stays in (0, 1) because
      # b10, b11 < 0 keep the exponent negative, so the height is always finite.
      # Kept as a guard against pathological coefficients.
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_nigh2017}.",
        "i" = "Check inputs and model coefficients."
      ))
      # nocov end
    }
    return(dplyr::tibble(height = out))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .nigh2017_si_from_height_one(
        bha = df$age[[i]],
        height = df$height[[i]],
        pars = df[i, , drop = FALSE]
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_nigh2017}.",
      "i" = "Check inputs and model coefficients."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.nigh2017_prepare <- function(age, x, x_name) {
  n <- max(length(age), length(x))
  if (n == 0L) {
    # nocov start
    # Defensive: unreachable via the public API, which requires `age` plus one of
    # `height`/`si` (both length > 0). Kept as a guard for direct internal calls.
    cli::cli_abort("{.arg age} must have length > 0.")
    # nocov end
  }

  recycled <- assert_len_compat(age = age, x = x, .n = n, .recycle = TRUE)
  age <- recycled$age
  x <- recycled$x

  assert_numeric_vec(age, "age", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 0, allow_na = FALSE)

  pars <- .nigh2017_parameters()

  out <- dplyr::tibble(
    age = as.numeric(age),
    b10 = pars$b10,
    b11 = pars$b11,
    b20 = pars$b20,
    b21 = pars$b21,
    si_b0_c0 = pars$si_b0_c0,
    si_b0_c1 = pars$si_b0_c1,
    si_b0_c2 = pars$si_b0_c2,
    si_b0_c3 = pars$si_b0_c3
  )

  if (identical(x_name, "height")) {
    out$height <- as.numeric(x)
  } else {
    out$si <- as.numeric(x)
  }

  out
}


# internal
# g-GADA site height at BHA given the localizing parameter beta0 (eq. 4).
# Vectorized over inputs sharing a common length; `pars` supplies b10/b11/b20/b21.
.nigh2017_height <- function(bha, b0, pars) {
  b1 <- pars$b10 + pars$b11 * b0
  b2 <- pars$b20 + pars$b21 * b0
  1.3 + b0 * (1 - exp(b1 * (bha - 0.5)))^b2
}


# internal
# Paper's cubic SI -> beta0 conversion for base age 50 (Discussion, p. 18).
.nigh2017_b0_from_si <- function(si, pars) {
  pars$si_b0_c0 +
    pars$si_b0_c1 * si +
    pars$si_b0_c2 * si^2 +
    pars$si_b0_c3 * si^3
}


# internal
# Solve site index from an observed (BHA, height) pair. Calibrate beta0 to the
# pair by root-finding on height(BHA, beta0) - height = 0, then evaluate the
# height curve at BHA 50 to get the site index.
.nigh2017_si_from_height_one <- function(bha, height, pars) {
  if (!is.finite(bha) || !is.finite(height) || bha <= 0.5 || height <= 1.3) {
    return(NaN)
  }
  f <- function(b0) .nigh2017_height(bha = bha, b0 = b0, pars = pars) - height
  lo <- 1e-6
  hi <- 60
  if (!is.finite(f(lo)) || !is.finite(f(hi)) || f(lo) * f(hi) > 0) {
    # nocov start
    # Defensive: for a valid (BHA, height) pair with height in (1.3, ~asymptote)
    # the monotone height curve brackets a unique beta0 within (0, 60], so this
    # no-bracket guard is not reached via the public API; out-of-domain heights
    # are screened by the `height <= 1.3` check above and surface as a non-finite
    # abort.
    return(NaN)
    # nocov end
  }
  b0 <- stats::uniroot(f, c(lo, hi), tol = .Machine$double.eps^0.5)$root
  .nigh2017_height(bha = 50, b0 = b0, pars = pars)
}


# internal
.nigh2017_parameters <- function() {
  pars <- .get_internal_data("parameters_Nigh2017") |>
    dplyr::as_tibble()

  req <- c(
    "Species",
    "b10",
    "b11",
    "b20",
    "b21",
    "si_b0_c0",
    "si_b0_c1",
    "si_b0_c2",
    "si_b0_c3"
  )
  assert_required_cols(pars, req, object = "parameters_Nigh2017")

  pars
}
