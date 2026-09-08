#' Brisco, Klinka and Nigh (2002) western larch height-age (site index) model
#'
#' Unified, vectorized implementation of the recommended height-growth (site
#' index) model in Brisco, Klinka and Nigh (2002) for western larch
#' (\emph{Larix occidentalis}) in British Columbia.
#'
#' \strong{Model scope (species coverage):} western larch, NFI code
#' \code{LARI.OCC}.
#'
#' \strong{Age definition note:} `age` is breast-height age (BHA, years).
#'
#' \strong{Base-age note:} site index is site height at breast-height age 50.
#'
#' \strong{Model form:} the authors compared four model forms and recommend the
#' unconstrained Chapman-Richards model (their eq. 3) refit to the complete data
#' set (their eq. 6):
#' \deqn{H = 1.3 + b_1 (S - 1.3)^{b_2}
#'   \left(1 - e^{b_3 (A - 0.5)}\right)^{b_4 (S - 1.3)^{b_5}}}
#' with \eqn{b_1 = 3.875}, \eqn{b_2 = 0.7850}, \eqn{b_3 = -0.01497},
#' \eqn{b_4 = 2.193}, \eqn{b_5 = -0.2318}, where \eqn{H} is site height (m),
#' \eqn{A} is breast-height age (years), and \eqn{S} is site index (m). At
#' \eqn{A = 50} the equation returns \eqn{H = S}.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `si` is provided, the function predicts `height` directly from
#'     eq. 6.
#'   \item If `height` is provided, the function predicts `si`. Because \eqn{S}
#'     appears in the base, the leading power, and the outer exponent of eq. 6,
#'     it cannot be isolated in closed form and is obtained by numerically
#'     inverting eq. 6 for each (age, height) pair.
#' }
#'
#' @param age Numeric vector. Breast-height age (years).
#' @param height Optional numeric vector. Site height (m). If provided, `si` is
#'   predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at breast
#'   height). If provided, `height` is predicted.
#'
#' @return A tibble with a single column:
#' \describe{
#'   \item{height}{Predicted site height (m), returned when `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when `height` is provided.}
#' }
#'
#' @references
#' Brisco, D., Klinka, K., and Nigh, G. 2002. Height growth models for western
#' larch in British Columbia. West. J. Appl. For. 17(2):66--74.
#'
#' @examples
#' # Predict height from age + site index
#' si_brisco2002(age = c(25, 50, 80), si = c(15, 20, 25))
#'
#' # Predict site index from age + height
#' si_brisco2002(age = c(25, 50, 80), height = c(10, 20, 28))
#'
#' @export
si_brisco2002 <- function(age, height = NULL, si = NULL) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .brisco2002_prepare(age = age, x = x, x_name = x_name)
  pars <- .brisco2002_parameters()

  if (mode == "predict_height") {
    out <- .brisco2002_height(age = df$age, si = df$si, pars = pars)
    if (any(!is.finite(out))) {
      # nocov start
      # Defensive: for finite positive `si` (si > 1.3 in practice) the base
      # 1 - exp(b3*(age - 0.5)) stays in (0, 1) because b3 < 0 keeps the exponent
      # negative, so height is always finite. Kept as a guard against pathological
      # coefficients.
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_brisco2002}.",
        "i" = "Check inputs and model coefficients."
      ))
      # nocov end
    }
    return(dplyr::tibble(height = out))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .brisco2002_si_from_height_one(
        age = df$age[[i]],
        height = df$height[[i]],
        pars = pars
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_brisco2002}.",
      "i" = "Check inputs and model coefficients."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.brisco2002_prepare <- function(age, x, x_name) {
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

  out <- dplyr::tibble(age = as.numeric(age))
  if (identical(x_name, "height")) {
    out$height <- as.numeric(x)
  } else {
    out$si <- as.numeric(x)
  }
  out
}


# internal
# Chapman-Richards height at breast-height age given site index (eq. 6).
# Vectorized over `age` and `si` (recycled to a common length upstream).
.brisco2002_height <- function(age, si, pars) {
  s <- si - 1.3
  1.3 +
    pars$b1 * s^pars$b2 * (1 - exp(pars$b3 * (age - 0.5)))^(pars$b4 * s^pars$b5)
}


# internal
# Solve site index from an observed (BHA, height) pair by numerically inverting
# eq. 6 (height increases monotonically with site index at fixed age).
.brisco2002_si_from_height_one <- function(age, height, pars) {
  if (!is.finite(age) || !is.finite(height) || age <= 0.5 || height <= 1.3) {
    return(NaN)
  }
  f <- function(si) .brisco2002_height(age = age, si = si, pars = pars) - height
  lo <- 1.3 + 1e-6
  hi <- 60
  if (!is.finite(f(lo)) || !is.finite(f(hi)) || f(lo) * f(hi) > 0) {
    # nocov start
    # Defensive: for a valid (BHA, height) pair the monotone height curve
    # brackets a unique site index within (1.3, 60]; out-of-domain heights are
    # screened by the `height <= 1.3` guard above and surface as a non-finite
    # abort. Kept as a guard against pathological inputs.
    return(NaN)
    # nocov end
  }
  stats::uniroot(f, c(lo, hi), tol = .Machine$double.eps^0.5)$root
}


# internal
.brisco2002_parameters <- function() {
  pars <- .get_internal_data("parameters_Brisco2002") |>
    dplyr::as_tibble()

  req <- c("Species", "b1", "b2", "b3", "b4", "b5")
  assert_required_cols(pars, req, object = "parameters_Brisco2002")

  pars[1, , drop = FALSE]
}
