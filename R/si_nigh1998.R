#' Nigh (1998) height-age (site index) model for interior western hemlock
#'
#' Vectorized implementation of the log-logistic height--breast-height-age
#' (site index) model in Nigh (1998) for western hemlock (\code{TSUG.HET}) in
#' the interior of British Columbia.
#'
#' \strong{Model scope (species coverage):} western hemlock, NFI code
#' \code{TSUG.HET}.
#'
#' \strong{Age definition note:} `age` is breast-height age (BHA, years). The
#' model is conditioned to return `height = si` exactly at BHA 50.
#'
#' \strong{Base-age note:} site index is site height at breast-height age 50.
#'
#' The fitted model (eq. 6) is the log-logistic form
#' \deqn{H = 1.3 + (SI - 1.3) \times
#'   \frac{1 + e^{b_0 + b_1 \ln(49.5) + b_2 \ln(SI - 1.3)}}
#'        {1 + e^{b_0 + b_1 \ln(A - 0.5) + b_2 \ln(SI - 1.3)}}}
#' with \eqn{b_0 = 8.998}, \eqn{b_1 = -1.434}, \eqn{b_2 = -1.051}, where `A` is
#' breast-height age (years).
#'
#' Because \eqn{SI} appears both as a multiplier and inside a logarithm, the
#' equation has no closed-form inverse in \eqn{SI}; when predicting site index
#' the equation is solved numerically.
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
#'
#' @return A tibble with a single column:
#' \describe{
#'   \item{height}{Predicted site height (m), returned when `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when `height` is provided.}
#' }
#'
#' @references
#' Nigh, G.D. (1998). A system for estimating height and site index of western
#' hemlock in the interior of British Columbia. The Forestry Chronicle 74(4):
#' 588--596.
#'
#' @examples
#' # Predict height from age + site index
#' si_nigh1998(age = c(25, 50, 80), si = c(12, 18, 24))
#'
#' # Predict site index from age + height
#' si_nigh1998(age = c(25, 50, 80), height = c(8, 18, 26))
#'
#' @export
si_nigh1998 <- function(age, height = NULL, si = NULL) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  n <- max(length(age), length(x))
  if (n == 0L) {
    cli::cli_abort("{.arg age} must have length > 0.")
  }

  recycled <- assert_len_compat(
    age = age,
    x = x,
    .n = n,
    .recycle = TRUE
  )
  age <- recycled$age
  x <- recycled$x

  assert_numeric_vec(age, "age", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 0, allow_na = FALSE)

  if (mode == "predict_height") {
    out <- .nigh1998_height(bha = as.numeric(age), si = as.numeric(x))
    if (any(!is.finite(out))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_nigh1998}.",
        "i" = "Check inputs and model domain."
      ))
    }
    return(dplyr::tibble(height = out))
  }

  si_est <- vapply(
    seq_len(n),
    function(i) {
      .nigh1998_si_from_height_one(
        bha = as.numeric(age)[[i]],
        height = as.numeric(x)[[i]]
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_nigh1998}.",
      "i" = "Check inputs and model domain."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
# Fitted height-bha coefficients (Nigh 1998, Table 2 / eq. 6).
.nigh1998_coef <- function() {
  pars <- .get_internal_data("parameters_Nigh1998_ht") |>
    dplyr::as_tibble()
  assert_required_cols(
    pars,
    c("b0", "b1", "b2"),
    object = "parameters_Nigh1998_ht"
  )
  list(b0 = pars$b0[[1]], b1 = pars$b1[[1]], b2 = pars$b2[[1]])
}


# internal
# Site height at BHA given site index (eq. 6). Vectorized over inputs sharing a
# common length.
.nigh1998_height <- function(bha, si) {
  cf <- .nigh1998_coef()
  z <- log(si - 1.3)
  num <- 1 + exp(cf$b0 + cf$b1 * log(49.5) + cf$b2 * z)
  den <- 1 + exp(cf$b0 + cf$b1 * log(bha - 0.5) + cf$b2 * z)
  1.3 + (si - 1.3) * num / den
}


# internal
# Solve site index from an observed (BHA, height) pair by root-finding on
# height(BHA, SI) - height = 0. SI must exceed 1.3 m (breast height).
.nigh1998_si_from_height_one <- function(bha, height) {
  if (!is.finite(bha) || !is.finite(height) || bha <= 0.5 || height <= 1.3) {
    return(NaN)
  }
  # At BHA 50 the model is conditioned so height == si exactly.
  if (isTRUE(all.equal(bha, 50))) {
    return(height)
  }
  f <- function(s) .nigh1998_height(bha = bha, si = s) - height
  lo <- 1.3 + 1e-6
  hi <- 200
  if (!is.finite(f(lo)) || !is.finite(f(hi)) || f(lo) * f(hi) > 0) {
    # nocov start
    # Defensive: for a valid (BHA, height) pair with height in (1.3, 200) the
    # monotone height curve brackets a unique site index, so this no-bracket
    # guard is not reached via the public API.
    return(NaN)
    # nocov end
  }
  stats::uniroot(f, c(lo, hi), tol = .Machine$double.eps^0.5)$root
}
