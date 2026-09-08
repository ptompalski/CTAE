#' Nigh (1997) Sitka spruce height-age (site index) model for coastal British Columbia
#'
#' Vectorized implementation of the logistic height-age (site index) model in
#' Nigh (1997) for Sitka spruce (\emph{Picea sitchensis}) in coastal British
#' Columbia. The model was developed from stem-analysis data collected in the
#' Queen Charlotte Islands and is recommended for use in British Columbia because
#' of its improved extrapolation to old ages.
#'
#' \strong{Model scope (species coverage):} Sitka spruce, NFI code
#' \code{PICE.SIT}.
#'
#' \strong{Age definition note:} `age` is breast-height age (BHA, years). The
#' model is conditioned to return `height = si` exactly at BHA 50.
#'
#' \strong{Base-age note:} site index is site height at breast-height age 50.
#'
#' The recommended model (Nigh 1997, eq. 8) is the integral logistic form
#' \deqn{H = 1.3 + (SI - 1.3) \times
#'   \frac{1 + e^{a_0 + a_1 \ln(49.5) + a_2 \ln(SI - 1.3)}}
#'        {1 + e^{a_0 + a_1 \ln(BHA - 0.5) + a_2 \ln(SI - 1.3)}}}
#' with \eqn{a_0 = 8.947}, \eqn{a_1 = -1.357}, \eqn{a_2 = -1.013} (Table 3, model
#' \[7\]).
#'
#' Because \eqn{SI} appears both as a multiplier and inside a logarithm, the
#' equation has no closed-form inverse in \eqn{SI}; when predicting site index the
#' equation is solved numerically.
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
#' Nigh, G.D. (1997). A Sitka spruce height-age model with improved extrapolation
#' properties. The Forestry Chronicle 73(3): 363--369.
#'
#' @examples
#' # Predict height from age + site index
#' si_nigh1997(age = c(25, 50, 80), si = c(20, 30, 38))
#'
#' # Predict site index from age + height
#' si_nigh1997(age = c(25, 50, 80), height = c(15, 30, 42))
#'
#' @export
si_nigh1997 <- function(age, height = NULL, si = NULL) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .nigh1997_prepare(age = age, x = x, x_name = x_name)

  if (mode == "predict_height") {
    out <- .nigh1997_height(bha = df$age, si = df$si, df)
    if (any(!is.finite(out))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_nigh1997}.",
        "i" = "Check inputs and model coefficients."
      ))
    }
    return(dplyr::tibble(height = out))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .nigh1997_si_from_height_one(
        bha = df$age[[i]],
        height = df$height[[i]],
        pars = df[i, , drop = FALSE]
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_nigh1997}.",
      "i" = "Check inputs and model coefficients."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.nigh1997_prepare <- function(age, x, x_name) {
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

  pars <- .nigh1997_parameters()

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    a0 = pars$a0,
    a1 = pars$a1,
    a2 = pars$a2
  )

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
  }

  out
}


# internal
# Site height at BHA given site index (Nigh 1997 eq. 8). Vectorized over inputs
# sharing a common length; `pars` supplies a0/a1/a2 columns.
.nigh1997_height <- function(bha, si, pars) {
  a0 <- pars$a0
  a1 <- pars$a1
  a2 <- pars$a2
  z <- log(si - 1.3)
  num <- 1 + exp(a0 + a1 * log(49.5) + a2 * z)
  den <- 1 + exp(a0 + a1 * log(bha - 0.5) + a2 * z)
  1.3 + (si - 1.3) * num / den
}


# internal
# Solve site index from an observed (BHA, height) pair by root-finding on
# height(BHA, SI) - height = 0. SI must exceed 1.3 m (breast height).
.nigh1997_si_from_height_one <- function(bha, height, pars) {
  if (!is.finite(bha) || !is.finite(height) || bha <= 0.5 || height <= 1.3) {
    return(NaN)
  }
  # At BHA 50 the model is conditioned so height == si exactly.
  if (isTRUE(all.equal(bha, 50))) {
    return(height)
  }
  f <- function(s) .nigh1997_height(bha = bha, si = s, pars = pars) - height
  lo <- 1.3 + 1e-6
  hi <- 200
  if (!is.finite(f(lo)) || !is.finite(f(hi)) || f(lo) * f(hi) > 0) {
    # nocov start
    # Defensive: for a valid (BHA, height) pair with height in (1.3, 200) the
    # monotone height curve brackets a unique site index, so this no-bracket
    # guard is not reached via the public API (out-of-domain heights are screened
    # by the `height <= 1.3` check above and surface as a non-finite abort).
    return(NaN)
    # nocov end
  }
  stats::uniroot(f, c(lo, hi), tol = .Machine$double.eps^0.5)$root
}


# internal
.nigh1997_parameters <- function() {
  pars <- .get_internal_data("parameters_Nigh1997") |>
    dplyr::as_tibble()

  req <- c("Species", "model", "a0", "a1", "a2")
  assert_required_cols(pars, req, object = "parameters_Nigh1997")

  pars
}
