#' Alemdag (1991) national site-index and height-growth model for white spruce
#'
#' Vectorized implementation of the national (Canada-wide) site-index and
#' height-growth equations of Alemdag (1991) for white spruce
#' (\emph{Picea glauca} (Moench) Voss), developed from stem-analysis data pooled
#' across the two territories and eight provinces (no data from British Columbia
#' or Nova Scotia), from the Yukon to Newfoundland.
#'
#' \strong{Model scope (species coverage):} white spruce, NFI code
#' \code{PICE.GLA}. This is a single national model with no species argument.
#'
#' \strong{Age definition note:} `age` is breast-height age (years), i.e. rings
#' counted at 1.30 m. Both equations use height and site index measured above
#' breast height (\eqn{H - 1.30}, \eqn{S - 1.30}).
#'
#' \strong{Base-age note:} site index is total tree height at index (base) age
#' \eqn{A_1 = 50} years breast-height age. Both equations are conditioned so that
#' the predicted value equals the input at \eqn{A = 50} (\eqn{S = H} there).
#'
#' The site-index equation (Alemdag 1991, model \[9\], \eqn{c} coefficients) is
#' \deqn{S = 1.30 + \frac{1}{c_1 (H - 1.30)^{c_2}
#'   \left(1 - m^{A/A_1}\right)^{c_4 (H - 1.30)^{c_5}}},}
#' with (model \[8\])
#' \deqn{m = 1 - \left[\frac{1}{c_1 (H - 1.30)^{1 + c_2}}\right]
#'   ^{1 / (c_4 (H - 1.30)^{c_5})}.}
#'
#' The height-growth equation (Alemdag 1991, model \[4\], \eqn{b} coefficients) is
#' \deqn{H = 1.30 + b_1 (S - 1.30)^{b_2}
#'   \left(1 - k^{A/A_1}\right)^{b_4 (S - 1.30)^{b_5}},}
#' with (model \[5\])
#' \deqn{k = 1 - \left[\frac{S - 1.30}{b_1 (S - 1.30)^{b_2}}\right]
#'   ^{1 / (b_4 (S - 1.30)^{b_5})}.}
#'
#' The two equations were fitted independently (they are not exact inverses of
#' one another), each on the combined national data. Both directions are
#' closed-form.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `si` is provided, the function predicts `height` (model \[4\]).
#'   \item If `height` is provided, the function predicts `si` (model \[9\]).
#' }
#'
#' @param age Numeric vector. Breast-height age (years).
#' @param height Optional numeric vector. Total tree height (m). If provided,
#'   `si` is predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at breast
#'   height). If provided, `height` is predicted.
#'
#' @return A tibble with a single column:
#' \describe{
#'   \item{height}{Predicted total tree height (m), returned when `si` is
#'     provided.}
#'   \item{si}{Predicted site index (m), returned when `height` is provided.}
#' }
#'
#' @references
#' Alemdag, I.S. (1991). National site-index and height-growth curves for white
#' spruce growing in natural stands in Canada. Canadian Journal of Forest
#' Research 21(10): 1466--1474. \doi{10.1139/x91-206}
#'
#' @examples
#' # Predict height from age + site index
#' si_alemdag1991(age = c(25, 50, 80), si = c(12, 15, 18))
#'
#' # Predict site index from age + height
#' si_alemdag1991(age = c(25, 50, 80), height = c(9, 15, 20))
#'
#' @export
si_alemdag1991 <- function(age, height = NULL, si = NULL) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .alemdag1991_prepare(age = age, x = x, x_name = x_name)

  if (mode == "predict_height") {
    out <- .alemdag1991_height(age = df$age, si = df$si, pars = df)
    if (any(!is.finite(out))) {
      # nocov start
      # Defensive: the model [4] height form is bounded and finite for any age > 0
      # and si > 1.3 m (the validated domain), so this abort is not reached via the
      # public API. Kept as a guard against pathological coefficient edits.
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_alemdag1991}.",
        "i" = "Check inputs and model coefficients."
      ))
      # nocov end
    }
    return(dplyr::tibble(height = out))
  }

  out <- .alemdag1991_si(age = df$age, height = df$height, pars = df)
  if (any(!is.finite(out))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_alemdag1991}.",
      "i" = "Check inputs and model coefficients."
    ))
  }
  dplyr::tibble(si = out)
}


# internal
.alemdag1991_prepare <- function(age, x, x_name) {
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
  # Height and site index must exceed breast height (1.30 m) for the model to be
  # defined; the reciprocal / power forms are otherwise non-finite.
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 1.3, allow_na = FALSE)

  pars <- .alemdag1991_parameters()

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    c1 = pars$c1,
    c2 = pars$c2,
    c4 = pars$c4,
    c5 = pars$c5,
    b1 = pars$b1,
    b2 = pars$b2,
    b4 = pars$b4,
    b5 = pars$b5,
    base_age = pars$base_age
  )

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
  }

  out
}


# internal
# Total tree height at breast-height `age` given site index (Alemdag 1991,
# model [4] with the constraint constant k from model [5]). Vectorized.
.alemdag1991_height <- function(age, si, pars) {
  b1 <- pars$b1
  b2 <- pars$b2
  b4 <- pars$b4
  b5 <- pars$b5
  a1 <- pars$base_age
  sd <- si - 1.3
  b4s <- b4 * sd^b5
  k <- 1 - (sd / (b1 * sd^b2))^(1 / b4s)
  1.3 + b1 * sd^b2 * (1 - k^(age / a1))^b4s
}


# internal
# Site index (total height at base age) given breast-height `age` and total
# height (Alemdag 1991, model [9] with the constraint constant m from model
# [8]). Vectorized.
.alemdag1991_si <- function(age, height, pars) {
  c1 <- pars$c1
  c2 <- pars$c2
  c4 <- pars$c4
  c5 <- pars$c5
  a1 <- pars$base_age
  hd <- height - 1.3
  c4h <- c4 * hd^c5
  m <- 1 - (1 / (c1 * hd^(1 + c2)))^(1 / c4h)
  1.3 + 1 / (c1 * hd^c2 * (1 - m^(age / a1))^c4h)
}


# internal
.alemdag1991_parameters <- function() {
  pars <- .get_internal_data("parameters_Alemdag1991") |>
    dplyr::as_tibble()

  req <- c(
    "Species",
    "c1",
    "c2",
    "c4",
    "c5",
    "b1",
    "b2",
    "b4",
    "b5",
    "base_age"
  )
  assert_required_cols(pars, req, object = "parameters_Alemdag1991")

  pars
}
