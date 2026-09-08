#' Goelz and Burk (1992) base-age invariant site index model for jack pine in north central Ontario
#'
#' Unified, vectorized implementation of the base-age invariant version of the
#' Chapman-Richards difference equation (eq. 16) published in Goelz and Burk
#' (1992) for jack pine (\emph{Pinus banksiana}) in north central Ontario.
#'
#' \strong{Model scope (species coverage):} jack pine (\code{PINU.BAN}).
#'
#' \strong{Age definition note:} `age` is breast-height age (years). Heights are
#' referenced to a breast height of 1.3 m.
#'
#' \strong{Base-age note:} the equation is base-age invariant. Site index is
#' returned as the predicted height (m) at a breast-height age of 50 years, the
#' reference base age used throughout Goelz and Burk (1992).
#'
#' \strong{Domain note:} the equation was fitted to 109 plots (32 held out for
#' validation) in north central Ontario. Breast-height ages of roughly 20-80
#' years represent the range likely applied to jack pine in the region.
#'
#' The model form (eq. 16) predicts height \eqn{\hat{H}_2} at breast-height age
#' \eqn{A_2} from a known height \eqn{H_1} at breast-height age \eqn{A_1}:
#' \deqn{\hat{H}_2 = 1.3 + (H_1 - 1.3)
#'   \frac{\left[1 - \exp\left(-b_1 (H_1/A_1)^{b_2} A_1^{b_3} A_2\right)\right]^{b_4}}
#'        {\left[1 - \exp\left(-b_1 (H_1/A_1)^{b_2} A_1^{b_3} A_1\right)\right]^{b_4}}.}
#' Because the equation is base-age invariant, the same form is used in both
#' directions: site index is obtained by setting \eqn{A_2 = 50}, and height at a
#' given age is obtained by setting \eqn{H_1 = SI}, \eqn{A_1 = 50}.
#'
#' \strong{Note on approximate invariance:} eq. 16 was not constrained to be
#' exactly base-age invariant (Goelz and Burk 1992, criterion 7). Predicting
#' height from site index and then predicting site index back from that height
#' therefore need not recover the original value exactly; the round-trip
#' discrepancy is small (the source reports curve differences across base ages of
#' less than 0.5 m).
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `si` is provided, the function predicts `height` at `age`.
#'   \item If `height` is provided, the function predicts `si` (height at base
#'     age 50) from the observed (`age`, `height`) pair.
#' }
#'
#' This model is specific to jack pine (`PINU.BAN`); the species is fixed and
#' there is no `species` argument.
#'
#' @param age Numeric vector. Breast-height age (years), with `age > 0`.
#' @param height Optional numeric vector. Site height (m), with `height > 1.3`.
#'   If provided, `si` is predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at breast
#'   height), with `si > 1.3`. If provided, `height` is predicted.
#'
#' @return A tibble with a single column:
#' \describe{
#'   \item{height}{Predicted site height (m), returned when `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when `height` is provided.}
#' }
#'
#' @references
#' Goelz, J.C.G., and Burk, T.E. (1992). Development of a well-behaved site index
#' equation: jack pine in north central Ontario. Canadian Journal of Forest
#' Research 22: 776--784.
#'
#' @examples
#' # Predict height from age + site index
#' si_goelz1992(age = c(20, 50, 80), si = c(16, 16, 16))
#'
#' # Predict site index from age + height
#' si_goelz1992(age = c(20, 50, 80), height = c(8.83, 16, 19.82))
#'
#' @export
si_goelz1992 <- function(age, height = NULL, si = NULL) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .goelz1992_prepare(age = age, x = x, x_name = x_name)

  pars <- df[1, , drop = FALSE]
  base_age <- pars$base_age[[1]]

  if (mode == "predict_height") {
    # Height at `age` given site index (height at base age 50).
    h <- .goelz1992_predict_one(
      h1 = df$si,
      a1 = base_age,
      a2 = df$age,
      pars = pars
    )

    if (any(!is.finite(h))) {
      # nocov start
      # Defensive: eq. 16 is numerically well-behaved for valid (age, si) with
      # si > 1.3, age > 0; no such inputs produce a non-finite height.
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_goelz1992}.",
        "i" = "Check inputs and model coefficients."
      ))
      # nocov end
    }

    return(dplyr::tibble(height = h))
  }

  # Site index = height at base age 50 given observed (age, height).
  si_est <- .goelz1992_predict_one(
    h1 = df$height,
    a1 = df$age,
    a2 = base_age,
    pars = pars
  )

  if (any(!is.finite(si_est))) {
    # nocov start
    # Defensive: eq. 16 is numerically well-behaved for valid (age, height) with
    # height > 1.3, age > 0; no such inputs produce a non-finite site index.
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_goelz1992}.",
      "i" = "Check inputs and model coefficients."
    ))
    # nocov end
  }

  dplyr::tibble(si = si_est)
}


# internal
.goelz1992_prepare <- function(age, x, x_name) {
  n <- max(length(age), length(x))
  if (n == 0L) {
    # nocov start
    # Defensive: unreachable via the public API, which requires `age` plus one
    # of `height`/`si` (all length > 0).
    cli::cli_abort("{.arg age} must have length > 0.")
    # nocov end
  }

  recycled <- assert_len_compat(age = age, x = x, .n = n, .recycle = TRUE)
  age <- recycled$age
  x <- recycled$x

  assert_numeric_vec(age, "age", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 1.3, allow_na = FALSE)

  # Single-species model (jack pine, PINU.BAN); parameters are a fixed one-row
  # table.
  pars <- .goelz1992_parameters()

  out <- dplyr::tibble(
    age = as.numeric(age),
    b1 = pars$b1[[1]],
    b2 = pars$b2[[1]],
    b3 = pars$b3[[1]],
    b4 = pars$b4[[1]],
    base_age = pars$base_age[[1]]
  )

  if (identical(x_name, "height")) {
    out$height <- as.numeric(x)
  } else {
    out$si <- as.numeric(x)
  }

  out
}


# internal
# Base-age invariant difference equation (eq. 16). Predicts height at age `a2`
# from a known height `h1` at age `a1`. Vectorized over h1, a1, a2.
.goelz1992_predict_one <- function(h1, a1, a2, pars) {
  b1 <- pars$b1[[1]]
  b2 <- pars$b2[[1]]
  b3 <- pars$b3[[1]]
  b4 <- pars$b4[[1]]

  base <- b1 * (h1 / a1)^b2 * a1^b3
  num <- (1 - exp(-base * a2))^b4
  den <- (1 - exp(-base * a1))^b4

  1.3 + (h1 - 1.3) * num / den
}


# internal
.goelz1992_parameters <- function() {
  pars <- .get_internal_data("parameters_Goelz1992") |>
    dplyr::as_tibble()

  req <- c("Species", "b1", "b2", "b3", "b4", "base_age")
  assert_required_cols(pars, req, object = "parameters_Goelz1992")

  pars
}
