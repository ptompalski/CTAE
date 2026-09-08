#' Carmean, Niznowski and Hazenberg (2001) polymorphic site index model for northern Ontario
#'
#' Unified, vectorized implementation of the Newnham (1988) constrained
#' polymorphic height-age (site index) model published in Carmean, Niznowski
#' and Hazenberg (2001) for jack pine (\emph{Pinus banksiana}) in northern
#' Ontario.
#'
#' \strong{Model scope (species coverage):} jack pine (\code{PINU.BAN}).
#'
#' \strong{Age definition note:} `age` is breast-height age (years). Curves
#' start at breast height (0 years at BH) and the model is constrained so that
#' `height = si` exactly at breast-height age 50.
#'
#' \strong{Base-age note:} site index is total height (m) of dominant and
#' codominant trees at 50 years breast-height age.
#'
#' \strong{Domain note:} the recommended equation was fitted to data 100 years
#' and less breast-height age, combining 383 plots across the Northwestern,
#' North Central, Northern and Northeastern regions of northern Ontario. Site
#' index ranged 7.6-22.4 m and breast-height age 50-157 years in the fitting
#' data (Table 1).
#'
#' The model form (eq. 1) is
#' \deqn{\hat{H} = 1.3 + b_1 (SI - 1.3)^{b_2}
#'   \left[1 - k^{Age/50}\right]^{b_3 (SI - 1.3)^{b_4}}}
#' with
#' \deqn{k = 1 - \left[\frac{SI - 1.3}{b_1 (SI - 1.3)^{b_2}}\right]^
#'   {1 / (b_3 (SI - 1.3)^{b_4})}.}
#' Because \eqn{SI} appears in several nonlinear positions the model has no
#' closed-form inverse in \eqn{SI}; when predicting site index the equation is
#' solved numerically.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `si` is provided, the function predicts `height`.
#'   \item If `height` is provided, the function predicts `si`.
#' }
#'
#' This model is specific to jack pine (`PINU.BAN`); the species is fixed and
#' there is no `species` argument.
#'
#' @param age Numeric vector. Breast-height age (years), with `age > 0`.
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
#' Carmean, W.H., Niznowski, G.P., and Hazenberg, G. (2001). Polymorphic site
#' index curves for jack pine in northern Ontario. The Forestry Chronicle
#' 77(1): 141--150.
#'
#' Newnham, R.M. (1988). A modification of the Ek-Payandeh nonlinear regression
#' model for site index curves. Canadian Journal of Forest Research 18:
#' 115--120.
#'
#' @examples
#' # Predict height from age + site index
#' si_carmean2001(age = c(25, 50, 80), si = c(12, 16, 20))
#'
#' # Predict site index from age + height
#' si_carmean2001(age = c(25, 50, 80), height = c(8, 16, 22))
#'
#' @export
si_carmean2001 <- function(age, height = NULL, si = NULL) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .carmean2001_prepare(
    age = age,
    x = x,
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
          fn = "si_carmean2001"
        )
      },
      numeric(1)
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_carmean2001}.",
        "i" = "Check inputs and model coefficients."
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
        fn = "si_carmean2001"
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_carmean2001}.",
      "i" = "Check inputs and model coefficients."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.carmean2001_prepare <- function(age, x, x_name) {
  n <- max(length(age), length(x))
  if (n == 0L) {
    # nocov start
    # Defensive: unreachable via the public API, which requires `age` plus one
    # of `height`/`si` (all length > 0).
    cli::cli_abort("{.arg age} must have length > 0.")
    # nocov end
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

  # Single-species model (jack pine, PINU.BAN); parameters are a fixed one-row
  # table.
  pars <- .carmean2001_parameters()

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    b1 = pars$b1[[1]],
    b2 = pars$b2[[1]],
    b3 = pars$b3[[1]],
    b4 = pars$b4[[1]],
    base_age = pars$base_age[[1]]
  )

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
  }

  out
}


# internal
.carmean2001_parameters <- function() {
  pars <- .get_internal_data("parameters_Carmean2001") |>
    dplyr::as_tibble()

  req <- c("Species", "b1", "b2", "b3", "b4", "base_age")
  assert_required_cols(pars, req, object = "parameters_Carmean2001")

  pars
}
