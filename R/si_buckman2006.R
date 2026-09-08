#' Buckman et al. (2006) site index model for red pine
#'
#' Implementation of the Appendix III site-index equations from
#' Buckman et al. (2006) for red pine (\code{PINU.RES}) in the Lake States.
#' Appendix III presents this model as an improved version of the Lundgren and
#' Dolid red-pine height function implemented in [si_lundgrendolid1970()].
#' Buckman et al. note two imperfections in the earlier Lundgren-Dolid form:
#' height predicted at age 50 was slightly below site index rather than equal
#' to it exactly, and the curve overpredicted heights for young stands below
#' age 20. To address this, they refit the age-20+ equation with the constraint
#' that height at age 50 equals site index, and they replace the younger-age
#' portion with a separate polynomial curve that joins smoothly with the
#' older-age curve at age 20.
#'
#' \strong{Species coverage:} \code{PINU.RES}.
#'
#' \strong{Geographic use:} Ontario
#'
#' \strong{Age definition note:} `age` is stand age from seed (years)
#'
#' \strong{Height definition note:} `height` is total stand height (m).
#'
#' \strong{Base-age note:} site index is total height at 50 years.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' The source model is piecewise:
#' \itemize{
#'   \item for `age < 20`: \eqn{H = SI (k t^2 - m t^4)}
#'   \item for `age >= 20`: \eqn{H = A SI (1 - e^{-Bt})^C}
#' }
#'
#' The constrained Buckman refit uses `A = 1.8604`, `B = 0.020928`, and
#' `C = 1.4349`. The younger-age correction uses `k = 1.41876e-3` and
#' `m = 1.05304e-6`, chosen so the two segments join smoothly at age 20.
#'
#' Inputs and outputs are metric; the original equations are in imperial units,
#' so the function converts internally.
#'
#' @param age Numeric vector. Stand age from seed (years), with `age > 0`.
#' @param height Optional numeric vector. Total stand height (m). If provided,
#'   `si` is predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years from
#'   seed). If provided, `height` is predicted.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted total stand height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m at total age 50), returned when input `height` is provided.}
#' }
#'
#' @references
#' Buckman, R. E., et al. (2006). Growth and yield of red pine in the Lake
#' States. USDA Forest Service.
#'
#' Lundgren, A. L., & Dolid, W. A. (1970). Biological growth functions describe
#' published site index curves for Lake States timber species. Research Paper
#' NC-36. St. Paul, MN: U.S. Department of Agriculture, Forest Service, North
#' Central Forest Experiment Station.
#'
#' @examples
#' # Predict site index from age + height
#' si_buckman2006(
#'   age = c(15, 25, 50),
#'   height = c(4, 10, 18)
#' )
#'
#' # Predict height from age + site index
#' si_buckman2006(
#'   age = c(15, 25, 50),
#'   si = c(14, 16, 18)
#' )
#'
#'
#' @export
si_buckman2006 <- function(age, height = NULL, si = NULL) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .buckman2006_prepare(
    age = age,
    x = x,
    x_name = x_name
  )

  if (mode == "predict_height") {
    h_ft <- .buckman2006_height(age = df$age, si_ft = df$si_ft)

    if (any(!is.finite(h_ft))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_buckman2006}.",
        "i" = "Check inputs and model domain."
      ))
    }
    if (any(h_ft < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_buckman2006}.",
        "i" = "Check inputs and model domain."
      ))
    }

    return(dplyr::tibble(height = h_ft / 3.28084))
  }

  si_ft <- .buckman2006_si(age = df$age, height_ft = df$height_ft)

  if (any(!is.finite(si_ft))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_buckman2006}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(si_ft < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_buckman2006}.",
      "i" = "Check inputs and model domain."
    ))
  }

  dplyr::tibble(si = si_ft / 3.28084)
}


# internal
.buckman2006_height <- function(age, si_ft) {
  rel <- .buckman2006_relative_height(age = age)
  si_ft * rel
}


# internal
.buckman2006_si <- function(age, height_ft) {
  rel <- .buckman2006_relative_height(age = age)

  if (any(!is.finite(rel)) || any(rel <= 0)) {
    cli::cli_abort(c(
      "Invalid relative height multiplier generated in {.fn si_buckman2006}.",
      "i" = "Check age inputs."
    ))
  }

  height_ft / rel
}


# internal
# Constrained Buckman refit constants (A, B, C) plus younger-age polynomial
# constants (k, m); Appendix III. Verified against the source publication.
.buckman2006_constants <- function() {
  pars <- .get_internal_data("parameters_Buckman2006") |>
    dplyr::as_tibble()
  assert_required_cols(
    pars,
    c("k", "m", "A", "B", "C"),
    object = "parameters_Buckman2006"
  )
  list(
    k = pars$k[[1]],
    m = pars$m[[1]],
    A = pars$A[[1]],
    B = pars$B[[1]],
    C = pars$C[[1]]
  )
}


# internal
.buckman2006_relative_height <- function(age) {
  young <- age < 20
  rel <- numeric(length(age))

  cf <- .buckman2006_constants()
  k <- cf$k
  m <- cf$m
  A <- cf$A
  B <- cf$B
  C <- cf$C

  rel[young] <- k * age[young]^2 - m * age[young]^4
  rel[!young] <- A * (1 - exp(-B * age[!young]))^C

  rel
}


# internal
.buckman2006_bh_age <- function(si_ft) {
  cf <- .buckman2006_constants()
  k <- cf$k
  m <- cf$m

  disc <- k^2 - 18 * m / si_ft
  out <- sqrt((k - sqrt(disc)) / (2 * m))

  bad <- !is.finite(out) |
    !is.finite(disc) |
    disc < 0 |
    si_ft <= 0

  out[bad] <- NA_real_
  out
}


# internal
.buckman2006_prepare <- function(age, x, x_name) {
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

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x)
  )

  if (identical(x_name, "height")) {
    out$height_ft <- out$x * 3.28084
  } else {
    out$si_ft <- out$x * 3.28084
  }

  out
}
