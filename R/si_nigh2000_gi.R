#' Nigh (2000) growth-intercept site index model for interior western redcedar
#'
#' Implementation of the Nigh (2000) growth-intercept model for
#' interior western redcedar (\code{THUJ.PLI}).
#'
#' The model form is age-specific:
#' \deqn{SI = 1.3 + b_0(A)\times GI^{b_1(A)}}
#' where `A` is breast-height age (1 to 50) and `GI` is growth intercept
#' (cm/year).
#'
#' @param age Numeric vector. Breast-height age (years), expected in \code{[1, 50]}.
#' @param gi Numeric vector. Growth intercept (cm/year).
#'
#' @return A tibble with one column:
#' \describe{
#'   \item{si}{Predicted site index (m, base age 50 years at breast height).}
#' }
#'
#' @references
#' Nigh, G. D. (2000). Western redcedar site index models for the interior of
#' British Columbia. British Columbia Ministry of Forests, Research Report 18.
#'
#' @examples
#' si_nigh2000_gi(
#'   age = c(5, 15, 30, 45),
#'   gi = c(12, 8, 6, 5)
#' )
#'
#' @export
si_nigh2000_gi <- function(age, gi) {
  n <- max(length(age), length(gi))
  if (n == 0L) {
    cli::cli_abort("{.arg age} must have length > 0.")
  }

  recycled <- assert_len_compat(
    age = age,
    gi = gi,
    .n = n,
    .recycle = TRUE
  )
  age <- recycled$age
  gi <- recycled$gi

  assert_numeric_vec(age, "age", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(gi, "gi", finite = TRUE, gt = 0, allow_na = FALSE)

  age_int <- as.integer(round(as.numeric(age)))
  if (any(abs(as.numeric(age) - age_int) > 1e-8)) {
    cli::cli_abort(
      "{.arg age} must contain integer breast-height ages for {.fn si_nigh2000_gi}."
    )
  }
  if (any(age_int < 1L | age_int > 50L)) {
    cli::cli_abort("{.arg age} must be in [1, 50] for {.fn si_nigh2000_gi}.")
  }

  pars <- .get_internal_data("parameters_Nigh2000_gi") |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$age, .keep_all = TRUE)

  req <- c("age", "b0", "b1")
  assert_required_cols(pars, req, object = "parameters_Nigh2000_gi")

  out <- dplyr::tibble(
    age = age_int,
    gi = as.numeric(gi)
  ) |>
    dplyr::left_join(pars, by = "age")

  # Unreachable with current internal data: age is constrained to [1, 50]
  # and parameters_Nigh2000_gi contains complete rows for ages 1..50.
  # nocov start
  if (anyNA(out$b0) || anyNA(out$b1)) {
    bad_age <- unique(out$age[is.na(out$b0) | is.na(out$b1)])
    cli::cli_abort(
      "No Nigh2000 growth-intercept parameters found for age(s): {paste(bad_age, collapse = ', ')}."
    )
  }
  # nocov end

  si <- with(out, 1.3 + b0 * (gi^b1))

  if (any(!is.finite(si))) {
    cli::cli_abort(c(
      "Non-finite site-index prediction generated in {.fn si_nigh2000_gi}.",
      "i" = "Check inputs and model domain."
    ))
  }
  # Unreachable with current valid domain: gi > 0 and fitted b0/b1 are > 0,
  # so 1.3 + b0 * gi^b1 cannot be negative.
  # nocov start
  if (any(si < 0)) {
    cli::cli_abort(c(
      "Negative site-index prediction generated in {.fn si_nigh2000_gi}.",
      "i" = "Check inputs and model domain."
    ))
  }
  # nocov end

  dplyr::tibble(si = si)
}
