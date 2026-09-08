#' Nigh (1998) growth-intercept site index model for interior western hemlock
#'
#' Implementation of the Nigh (1998) growth-intercept site index model for
#' western hemlock (\code{TSUG.HET}) in the interior of British Columbia. Site
#' index is estimated from the growth intercept (early height growth) with a
#' separate parameter pair for each breast-height age.
#'
#' The model form (eq. 4) is age-specific:
#' \deqn{SI = 1.3 + e^{b_1(A)} \times GI^{b_2(A)}}
#' where `A` is breast-height age and `GI` is the growth intercept (cm/year).
#' Following the source, parameter \eqn{b_1} was fitted on the log scale and
#' enters the model as \eqn{e^{b_1}} (Ratkowsky transformation); the tabulated
#' \eqn{b_1} values (stored as `b1_log`) are therefore exponentiated internally.
#'
#' Coefficients are tabulated (Table 3) for breast-height ages 5, 10, 20, 30,
#' 40, and 50; `age` must be one of these values.
#'
#' @param age Numeric vector. Breast-height age (years); one of 5, 10, 20, 30,
#'   40, 50.
#' @param gi Numeric vector. Growth intercept (cm/year).
#'
#' @return A tibble with one column:
#' \describe{
#'   \item{si}{Predicted site index (m, base age 50 years at breast height).}
#' }
#'
#' @references
#' Nigh, G.D. (1998). A system for estimating height and site index of western
#' hemlock in the interior of British Columbia. The Forestry Chronicle 74(4):
#' 588--596.
#'
#' @examples
#' si_nigh1998_gi(
#'   age = c(5, 20, 50),
#'   gi = c(30, 18, 12)
#' )
#'
#' @export
si_nigh1998_gi <- function(age, gi) {
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

  pars <- .get_internal_data("parameters_Nigh1998_gi") |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$bha, .keep_all = TRUE)

  req <- c("bha", "b1_log", "b2")
  assert_required_cols(pars, req, object = "parameters_Nigh1998_gi")

  valid_ages <- sort(unique(pars$bha))
  age_num <- as.numeric(age)
  if (!all(age_num %in% valid_ages)) {
    cli::cli_abort(
      "{.arg age} must be one of {paste(valid_ages, collapse = ', ')} for {.fn si_nigh1998_gi}."
    )
  }

  out <- dplyr::tibble(
    bha = age_num,
    gi = as.numeric(gi)
  ) |>
    dplyr::left_join(pars, by = "bha")

  si <- with(out, 1.3 + exp(b1_log) * (gi^b2))

  if (any(!is.finite(si))) {
    # nocov start
    cli::cli_abort(c(
      "Non-finite site-index prediction generated in {.fn si_nigh1998_gi}.",
      "i" = "Check inputs and model domain."
    ))
    # nocov end
  }

  dplyr::tibble(si = si)
}
