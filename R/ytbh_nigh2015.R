#' Nigh (2015) years-to-breast-height model for Engelmann spruce
#'
#' Implementation of the Nigh (2015) years-to-breast-height model for
#' Engelmann spruce (\code{PICE.ENG}) in the Engelmann Spruce -- Subalpine
#' Fir (ESSF) biogeoclimatic zone of British Columbia.
#'
#' The source equation (model 2 in Nigh 2015) is:
#' \deqn{YTBH = 4.465 + 154.6 / SI}
#'
#' This model was developed specifically for Engelmann spruce and is
#' recommended in place of the natural-stand white spruce years-to-breast-height
#' model that had previously been used for the species. It may be extrapolated
#' into zones neighbouring the ESSF provided the species is truly Engelmann
#' spruce; for the white x Engelmann cross, the white spruce models should be
#' used instead.
#'
#' @param si Numeric vector. Site index (m, base age 50 years at breast height).
#'
#' @return A tibble with one column:
#' \describe{
#'   \item{ytbh}{Predicted years to breast height (years).}
#' }
#'
#' @references
#' Nigh, G. D. (2015). Years-to-breast-height model for Engelmann spruce in the
#' Engelmann Spruce -- Subalpine Fir biogeoclimatic zone. Province of British
#' Columbia, Victoria, B.C. Extension Note 115.
#'
#' @examples
#' ytbh_nigh2015(
#'   si = c(10, 15, 20)
#' )
#'
#' @export
ytbh_nigh2015 <- function(si) {
  n <- length(si)
  if (n == 0L) {
    cli::cli_abort("{.arg si} must have length > 0.")
  }

  assert_numeric_vec(si, "si", finite = TRUE, gt = 0, allow_na = FALSE)

  ytbh <- 4.465 + 154.6 / as.numeric(si)

  # nocov start
  # Unreachable with validated inputs: `si` is finite and > 0, so
  # 4.465 + 154.6/si is always finite and strictly positive.
  if (any(!is.finite(ytbh))) {
    cli::cli_abort(c(
      "Non-finite years-to-breast-height prediction generated in {.fn ytbh_nigh2015}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(ytbh < 0)) {
    cli::cli_abort(c(
      "Negative years-to-breast-height prediction generated in {.fn ytbh_nigh2015}.",
      "i" = "Check inputs and model domain."
    ))
  }
  # nocov end

  dplyr::tibble(ytbh = ytbh)
}
