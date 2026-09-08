#' Nigh (1998) years-to-breast-height model for interior western hemlock
#'
#' Implementation of the Nigh (1998) years-to-breast-height (YTBH) model for
#' western hemlock (\code{TSUG.HET}) in the interior of British Columbia. The
#' model estimates the number of years a top-height tree takes to grow from
#' germination to breast height (1.3 m), which is used to convert breast-height
#' age to total age (and vice versa).
#'
#' The source equation (eq. 5) is:
#' \deqn{YTBH = 446.6 \times SI^{-1.432}}
#'
#' Provide exactly one of `si` or `ytbh`:
#' \itemize{
#'   \item If `si` is provided, the function predicts `ytbh`.
#'   \item If `ytbh` is provided, the function predicts `si` by inverting the
#'     equation: \eqn{SI = (YTBH / 446.6)^{1 / -1.432}}.
#' }
#'
#' @param si Optional numeric vector. Site index (m, base age 50 years at breast
#'   height). If provided, `ytbh` is predicted.
#' @param ytbh Optional numeric vector. Years to breast height (years). If
#'   provided, `si` is predicted.
#'
#' @return A tibble with a single column:
#' \describe{
#'   \item{ytbh}{Predicted years to breast height (years), returned when `si`
#'     is provided.}
#'   \item{si}{Predicted site index (m), returned when `ytbh` is provided.}
#' }
#'
#' @references
#' Nigh, G.D. (1998). A system for estimating height and site index of western
#' hemlock in the interior of British Columbia. The Forestry Chronicle 74(4):
#' 588--596.
#'
#' @examples
#' # Predict years-to-breast-height from site index
#' ytbh_nigh1998(si = c(10, 15, 20))
#'
#' # Invert: predict site index from years-to-breast-height
#' ytbh_nigh1998(ytbh = c(8, 4, 3))
#'
#' @export
ytbh_nigh1998 <- function(si = NULL, ytbh = NULL) {
  if (xor(is.null(si), is.null(ytbh)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg si} or {.arg ytbh}.")
  }

  b0 <- 446.6
  b1 <- -1.432

  if (is.null(ytbh)) {
    n <- length(si)
    if (n == 0L) {
      cli::cli_abort("{.arg si} must have length > 0.")
    }
    assert_numeric_vec(si, "si", finite = TRUE, gt = 0, allow_na = FALSE)

    ytbh_pred <- b0 * as.numeric(si)^b1
    if (any(!is.finite(ytbh_pred))) {
      # nocov start
      cli::cli_abort(c(
        "Non-finite years-to-breast-height prediction generated in {.fn ytbh_nigh1998}.",
        "i" = "Check inputs and model domain."
      ))
      # nocov end
    }
    return(dplyr::tibble(ytbh = ytbh_pred))
  }

  n <- length(ytbh)
  if (n == 0L) {
    cli::cli_abort("{.arg ytbh} must have length > 0.")
  }
  assert_numeric_vec(ytbh, "ytbh", finite = TRUE, gt = 0, allow_na = FALSE)

  si_pred <- (as.numeric(ytbh) / b0)^(1 / b1)
  if (any(!is.finite(si_pred))) {
    # nocov start
    cli::cli_abort(c(
      "Non-finite site-index prediction generated in {.fn ytbh_nigh1998}.",
      "i" = "Check inputs and model domain."
    ))
    # nocov end
  }
  dplyr::tibble(si = si_pred)
}
