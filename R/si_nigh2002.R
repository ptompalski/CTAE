#' Nigh, Krestov and Klinka (2002) trembling aspen height-age (site index) model for British Columbia
#'
#' Unified, vectorized implementation of the logistic height-age (site index)
#' model in Nigh, Krestov and Klinka (2002) for trembling aspen
#' (\emph{Populus tremuloides}) in British Columbia.
#'
#' \strong{Model scope (species coverage):} trembling aspen, NFI code
#' \code{POPU.TRE}.
#'
#' \strong{Age definition note:} `age` is breast-height age (BHA, years). The
#' model is conditioned to return `height = si` exactly at BHA 50.
#'
#' \strong{Base-age note:} site index is site height at breast-height age 50.
#'
#' \strong{Base vs. extended model:} the paper fits one logistic form (eq. 1)
#' \deqn{HT = 1.3 + (SI - 1.3) \times
#'   \frac{1 + e^{a_0 + a_1 \ln(49.5) + a_2 \ln(SI - 1.3)}}
#'        {1 + e^{a_0 + a_1 \ln(BHA - 0.5) + a_2 \ln(SI - 1.3)}}}
#' where \eqn{a_0, a_1, a_2} are either the general **base-model** coefficients
#' (used anywhere in BC, or when the biogeoclimatic zone is unknown) or the
#' **extended-model** per-zone coefficients calibrated to six BEC zones. Supply
#' `bec_zone` to use the extended model; leave it `NULL` (the default) for the
#' base model.
#'
#' Because \eqn{SI} appears both as a multiplier and inside a logarithm, eq. 1
#' has no closed-form inverse in \eqn{SI}; when predicting site index the
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
#' @param bec_zone Optional character vector selecting the extended (per-zone)
#'   model. One of `"BWBS"`, `"ICH"`, `"IDF"`, `"MS"`, `"SBPS"`, `"SBS"`. When
#'   `NULL` (default) the general base model is used.
#'
#' @return A tibble with a single column:
#' \describe{
#'   \item{height}{Predicted site height (m), returned when `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when `height` is provided.}
#' }
#'
#' @references
#' Nigh, G.D., Krestov, P.V., and Klinka, K. (2002). Trembling aspen height-age
#' models for British Columbia. Northwest Science 76(3): 202--212.
#'
#' @examples
#' # Base model: predict height from age + site index
#' si_nigh2002(age = c(25, 50, 80), si = c(12, 18, 24))
#'
#' # Base model: predict site index from age + height
#' si_nigh2002(age = c(25, 50, 80), height = c(8, 18, 26))
#'
#' # Extended model (per BEC zone)
#' si_nigh2002(age = c(30, 60), si = c(15, 20), bec_zone = "BWBS")
#'
#' @export
si_nigh2002 <- function(age, height = NULL, si = NULL, bec_zone = NULL) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .nigh2002_prepare(
    age = age,
    x = x,
    bec_zone = bec_zone,
    x_name = x_name
  )

  if (mode == "predict_height") {
    out <- .nigh2002_height(bha = df$age, si = df$si, df)
    if (any(!is.finite(out))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_nigh2002}.",
        "i" = "Check inputs and model coefficients."
      ))
    }
    return(dplyr::tibble(height = out))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .nigh2002_si_from_height_one(
        bha = df$age[[i]],
        height = df$height[[i]],
        pars = df[i, , drop = FALSE]
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_nigh2002}.",
      "i" = "Check inputs and model coefficients."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.nigh2002_prepare <- function(age, x, bec_zone, x_name) {
  n <- max(length(age), length(x), length(bec_zone %||% character(0)))
  if (n == 0L) {
    # nocov start
    # Defensive: unreachable via the public API, which requires `age` plus one of
    # `height`/`si` (both length > 0). Kept as a guard for direct internal calls.
    cli::cli_abort("{.arg age} must have length > 0.")
    # nocov end
  }

  if (is.null(bec_zone)) {
    model <- "base"
    zone <- NA_character_
  } else {
    model <- "extended"
    zone <- as.character(bec_zone)
  }

  recycled <- assert_len_compat(
    age = age,
    x = x,
    zone = zone,
    .n = n,
    .recycle = TRUE
  )
  age <- recycled$age
  x <- recycled$x
  zone <- recycled$zone

  assert_numeric_vec(age, "age", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 0, allow_na = FALSE)

  pars <- .nigh2002_parameters()
  valid_zones <- pars$bec_zone[pars$model == "extended"]

  if (identical(model, "extended")) {
    bad <- setdiff(unique(zone), valid_zones)
    if (length(bad) > 0) {
      cli::cli_abort(c(
        "Unknown {.arg bec_zone}: {paste(bad, collapse = ', ')}.",
        "i" = "Valid zones: {paste(valid_zones, collapse = ', ')}."
      ))
    }
    key <- data.frame(model = "extended", bec_zone = zone)
  } else {
    key <- data.frame(model = "base", bec_zone = NA_character_)
  }

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    model = key$model,
    bec_zone = key$bec_zone
  ) |>
    dplyr::left_join(
      pars[, c("model", "bec_zone", "a0", "a1", "a2")],
      by = c("model", "bec_zone")
    )

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
  }

  out
}


# internal
# Site height at BHA given site index (eq. 1). Vectorized over inputs sharing a
# common length; `pars` supplies a0/a1/a2 columns.
.nigh2002_height <- function(bha, si, pars) {
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
.nigh2002_si_from_height_one <- function(bha, height, pars) {
  if (!is.finite(bha) || !is.finite(height) || bha <= 0.5 || height <= 1.3) {
    return(NaN)
  }
  # At BHA 50 the model is conditioned so height == si exactly.
  if (isTRUE(all.equal(bha, 50))) {
    return(height)
  }
  f <- function(s) .nigh2002_height(bha = bha, si = s, pars = pars) - height
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
.nigh2002_parameters <- function() {
  pars <- .get_internal_data("parameters_Nigh2002") |>
    dplyr::as_tibble()

  req <- c("Species", "model", "bec_zone", "a0", "a1", "a2")
  assert_required_cols(pars, req, object = "parameters_Nigh2002")

  pars
}
