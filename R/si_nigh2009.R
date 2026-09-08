#' Nigh, Thomas, Yearsley and Wang (2009) paper birch height-age (site index) model for British Columbia
#'
#' Unified, vectorized implementation of the log-logistic height-age (site
#' index) model in Nigh, Thomas, Yearsley and Wang (2009) for paper birch
#' (\emph{Betula papyrifera}) in British Columbia.
#'
#' \strong{Model scope (species coverage):} paper birch, NFI code
#' \code{BETU.PAP}.
#'
#' \strong{Age definition note:} `age` is breast-height age (BHA, years). The
#' model is conditioned to return `height = si` exactly at BHA 50.
#'
#' \strong{Base-age note:} site index is site height at breast-height age 50.
#'
#' \strong{Model variants:} the paper fits three variants of one log-logistic
#' form (eq. 1)
#' \deqn{HT = 1.3 + (SI - 1.3) \times
#'   \frac{1 + e^{a_0 + a_1 \ln(49.5) + a_2 \ln(SI - 1.3)}}
#'        {1 + e^{a_0 + a_1 \ln(BHA - 0.5) + a_2 \ln(SI - 1.3)}}}
#' differing only in their coefficients:
#' \describe{
#'   \item{`model = 1`}{Base log-logistic fit (Table 2). Default.}
#'   \item{`model = 2`}{Operational form of the mixed model. Recommended by the
#'     authors when the biogeoclimatic zone is unknown or is not ICH/IDF/SBS.}
#'   \item{`model = 3`}{Zonal indicator-variable model. Only \eqn{a_1} carries a
#'     significant zone effect (SBS vs. ICH/IDF). Supply `bec_zone` (one of
#'     `"ICH"`, `"IDF"`, `"SBS"`); recommended for those zones.}
#' }
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
#' @param model Model variant: `1` (base, default), `2` (operational), or `3`
#'   (zonal). See Details.
#' @param bec_zone Optional character vector selecting the zonal (`model = 3`)
#'   coefficients. One of `"ICH"`, `"IDF"`, `"SBS"`. Required when `model = 3`
#'   and ignored otherwise.
#'
#' @return A tibble with a single column:
#' \describe{
#'   \item{height}{Predicted site height (m), returned when `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when `height` is provided.}
#' }
#'
#' @references
#' Nigh, G.D., Thomas, K.D., Yearsley, K., and Wang, J. (2009). Site-dependent
#' height-age models for paper birch in British Columbia. Northwest Science
#' 83(3): 253--261. \doi{10.3955/046.083.0308}
#'
#' @examples
#' # Base model: predict height from age + site index
#' si_nigh2009(age = c(25, 50, 80), si = c(12, 18, 24))
#'
#' # Base model: predict site index from age + height
#' si_nigh2009(age = c(25, 50, 80), height = c(8, 18, 26))
#'
#' # Operational model (recommended when zone is unknown)
#' si_nigh2009(age = 60, si = 18, model = 2)
#'
#' # Zonal model
#' si_nigh2009(age = 60, si = 18, model = 3, bec_zone = "SBS")
#'
#' @export
si_nigh2009 <- function(
  age,
  height = NULL,
  si = NULL,
  model = 1,
  bec_zone = NULL
) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .nigh2009_prepare(
    age = age,
    x = x,
    model = model,
    bec_zone = bec_zone,
    x_name = x_name
  )

  if (mode == "predict_height") {
    out <- .nigh2009_height(bha = df$age, si = df$si, df)
    if (any(!is.finite(out))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_nigh2009}.",
        "i" = "Check inputs and model coefficients."
      ))
    }
    return(dplyr::tibble(height = out))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .nigh2009_si_from_height_one(
        bha = df$age[[i]],
        height = df$height[[i]],
        pars = df[i, , drop = FALSE]
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_nigh2009}.",
      "i" = "Check inputs and model coefficients."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.nigh2009_prepare <- function(age, x, model, bec_zone, x_name) {
  model <- as.integer(model)
  if (length(model) != 1L || !model %in% c(1L, 2L, 3L)) {
    cli::cli_abort("{.arg model} must be one of 1, 2, or 3.")
  }

  n <- max(length(age), length(x), length(bec_zone %||% character(0)))
  if (n == 0L) {
    # nocov start
    cli::cli_abort("{.arg age} must have length > 0.")
    # nocov end
  }

  # Only model 3 uses a zone; models 1 and 2 are global.
  if (model == 3L) {
    if (is.null(bec_zone)) {
      cli::cli_abort(c(
        "{.arg bec_zone} is required when {.arg model} is 3.",
        "i" = "Supply one of \"ICH\", \"IDF\", or \"SBS\"."
      ))
    }
    zone <- as.character(bec_zone)
    model_key <- "model3"
  } else {
    if (!is.null(bec_zone)) {
      cli::cli_warn(
        "{.arg bec_zone} is ignored when {.arg model} is {model}."
      )
    }
    zone <- NA_character_
    model_key <- if (model == 1L) "model1" else "model2"
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

  pars <- .nigh2009_parameters()

  if (model_key == "model3") {
    valid_zones <- pars$bec_zone[pars$model == "model3"]
    bad <- setdiff(unique(zone), valid_zones)
    if (length(bad) > 0) {
      cli::cli_abort(c(
        "Unknown {.arg bec_zone}: {paste(bad, collapse = ', ')}.",
        "i" = "Valid zones for {.arg model} 3: {paste(valid_zones, collapse = ', ')}."
      ))
    }
    key <- data.frame(model = "model3", bec_zone = zone)
  } else {
    key <- data.frame(model = model_key, bec_zone = NA_character_)
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
.nigh2009_height <- function(bha, si, pars) {
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
.nigh2009_si_from_height_one <- function(bha, height, pars) {
  if (!is.finite(bha) || !is.finite(height) || bha <= 0.5 || height <= 1.3) {
    return(NaN)
  }
  # At BHA 50 the model is conditioned so height == si exactly.
  if (isTRUE(all.equal(bha, 50))) {
    return(height)
  }
  f <- function(s) .nigh2009_height(bha = bha, si = s, pars = pars) - height
  lo <- 1.3 + 1e-6
  hi <- 200
  if (!is.finite(f(lo)) || !is.finite(f(hi)) || f(lo) * f(hi) > 0) {
    # nocov start
    return(NaN)
    # nocov end
  }
  stats::uniroot(f, c(lo, hi), tol = .Machine$double.eps^0.5)$root
}


# internal
.nigh2009_parameters <- function() {
  pars <- .get_internal_data("parameters_Nigh2009") |>
    dplyr::as_tibble()

  req <- c("Species", "model", "bec_zone", "a0", "a1", "a2")
  assert_required_cols(pars, req, object = "parameters_Nigh2009")

  pars
}
