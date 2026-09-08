#' Cieszewski, Bella and Yeung (1993) variable-age site-index model for Saskatchewan
#'
#' Unified, vectorized implementation of the preliminary variable-age
#' height-growth / site-index model of Cieszewski, Bella and Yeung (1993) for
#' eleven timber species in Saskatchewan. The model is a simplified form
#' (their eq. 2) of the Cieszewski and Bella (1989) polymorphic variable-age
#' height-growth model.
#'
#' \strong{Model scope (species coverage):} eleven species, mapped to the NFI
#' codes \code{ABIE.BAL} (balsam fir), \code{POPU.BAL} (balsam poplar),
#' \code{PICE.MAR} (black spruce), \code{PINU.BAN} (jack pine),
#' \code{PINU.CON} (lodgepole pine), \code{ACER.NEG} (manitoba maple),
#' \code{POPU.TRE} (trembling aspen), \code{LARI.LAR} (tamarack),
#' \code{BETU.PAP} (white birch), \code{ULMU.AME} (white elm), and
#' \code{PICE.GLA} (white spruce).
#'
#' \strong{Age definition note:} `age` is breast-height age (years). Curves pass
#' through breast height (1.3 m) at age 0; the model is defined only for
#' post-breast-height growth.
#'
#' \strong{Base-age note:} site index is site height at breast-height age 50.
#'
#' The height-growth form (their eq. 2) is
#' \deqn{H = 1.3 + \frac{h_x + d + r}{2 + \dfrac{4 b / t^{a}}{h_x - d + r}}, \quad
#'   r = \sqrt{(h_x - d)^2 + \frac{4 b\, h_x}{t_r^{a}}}, \quad d = \frac{b}{50^{a}},}
#' where \eqn{h_x = SI - 1.3} is the reference height above breast height at the
#' reference age \eqn{t_r = 50}, \eqn{t} is the prediction (breast-height) age,
#' and \eqn{a, b} are species-specific fitted coefficients. Because the reference
#' age equals the base age (50), both directions are closed form: at \eqn{t = 50}
#' the curve returns \eqn{H = SI} exactly, and site index is recovered
#' analytically from an observed (age, height) pair.
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
#' @param species Character vector of NFI species codes (see scope above).
#'
#' @return A tibble with a single column:
#' \describe{
#'   \item{height}{Predicted site height (m), returned when `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when `height` is provided.}
#' }
#'
#' @references
#' Cieszewski, C.J., Bella, I.E., and Yeung, D.P. (1993). Preliminary site-index
#' height growth curves for eleven timber species in Saskatchewan. Draft
#' unpublished project report, Canada--Saskatchewan Partnership Agreement in
#' Forestry. Natural Resources Canada, Canadian Forest Service, Prince Albert,
#' Saskatchewan.
#'
#' Cieszewski, C.J., and Bella, I.E. (1989). Polymorphic height and site index
#' curves for lodgepole pine in Alberta. Canadian Journal of Forest Research
#' 19: 1151--1160.
#'
#' @examples
#' # Predict height from age + site index
#' si_cieszewski1993(age = c(25, 50, 80), si = c(12, 16, 20), species = "PINU.BAN")
#'
#' # Predict site index from age + height
#' si_cieszewski1993(age = c(25, 50, 80), height = c(9, 16, 21), species = "PINU.BAN")
#'
#' @export
si_cieszewski1993 <- function(age, height = NULL, si = NULL, species) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .cieszewski1993_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name
  )

  if (mode == "predict_height") {
    out <- .cieszewski1993_height(
      age = df$age,
      si = df$si,
      a = df$a,
      b = df$b,
      base_age = df$base_age
    )
    if (any(!is.finite(out))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_cieszewski1993}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    return(dplyr::tibble(height = out))
  }

  si_est <- .cieszewski1993_si(
    age = df$age,
    height = df$height,
    a = df$a,
    b = df$b,
    base_age = df$base_age
  )
  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_cieszewski1993}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.cieszewski1993_prepare <- function(age, x, species, x_name) {
  n <- max(length(age), length(x), length(species))
  if (n == 0L) {
    cli::cli_abort("{.arg age} must have length > 0.")
  }

  recycled <- assert_len_compat(
    age = age,
    x = x,
    species = species,
    .n = n,
    .recycle = TRUE
  )
  age <- recycled$age
  x <- recycled$x
  species <- recycled$species

  assert_numeric_vec(age, "age", finite = TRUE, gt = 0, allow_na = FALSE)
  # Site index / height must exceed breast height (1.3 m) for a valid curve.
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 1.3, allow_na = FALSE)

  species_std <- standardize_species_code(species)
  pars <- .cieszewski1993_parameters()

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$a)) {
    bad <- unique(out$Species[is.na(out$a)])
    cli::cli_abort(
      "No Cieszewski1993 parameters found for species: {paste(bad, collapse = ', ')}."
    )
  }

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
  }

  out
}


# internal
# Site height at breast-height age `age` given site index (height at base age
# `base_age` = 50). Vectorized; a/b/base_age are per-row parameter vectors.
# Implements eq. 2.
.cieszewski1993_height <- function(age, si, a, b, base_age) {
  hx <- si - 1.3
  d <- b / base_age^a
  r <- sqrt((hx - d)^2 + 4 * b * hx / base_age^a)
  1.3 + (hx + d + r) / (2 + (4 * b / age^a) / (hx - d + r))
}


# internal
# Site index (height at base age `base_age` = 50) from an observed (age, height)
# pair. Closed form because the reference age is the base age. Vectorized.
.cieszewski1993_si <- function(age, height, a, b, base_age) {
  hxs <- height - 1.3
  d <- b / base_age^a
  r <- sqrt((hxs - d)^2 + 4 * b * hxs / age^a)
  hxroots <- hxs + r
  (d + hxroots) / (2 + (4 * b / base_age^a) / (hxroots - d)) + 1.3
}


# internal
.cieszewski1993_parameters <- function() {
  pars <- .get_internal_data("parameters_Cieszewski1993") |>
    dplyr::as_tibble()

  req <- c("Species", "a", "b", "base_age")
  assert_required_cols(pars, req, object = "parameters_Cieszewski1993")

  pars
}
