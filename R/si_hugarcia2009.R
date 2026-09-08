#' Hu and García (2009) height-growth and site-index model for interior spruce
#'
#' Unified, vectorized implementation of the Bertalanffy--Richards height-growth
#' and site-index model in Hu and García (2009) for interior spruce in the
#' Sub-Boreal Spruce (SBS) biogeoclimatic zone of British Columbia.
#'
#' \strong{Model scope (species coverage):} interior spruce, mapped to the NFI
#' codes \code{PICE.GLA} (white spruce) and \code{PICE.ENG} (Engelmann spruce).
#' The source treats the white x Engelmann interior-spruce complex as a single
#' entity; both codes use the same fitted coefficients.
#'
#' \strong{Age definition note:} `age` is breast-height age (years). The height
#' curve passes through the origin \eqn{(t_0, H_0) = (0.5\,\mathrm{yr},
#' 1.3\,\mathrm{m})}; the model is defined only for post-breast-height growth.
#'
#' \strong{Base-age note:} site index is the predicted top height at 50 years
#' breast-height age.
#'
#' The selected model (Hu and García 2009, "combined model 4") is the
#' polymorphic Bertalanffy--Richards form
#' \deqn{H = a\left\{1 - \left[1 - (H_0/a)^c\right]
#'   \exp[-b(t - t_0)]\right\}^{1/c},}
#' with a site-dependent asymptote \eqn{a = 283.9\,q^{0.5137}}, rate
#' \eqn{b = q}, and global shape constant \eqn{c = 0.5829}. The site parameter
#' \eqn{q} has no closed form and is solved numerically.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si`.
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' @param age Numeric vector. Breast-height age (years).
#' @param height Optional numeric vector. Top height (m). If provided, `si` is
#'   predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at breast
#'   height). If provided, `height` is predicted.
#' @param species Character vector of NFI species codes (`"PICE.GLA"` or
#'   `"PICE.ENG"`).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted top height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Hu, Z., and García, O. (2010). A height-growth and site-index model for
#' interior spruce in the Sub-Boreal Spruce biogeoclimatic zone of British
#' Columbia. Canadian Journal of Forest Research 40(6): 1175--1183.
#' \doi{10.1139/X10-076}
#'
#' @examples
#' # Predict height from age + site index
#' si_hugarcia2009(
#'   age = c(25, 50, 80),
#'   si = c(12, 18, 24),
#'   species = "PICE.GLA"
#' )
#'
#' # Predict site index from age + height
#' si_hugarcia2009(
#'   age = c(25, 50, 80),
#'   height = c(8, 18, 26),
#'   species = c("PICE.GLA", "PICE.ENG", "PICE.GLA")
#' )
#'
#' @export
si_hugarcia2009 <- function(age, height = NULL, si = NULL, species) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .hugarcia2009_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name
  )

  if (mode == "predict_height") {
    h <- vapply(
      seq_len(nrow(df)),
      function(i) {
        .hugarcia2009_height_from_si_one(
          age = df$age[[i]],
          si = df$si[[i]],
          pars = df[i, , drop = FALSE]
        )
      },
      numeric(1)
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_hugarcia2009}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    if (any(h < 0)) {
      # nocov start
      # Defensive: the Bertalanffy--Richards form cannot yield a negative top
      # height for valid parameters; unreachable via the public API (a failed
      # q-solve returns NaN, caught by the non-finite check above).
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_hugarcia2009}.",
        "i" = "Check inputs and species-specific parameters."
      ))
      # nocov end
    }

    return(dplyr::tibble(height = h))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .hugarcia2009_si_from_height_one(
        age = df$age[[i]],
        height = df$height[[i]],
        pars = df[i, , drop = FALSE]
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_hugarcia2009}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  if (any(si_est < 0)) {
    # nocov start
    # Defensive: predicted site index (height at base age 50) cannot be negative
    # for valid parameters; unreachable via the public API (a failed q-solve
    # returns NaN, caught by the non-finite check above).
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_hugarcia2009}.",
      "i" = "Check inputs and species-specific parameters."
    ))
    # nocov end
  }

  dplyr::tibble(si = si_est)
}


# internal
.hugarcia2009_prepare <- function(age, x, species, x_name) {
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
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 0, allow_na = FALSE)

  species_std <- standardize_species_code(species)
  pars <- .hugarcia2009_parameters()

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$a_coef)) {
    bad <- unique(out$Species[is.na(out$a_coef)])
    cli::cli_abort(
      "No HuGarcia2009 parameters found for species: {paste(bad, collapse = ', ')}."
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
# Top height at breast-height age `t` given the site parameter q (eq. 2).
.hugarcia2009_height_one <- function(t, q, pars) {
  a <- pars$a_coef[[1]] * q^pars$a_exp[[1]]
  cc <- pars$c[[1]]
  h0 <- pars$h0[[1]]
  t0 <- pars$t0[[1]]
  a * (1 - (1 - (h0 / a)^cc) * exp(-q * (t - t0)))^(1 / cc)
}


# internal
# Solve the site parameter q from an observed (age, height) pair using the
# fixed-point iteration given in Hu and García (2009, section 4.1):
#   q = 1/(t - t0) * ln[ (1 - (H0/a)^c) / (1 - (H/a)^c) ],  a = a_coef * q^a_exp.
# Starts from q = 0.02 and iterates to convergence, with a uniroot fallback.
.hugarcia2009_solve_q_one <- function(t, h, pars) {
  a_coef <- pars$a_coef[[1]]
  a_exp <- pars$a_exp[[1]]
  cc <- pars$c[[1]]
  h0 <- pars$h0[[1]]
  t0 <- pars$t0[[1]]

  if (!is.finite(t) || !is.finite(h) || t <= t0 || h <= h0) {
    return(NaN)
  }

  q <- 0.02
  for (iter in seq_len(200L)) {
    a <- a_coef * q^a_exp
    if (h >= a) {
      # Observed height at or above the asymptote: fall through to uniroot.
      q <- NA_real_
      break
    }
    num <- 1 - (h0 / a)^cc
    den <- 1 - (h / a)^cc
    q_new <- (1 / (t - t0)) * log(num / den)
    if (!is.finite(q_new) || q_new <= 0) {
      # nocov start
      # Defensive: for a valid height in (H0, a) the fixed-point step stays
      # positive and finite; this divergence guard is not reached via the public
      # API (out-of-domain heights are screened earlier or handled by `h >= a`).
      q <- NA_real_
      break
      # nocov end
    }
    if (abs(q_new - q) < 1e-10) {
      return(q_new)
    }
    q <- q_new
  }
  if (is.finite(q)) {
    return(q) # nocov
  }

  # Fallback: solve height(t; q) == h directly for q.
  f <- function(qq) .hugarcia2009_height_one(t = t, q = qq, pars = pars) - h
  lo <- 1e-6
  hi <- 1
  for (k in seq_len(20L)) {
    if (is.finite(f(lo)) && is.finite(f(hi)) && f(lo) * f(hi) < 0) {
      return(stats::uniroot(f, c(lo, hi), tol = .Machine$double.eps^0.5)$root)
    }
    hi <- hi * 2
  }
  NaN
}


# internal
# Predict height at `age` given site index (base age 50): solve q from
# (t = 50, H = si), then evaluate height at the requested age.
.hugarcia2009_height_from_si_one <- function(age, si, pars) {
  q <- .hugarcia2009_solve_q_one(t = pars$base_age[[1]], h = si, pars = pars)
  if (!is.finite(q)) {
    return(NaN)
  }
  .hugarcia2009_height_one(t = age, q = q, pars = pars)
}


# internal
# Predict site index given (age, height): solve q from the observed pair, then
# evaluate height at the base age (50 yr breast-height age).
.hugarcia2009_si_from_height_one <- function(age, height, pars) {
  q <- .hugarcia2009_solve_q_one(t = age, h = height, pars = pars)
  if (!is.finite(q)) {
    return(NaN)
  }
  .hugarcia2009_height_one(t = pars$base_age[[1]], q = q, pars = pars)
}


# internal
.hugarcia2009_parameters <- function() {
  pars <- .get_internal_data("parameters_HuGarcia2009") |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$Species, .keep_all = TRUE)

  req <- c(
    "Species",
    "a_coef",
    "a_exp",
    "c",
    "h0",
    "t0",
    "base_age"
  )
  assert_required_cols(pars, req, object = "parameters_HuGarcia2009")

  pars
}
