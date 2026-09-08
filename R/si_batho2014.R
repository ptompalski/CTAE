#' Batho and García (2014) height-growth and site-index model for lodgepole pine
#'
#' Unified, vectorized implementation of the Bertalanffy--Richards height-growth
#' and site-index model in Batho and García (2014) for lodgepole pine
#' (\emph{Pinus contorta} var. \emph{latifolia}) in the Sub-Boreal Spruce (SBS)
#' biogeoclimatic zone of British Columbia.
#'
#' \strong{Model scope (species coverage):} lodgepole pine, NFI code
#' \code{PINU.CON}.
#'
#' \strong{Age definition note:} `age` is breast-height age (years). The height
#' curve passes through \eqn{(t_0, H_0) = (0.5\,\mathrm{yr}, 1.3\,\mathrm{m})};
#' the model is defined only for post-breast-height growth.
#'
#' \strong{Base-age note:} site index is the predicted top height at 50 years
#' breast-height age.
#'
#' The final published model (the "Power combined" fit) is the polymorphic
#' Bertalanffy--Richards form
#' \deqn{H = a_q\left\{1 - \left[1 - (H_0/a_q)^c\right]
#'   \exp[-q(t - t_0)]\right\}^{1/c},}
#' with a site-dependent asymptote \eqn{a_q = 12313\,q^{1.645}} and global shape
#' constant \eqn{c = 0.8297} (Batho and García 2014, Eqs. 3--4). The site
#' parameter \eqn{q} has no closed form and is solved numerically from an
#' observed (age, height) pair (Eq. 6).
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
#' @param species Character vector of NFI species codes (`"PINU.CON"`).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted top height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Batho, A., and García, O. (2014). A Site Index Model for Lodgepole Pine in
#' British Columbia. Forest Science 60(5): 982--987.
#' \doi{10.5849/forsci.13-509}
#'
#' @examples
#' # Predict height from age + site index
#' si_batho2014(
#'   age = c(25, 50, 80),
#'   si = c(12, 18, 24),
#'   species = "PINU.CON"
#' )
#'
#' # Predict site index from age + height
#' si_batho2014(
#'   age = c(25, 50, 80),
#'   height = c(8, 18, 26),
#'   species = "PINU.CON"
#' )
#'
#' @export
si_batho2014 <- function(age, height = NULL, si = NULL, species) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .batho2014_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name
  )

  if (mode == "predict_height") {
    h <- vapply(
      seq_len(nrow(df)),
      function(i) {
        .batho2014_height_from_si_one(age = df$age[[i]], si = df$si[[i]])
      },
      numeric(1)
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_batho2014}.",
        "i" = "Check inputs and model domain."
      ))
    }
    if (any(h < 0)) {
      # nocov start
      # Defensive: the Bertalanffy--Richards form cannot yield a negative top
      # height for valid parameters; unreachable via the public API (a failed
      # q-solve returns NaN, caught by the non-finite check above).
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_batho2014}.",
        "i" = "Check inputs and model domain."
      ))
      # nocov end
    }

    return(dplyr::tibble(height = h))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .batho2014_si_from_height_one(age = df$age[[i]], height = df$height[[i]])
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_batho2014}.",
      "i" = "Check inputs and model domain."
    ))
  }
  if (any(si_est < 0)) {
    # nocov start
    # Defensive: predicted site index (height at base age 50) cannot be negative
    # for valid parameters; unreachable via the public API (a failed q-solve
    # returns NaN, caught by the non-finite check above).
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_batho2014}.",
      "i" = "Check inputs and model domain."
    ))
    # nocov end
  }

  dplyr::tibble(si = si_est)
}


# ---- global model constants (Batho and García 2014, Eqs. 3--6) ----
# Verified digit-for-digit against a 300-dpi render of p. 985.
.batho2014_constants <- function() {
  pars <- .get_internal_data("parameters_Batho2014") |>
    dplyr::as_tibble()
  assert_required_cols(
    pars,
    c("a_coef", "a_exp", "c", "h0", "t0", "base_age"),
    object = "parameters_Batho2014"
  )
  list(
    a_coef = pars$a_coef[[1]], # a_q scale (Eq. 4)
    a_exp = pars$a_exp[[1]], # a_q exponent on q (Eq. 4)
    c = pars$c[[1]], # shape exponent (Eqs. 3--6)
    h0 = pars$h0[[1]], # top height at t0
    t0 = pars$t0[[1]], # reference breast-height age
    base_age = pars$base_age[[1]] # site-index base age (breast-height)
  )
}


# internal
.batho2014_prepare <- function(age, x, species, x_name) {
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
  bad <- unique(species_std[species_std != "PINU.CON"])
  if (length(bad) > 0) {
    cli::cli_abort(
      "{.fn si_batho2014} only supports {.val PINU.CON} (lodgepole pine); got: {paste(bad, collapse = ', ')}."
    )
  }

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x)
  )

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
  }

  out
}


# internal
# Top height at breast-height age `t` given the site parameter q (Eq. 3).
.batho2014_height_one <- function(t, q) {
  k <- .batho2014_constants()
  a <- k$a_coef * q^k$a_exp
  cc <- k$c
  a * (1 - (1 - (k$h0 / a)^cc) * exp(-q * (t - k$t0)))^(1 / cc)
}


# internal
# Solve the site parameter q from an observed (age, height) pair. Uses the
# fixed-point iteration of Batho and García (2014, Eq. 6), with a uniroot
# fallback on Eq. 3 when the iteration does not converge.
.batho2014_solve_q_one <- function(t, h) {
  k <- .batho2014_constants()
  cc <- k$c
  h0 <- k$h0
  t0 <- k$t0

  if (!is.finite(t) || !is.finite(h) || t <= t0 || h <= h0) {
    return(NaN)
  }

  # Eq. 6 fixed-point iteration.
  q <- 0.02
  for (iter in seq_len(200L)) {
    e <- exp(-q * (t - t0))
    num <- h^cc - h0^cc * e
    den <- 1 - e
    inner <- (num / den)^(1 / cc) / k$a_coef
    if (!is.finite(inner) || inner <= 0) {
      # nocov start
      # Defensive divergence guard: for valid heights in (H0, asymptote) the Eq. 6
      # step stays finite and positive; not reached via the public API (out-of-
      # domain heights are screened by the `h <= h0` check above).
      q <- NA_real_
      break
      # nocov end
    }
    q_new <- inner^(1 / k$a_exp)
    if (!is.finite(q_new) || q_new <= 0) {
      # nocov start
      q <- NA_real_
      break
      # nocov end
    }
    if (abs(q_new - q) < 1e-12) {
      return(q_new)
    }
    q <- q_new
  }
  # nocov start
  # Loop-exhaustion-with-finite-q: not reached because the Eq. 6 iteration meets
  # the convergence tolerance well within 200 steps for all public inputs.
  if (is.finite(q)) {
    return(q)
  }
  # nocov end

  # Fallback: solve height(t; q) == h directly for q. Defensive only -- the Eq. 6
  # iteration above converges for all finite positive inputs seen via the public
  # API, so this branch is not exercised in tests.
  # nocov start
  f <- function(qq) .batho2014_height_one(t = t, q = qq) - h
  lo <- 1e-6
  hi <- 1
  for (kk in seq_len(30L)) {
    if (is.finite(f(lo)) && is.finite(f(hi)) && f(lo) * f(hi) < 0) {
      return(stats::uniroot(f, c(lo, hi), tol = .Machine$double.eps^0.5)$root)
    }
    hi <- hi * 2
  }
  NaN
  # nocov end
}


# internal
# Predict height at `age` given site index (base age 50): solve q from
# (t = 50, H = si), then evaluate height at the requested age.
.batho2014_height_from_si_one <- function(age, si) {
  k <- .batho2014_constants()
  q <- .batho2014_solve_q_one(t = k$base_age, h = si)
  if (!is.finite(q)) {
    return(NaN)
  }
  .batho2014_height_one(t = age, q = q)
}


# internal
# Predict site index given (age, height): solve q from the observed pair, then
# evaluate height at the base age (50 yr breast-height age).
.batho2014_si_from_height_one <- function(age, height) {
  k <- .batho2014_constants()
  q <- .batho2014_solve_q_one(t = age, h = height)
  if (!is.finite(q)) {
    return(NaN)
  }
  .batho2014_height_one(t = k$base_age, q = q)
}
