#' Huang, Meng and Yang (2009) GYPSY site index models for Alberta species
#'
#' Unified, vectorized implementation of the GYPSY top-height / site-index
#' models in Huang, Meng, and Yang (2009) for four main Alberta tree species.
#'
#' \strong{Model scope (species coverage):} four species:
#' \code{POPU.TRE} (aspen), \code{PICE.MAR} (black spruce),
#' \code{PINU.CON} (lodgepole pine), \code{PICE.GLA} (white spruce).
#'
#' \strong{Age definition note:} `age` is \emph{total} age (years from the point
#' of germination). This is the age basis on which the GYPSY top-height models
#' are fitted and inverted.
#'
#' \strong{Base-age note:} site index is referenced to height at 50 years. The
#' `index_age` argument selects which 50-year index is used:
#' \itemize{
#'   \item `"breast_height"` (default): \eqn{SI_{bh}}, top height at 50 years
#'     breast-height age (the index currently used in Alberta).
#'   \item `"total"`: \eqn{SI_t}, top height at 50 years total age (the quantity
#'     the model form uses directly).
#' }
#' The conversion between the two uses the years-to-breast-height (Y2BH) relation
#' embedded in the source SAS program (Appendix 1).
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si` (on the
#'     `index_age` scale).
#'   \item If `si` is provided (on the `index_age` scale), the function predicts
#'     top height.
#' }
#'
#' The top-height model is not closed-form invertible in site index; predicting
#' `si` from `height` is solved by the damped fixed-point iteration used in the
#' source SAS program. When `index_age = "breast_height"`, recovering \eqn{SI_t}
#' from a supplied \eqn{SI_{bh}} is solved numerically with `stats::uniroot()`.
#'
#' @param age Numeric vector. Total age (years from germination).
#' @param height Optional numeric vector. Top height (m), i.e. average height of
#'   the 100 largest-DBH trees per hectare. If provided, `si` is predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years on the
#'   `index_age` scale). If provided, `height` is predicted.
#' @param species Character vector of NFI species codes (e.g., `"PINU.CON"`).
#' @param index_age Character scalar, `"breast_height"` (default) or `"total"`.
#'   Selects whether site index is expressed at 50 years breast-height age
#'   (\eqn{SI_{bh}}) or 50 years total age (\eqn{SI_t}).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted top height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Huang, S., Meng, S.X., and Yang, Y. (2009). A Growth and Yield Projection
#' System (GYPSY) for Natural and Post-harvest Stands in Alberta. Technical
#' Report Pub. No. T/216. Forest Management Branch, Alberta Sustainable Resource
#' Development, Edmonton, Alberta.
#'
#' @examples
#' # Predict site index (SIbh) from total age + top height
#' si_huang2009(
#'   age = c(50, 70),
#'   height = c(20, 20),
#'   species = c("PICE.MAR", "PINU.CON")
#' )
#'
#' # Predict top height from total age + total-age site index (SIt)
#' si_huang2009(
#'   age = c(50, 90),
#'   si = c(20, 12),
#'   species = c("POPU.TRE", "PICE.GLA"),
#'   index_age = "total"
#' )
#'
#' @export
si_huang2009 <- function(
  age,
  height = NULL,
  si = NULL,
  species,
  index_age = c("breast_height", "total")
) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }
  index_age <- rlang::arg_match(index_age)

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .huang2009_prepare(age = age, x = x, species = species, x_name = x_name)

  if (mode == "predict_height") {
    h <- vapply(
      seq_len(nrow(df)),
      function(i) {
        pars <- df[i, , drop = FALSE]
        # Supplied `si` is on the requested index scale; convert to SIt first.
        si_t <- if (index_age == "total") {
          df$si[[i]]
        } else {
          .huang2009_sibh_to_sit_one(sibh = df$si[[i]], pars = pars)
        }
        .huang2009_height_one(age = df$age[[i]], si_t = si_t, pars = pars)
      },
      numeric(1)
    )

    if (any(!is.finite(h))) {
      # nocov start
      # Defensive: valid inputs yield finite heights; guards against pathological
      # parameter/domain combinations.
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_huang2009}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_huang2009}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    # nocov end

    return(dplyr::tibble(height = h))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      pars <- df[i, , drop = FALSE]
      si_t <- .huang2009_solve_sit_one(
        age = df$age[[i]],
        topht = df$height[[i]],
        pars = pars
      )
      if (index_age == "total") {
        si_t
      } else {
        .huang2009_sibh_one(si_t = si_t, pars = pars)
      }
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    # nocov start
    # Defensive: the solver returns a finite root or aborts earlier.
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_huang2009}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_huang2009}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  # nocov end

  dplyr::tibble(si = si_est)
}


# internal
.huang2009_prepare <- function(age, x, species, x_name) {
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

  pars <- .get_internal_data("parameters_Huang2009") |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$Species, .keep_all = TRUE)

  req <- c(
    "Species",
    "model_form",
    "age_squared",
    "lnS_power",
    "b1",
    "b2",
    "b3",
    "b4"
  )
  assert_required_cols(pars, req, object = "parameters_Huang2009")

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$b1)) {
    bad <- unique(out$Species[is.na(out$b1)])
    cli::cli_abort(
      "No Huang2009 parameters found for species: {paste(bad, collapse = ', ')}."
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
# The age argument entering log() is squared for the white-spruce form (SW).
.huang2009_age_term <- function(a, pars) {
  if (isTRUE(pars$age_squared[[1]])) a^2 else a
}


# internal
# Top height from total age and total-age site index (SIt); source eqs. [1]-[3].
.huang2009_height_one <- function(age, si_t, pars) {
  b1 <- pars$b1[[1]]
  b2 <- pars$b2[[1]]
  b3 <- pars$b3[[1]]
  b4 <- pars$b4[[1]]
  pw <- pars$lnS_power[[1]]

  if (!is.finite(si_t) || si_t <= 0) {
    return(NaN) # nocov
  }

  num <- 1 +
    exp(
      b1 +
        b2 * sqrt(log(.huang2009_age_term(50, pars) + 1)) +
        b3 * log(si_t)^pw +
        b4 * sqrt(50)
    )
  den <- 1 +
    exp(
      b1 +
        b2 * sqrt(log(.huang2009_age_term(age, pars) + 1)) +
        b3 * log(si_t)^pw +
        b4 * sqrt(50)
    )

  si_t * num / den
}


# internal
# Solve SIt from total age and top height via the source's damped fixed point.
.huang2009_solve_sit_one <- function(age, topht, pars) {
  b1 <- pars$b1[[1]]
  b2 <- pars$b2[[1]]
  b3 <- pars$b3[[1]]
  b4 <- pars$b4[[1]]
  pw <- pars$lnS_power[[1]]

  si0 <- 10
  si1 <- Inf
  for (iter in seq_len(1000L)) {
    x10 <- 1 +
      exp(
        b1 +
          b2 * sqrt(log(.huang2009_age_term(age, pars) + 1)) +
          b3 * log(si0)^pw +
          b4 * sqrt(50)
      )
    x20 <- 1 +
      exp(
        b1 +
          b2 * sqrt(log(.huang2009_age_term(50, pars) + 1)) +
          b3 * log(si0)^pw +
          b4 * sqrt(50)
      )
    si1 <- topht * x10 / x20
    if (!is.finite(si1)) {
      return(NaN) # nocov
    }
    if (abs(si0 - si1) < 1e-8) {
      break
    }
    si0 <- (si0 + si1) / 2
  }

  si1
}


# internal
# Years-to-breast-height at a solved SIt (source SAS, Appendix 1).
.huang2009_y2bh_one <- function(si_t, pars) {
  b1 <- pars$b1[[1]]
  b2 <- pars$b2[[1]]
  b3 <- pars$b3[[1]]
  b4 <- pars$b4[[1]]
  pw <- pars$lnS_power[[1]]

  k1 <- exp(
    b1 +
      b2 * sqrt(log(.huang2009_age_term(50, pars) + 1)) +
      b3 * log(si_t)^pw +
      b4 * sqrt(50)
  )
  k2 <- if (pw == 1) si_t^b3 else si_t^(b3 * log(si_t))
  k3 <- (si_t * (1 + k1) / 1.3 - 1) / (exp(b1) * exp(b4 * sqrt(50)) * k2)

  inner <- exp((log(k3) / b2)^2) - 1
  if (isTRUE(pars$age_squared[[1]])) sqrt(inner) else inner
}


# internal
# Breast-height-age site index (SIbh) from total-age site index (SIt).
.huang2009_sibh_one <- function(si_t, pars) {
  b1 <- pars$b1[[1]]
  b2 <- pars$b2[[1]]
  b3 <- pars$b3[[1]]
  b4 <- pars$b4[[1]]
  pw <- pars$lnS_power[[1]]

  y2bh <- .huang2009_y2bh_one(si_t = si_t, pars = pars)
  if (!is.finite(y2bh)) {
    return(NaN) # nocov
  }

  num <- 1 +
    exp(
      b1 +
        b2 * sqrt(log(.huang2009_age_term(50, pars) + 1)) +
        b3 * log(si_t)^pw +
        b4 * sqrt(50)
    )
  den <- 1 +
    exp(
      b1 +
        b2 * sqrt(log(.huang2009_age_term(50 + y2bh, pars) + 1)) +
        b3 * log(si_t)^pw +
        b4 * sqrt(50)
    )

  si_t * num / den
}


# internal
# Invert SIbh -> SIt numerically (SIbh is monotone in SIt over the model domain).
.huang2009_sibh_to_sit_one <- function(sibh, pars) {
  f <- function(si_t) .huang2009_sibh_one(si_t = si_t, pars = pars) - sibh

  lower <- 1.5
  upper <- max(60, sibh * 3)

  # Advance lower until f() is finite (small si_t can make Y2BH undefined).
  f_lower <- f(lower)
  iter_lo <- 0L
  while (!is.finite(f_lower) && lower < upper && iter_lo < 60L) {
    # nocov start
    # Defensive: for the four fitted species f() is finite at the starting
    # lower bound; this advances past any si_t where Y2BH is undefined.
    lower <- lower + 0.25
    f_lower <- f(lower)
    iter_lo <- iter_lo + 1L
    # nocov end
  }

  f_upper <- f(upper)
  iter <- 0L
  while (
    is.finite(f_lower) &&
      (!is.finite(f_upper) || sign(f_lower) == sign(f_upper)) &&
      iter < 30L
  ) {
    upper <- upper * 2
    f_upper <- f(upper)
    iter <- iter + 1L
  }

  if (
    !is.finite(f_lower) || !is.finite(f_upper) || sign(f_lower) == sign(f_upper)
  ) {
    cli::cli_abort(c(
      "Failed to recover total-age site index in {.fn si_huang2009}.",
      "i" = "Check that age, site index, and species are within model domain."
    ))
  }

  stats::uniroot(
    f,
    lower = lower,
    upper = upper,
    tol = .Machine$double.eps^0.25
  )$root
}
