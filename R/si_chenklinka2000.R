#' Chen and Klinka (2000) height-growth / site-index model for ESSF species
#'
#' Unified, vectorized implementation of the conditioned Chapman-Richards
#' height-growth models of Chen and Klinka (2000) for high-elevation subalpine
#' fir, Engelmann spruce, and lodgepole pine in the Engelmann Spruce-Subalpine
#' Fir (ESSF) zone of British Columbia.
#'
#' \strong{Model scope (species coverage):} \code{ABIE.LAS} (subalpine fir),
#' \code{PICE.ENG} (Engelmann spruce), \code{PINU.CON} (lodgepole pine).
#'
#' \strong{Age definition note:} `age` is breast-height age (years).
#'
#' \strong{Base-age note:} site index is referenced to top height at 50 years
#' breast-height age.
#'
#' The model form (identical for all three species, differing only in the
#' coefficients \eqn{b_1, b_2, b_3}) is
#' \deqn{H = 1.3 + b_1 \left[(S - 1.3)^{b_2} (1 - e^{-b_3 A})^p\right],}
#' where
#' \deqn{p = \frac{\ln\left((S - 1.3)^{1 - b_2} / b_1\right)}{\ln\left(1 - e^{-b_3 \times 50}\right)}.}
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `height` is provided, the function predicts `si` (solved
#'     numerically; the model has no closed-form inverse).
#'   \item If `si` is provided, the function predicts `height`.
#' }
#'
#' @param age Numeric vector. Breast-height age (years).
#' @param height Optional numeric vector. Top height (m). If provided, `si` is
#'   predicted.
#' @param si Optional numeric vector. Site index (m, base age 50 years at breast
#'   height). If provided, `height` is predicted.
#' @param species Character vector of NFI species codes (`"ABIE.LAS"`,
#'   `"PICE.ENG"`, or `"PINU.CON"`).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{height}{Predicted top height (m), returned when input `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when input `height` is provided.}
#' }
#'
#' @references
#' Chen, H.Y.H. and Klinka, K. (2000). Height growth models for high-elevation
#' subalpine fir, Engelmann spruce, and lodgepole pine in British Columbia.
#' \emph{Western Journal of Applied Forestry} 15(2): 62-69.
#'
#' @examples
#' # Predict top height from age + site index
#' si_chenklinka2000(
#'   age = c(25, 50, 80),
#'   si = c(12, 16, 20),
#'   species = c("ABIE.LAS", "PICE.ENG", "PINU.CON")
#' )
#'
#' # Predict site index from age + top height
#' si_chenklinka2000(
#'   age = c(25, 50, 80),
#'   height = c(8, 16, 24),
#'   species = c("ABIE.LAS", "PICE.ENG", "PINU.CON")
#' )
#'
#' @export
si_chenklinka2000 <- function(age, height = NULL, si = NULL, species) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .chenklinka2000_prepare(
    age = age,
    x = x,
    species = species,
    x_name = x_name
  )

  if (mode == "predict_height") {
    h <- vapply(
      seq_len(nrow(df)),
      function(i) {
        .chenklinka2000_height_one(
          age = df$age[[i]],
          si = df$si[[i]],
          pars = df[i, , drop = FALSE]
        )
      },
      numeric(1)
    )

    if (any(!is.finite(h))) {
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_chenklinka2000}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    # nocov start
    # Unreachable via public API: the model floor is 1.3 m, so a finite height
    # is never negative. Retained as a defensive guard.
    if (any(h < 0)) {
      cli::cli_abort(c(
        "Negative height prediction generated in {.fn si_chenklinka2000}.",
        "i" = "Check inputs and species-specific parameters."
      ))
    }
    # nocov end

    return(dplyr::tibble(height = h))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .chenklinka2000_solve_si_one(
        age = df$age[[i]],
        height = df$height[[i]],
        pars = df[i, , drop = FALSE]
      )
    },
    numeric(1)
  )

  # nocov start
  # Unreachable via public API: .chenklinka2000_solve_si_one() either returns a
  # finite positive root or aborts with a bracketing error. Retained as
  # defensive guards.
  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_chenklinka2000}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  if (any(si_est < 0)) {
    cli::cli_abort(c(
      "Negative site index prediction generated in {.fn si_chenklinka2000}.",
      "i" = "Check inputs and species-specific parameters."
    ))
  }
  # nocov end

  dplyr::tibble(si = si_est)
}


# internal
.chenklinka2000_prepare <- function(age, x, species, x_name) {
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
  pars <- .chenklinka2000_parameters()

  out <- dplyr::tibble(
    age = as.numeric(age),
    x = as.numeric(x),
    Species = species_std
  ) |>
    dplyr::left_join(pars, by = "Species")

  if (anyNA(out$b1)) {
    bad <- unique(out$Species[is.na(out$b1)])
    cli::cli_abort(
      "No ChenKlinka2000 parameters found for species: {paste(bad, collapse = ', ')}."
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
.chenklinka2000_height_one <- function(age, si, pars) {
  b1 <- pars$b1[[1]]
  b2 <- pars$b2[[1]]
  b3 <- pars$b3[[1]]

  si_shift <- si - 1.3
  if (!is.finite(si_shift) || si_shift <= 0) {
    return(NaN)
  }

  # Conditioning exponent p is independent of age; enforces H = S at A = 50.
  p <- log(si_shift^(1 - b2) / b1) / log(1 - exp(-b3 * 50))

  1.3 + b1 * (si_shift^b2 * (1 - exp(-b3 * age))^p)
}


# internal
.chenklinka2000_solve_si_one <- function(age, height, pars) {
  f <- function(si) {
    .chenklinka2000_height_one(age = age, si = si, pars = pars) - height
  }

  lower <- 1.300001
  upper <- max(60, height * 3)
  bracket <- NULL

  for (iter in seq_len(8)) {
    grid <- unique(c(lower, seq(lower + 0.01, upper, length.out = 400)))
    vals <- vapply(grid, f, numeric(1))

    keep <- is.finite(vals)
    grid <- grid[keep]
    vals <- vals[keep]

    if (length(vals) >= 2L) {
      # nocov start
      # Exact floating-point zeros on the search grid essentially never occur;
      # the sign-change bracket below is the operative path.
      exact <- which(vals == 0)
      if (length(exact) > 0L) {
        return(grid[exact[[1]]])
      }
      # nocov end

      idx <- which(vals[-1] * vals[-length(vals)] < 0)
      if (length(idx) > 0L) {
        i <- idx[[1]]
        bracket <- c(grid[[i]], grid[[i + 1L]])
        break
      }
    }

    upper <- upper * 2
  }

  if (is.null(bracket)) {
    cli::cli_abort(c(
      "Failed to bracket a site-index solution in {.fn si_chenklinka2000}.",
      "i" = "Check that age, height, and species are within model domain."
    ))
  }

  stats::uniroot(
    f,
    interval = bracket,
    tol = .Machine$double.eps^0.5
  )$root
}


# internal
.chenklinka2000_parameters <- function() {
  pars <- .get_internal_data("parameters_ChenKlinka1998") |>
    dplyr::as_tibble() |>
    dplyr::distinct(.data$Species, .keep_all = TRUE)

  req <- c("Species", "b1", "b2", "b3")
  assert_required_cols(pars, req, object = "parameters_ChenKlinka1998")

  pars
}
