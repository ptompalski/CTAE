#' Nigh (2004) juvenile height-age (site index) model for British Columbia
#'
#' Unified, vectorized implementation of the juvenile height-age (site index)
#' models in Nigh (2004) for lodgepole pine (\emph{Pinus contorta} var.
#' \emph{latifolia}) and interior spruce (\emph{Picea glauca}, \emph{P.
#' engelmannii}, and their hybrid) in British Columbia.
#'
#' \strong{Model scope (species coverage):} lodgepole pine (NFI code
#' \code{PINU.CON}) and interior spruce, modelled as white spruce (NFI code
#' \code{PICE.GLA}), following the source publication.
#'
#' \strong{Age definition note:} `age` is \emph{total} age (years), not
#' breast-height age. The model is conditioned to predict a height of zero at
#' total age zero.
#'
#' \strong{Model form:} the base model (eq. 3) is
#' \deqn{H = a_1 \times SI \times A^{a_2 + a_3 SI} \times a_4^{A}}
#' where \eqn{H} is height (m), \eqn{SI} is site index (m), \eqn{A} is total age
#' (years), and \eqn{a_1, a_2, a_3, a_4} are fitted parameters. The parameters
#' were fitted province-wide (Table 2) and then allowed to vary by biogeoclimatic
#' zone via additive indicator terms (eqs. 4-6, Table 3). This implementation
#' stores the resolved per-zone \eqn{a_1, a_2, a_3, a_4} plus a province-wide set.
#'
#' \strong{Zone selection:} supply `bec_zone` to use a zone-specific parameter
#' set (one of `"BWBS"`, `"ESSF"`, `"ICH"`, `"IDF"`, `"MS"`, `"SBS"`, `"SBPS"`).
#' When `bec_zone` is `NULL` (the default), the province-wide average parameters
#' are used, as recommended by the author when the zone is unknown or was not
#' sampled.
#'
#' Because \eqn{SI} appears both as a multiplier and inside the exponent of
#' \eqn{A}, eq. 3 has no closed-form inverse in \eqn{SI}; when predicting site
#' index the equation is solved numerically.
#'
#' Provide exactly one of `height` or `si`:
#' \itemize{
#'   \item If `si` is provided, the function predicts `height`.
#'   \item If `height` is provided, the function predicts `si`.
#' }
#'
#' @param age Numeric vector. Total age (years).
#' @param species Character vector of NFI species codes: `"PINU.CON"`
#'   (lodgepole pine) or `"PICE.GLA"` (interior spruce). Recycled to a common
#'   length with the other inputs.
#' @param height Optional numeric vector. Site height (m). If provided, `si` is
#'   predicted.
#' @param si Optional numeric vector. Site index (m). If provided, `height` is
#'   predicted.
#' @param bec_zone Optional character vector selecting zone-specific
#'   coefficients. One of `"BWBS"`, `"ESSF"`, `"ICH"`, `"IDF"`, `"MS"`, `"SBS"`,
#'   `"SBPS"`. When `NULL` (default), the province-wide parameters are used.
#'
#' @return A tibble with a single column:
#' \describe{
#'   \item{height}{Predicted site height (m), returned when `si` is provided.}
#'   \item{si}{Predicted site index (m), returned when `height` is provided.}
#' }
#'
#' @references
#' Nigh, G.D. 2004. Juvenile height models for lodgepole pine and interior
#' spruce: validation of existing models and development of new models. Res.
#' Rep. 25. B.C. Ministry of Forests, Forest Science Program, Victoria, B.C.
#'
#' @examples
#' # Province-wide: predict height from age + site index
#' si_nigh2004(age = c(5, 10, 15), species = "PINU.CON", si = 20)
#'
#' # Zone-specific interior spruce
#' si_nigh2004(age = 12, species = "PICE.GLA", si = 18, bec_zone = "SBS")
#'
#' # Predict site index from age + height
#' si_nigh2004(age = 10, species = "PINU.CON", height = 2.5)
#'
#' @export
si_nigh2004 <- function(
  age,
  species,
  height = NULL,
  si = NULL,
  bec_zone = NULL
) {
  if (xor(is.null(height), is.null(si)) == FALSE) {
    cli::cli_abort("Provide exactly one of {.arg height} or {.arg si}.")
  }

  mode <- if (is.null(height)) "predict_height" else "predict_si"
  x <- if (mode == "predict_height") si else height
  x_name <- if (mode == "predict_height") "si" else "height"

  df <- .nigh2004_prepare(
    age = age,
    species = species,
    x = x,
    bec_zone = bec_zone,
    x_name = x_name
  )

  if (mode == "predict_height") {
    out <- .nigh2004_height(age = df$age, si = df$si, pars = df)
    if (any(!is.finite(out))) {
      # nocov start
      # Defensive: for finite positive age and si the base model (eq. 3) is a
      # product of finite positive terms, so height is always finite. Kept as a
      # guard against pathological coefficients.
      cli::cli_abort(c(
        "Non-finite height prediction generated in {.fn si_nigh2004}.",
        "i" = "Check inputs and model coefficients."
      ))
      # nocov end
    }
    return(dplyr::tibble(height = out))
  }

  si_est <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .nigh2004_si_from_height_one(
        age = df$age[[i]],
        height = df$height[[i]],
        pars = df[i, , drop = FALSE]
      )
    },
    numeric(1)
  )

  if (any(!is.finite(si_est))) {
    cli::cli_abort(c(
      "Non-finite site index prediction generated in {.fn si_nigh2004}.",
      "i" = "Check inputs and model coefficients."
    ))
  }

  dplyr::tibble(si = si_est)
}


# internal
.nigh2004_prepare <- function(age, species, x, bec_zone, x_name) {
  n <- max(
    length(age),
    length(species),
    length(x),
    length(bec_zone %||% character(0))
  )
  if (n == 0L) {
    # nocov start
    # Defensive: unreachable via the public API, which requires `age`, `species`,
    # and one of `height`/`si` (all length > 0).
    cli::cli_abort("{.arg age} must have length > 0.")
    # nocov end
  }

  # NULL bec_zone means the province-wide parameter set.
  zone <- if (is.null(bec_zone)) "PROV" else as.character(bec_zone)
  species <- standardize_species_code(as.character(species))

  recycled <- assert_len_compat(
    age = age,
    species = species,
    x = x,
    zone = zone,
    .n = n,
    .recycle = TRUE
  )
  age <- recycled$age
  species <- recycled$species
  x <- recycled$x
  zone <- recycled$zone

  assert_numeric_vec(age, "age", finite = TRUE, gt = 0, allow_na = FALSE)
  assert_numeric_vec(x, x_name, finite = TRUE, gt = 0, allow_na = FALSE)

  pars <- .nigh2004_parameters()

  bad_sp <- setdiff(unique(species), unique(pars$Species))
  if (length(bad_sp) > 0) {
    cli::cli_abort(c(
      "Unknown {.arg species}: {paste(bad_sp, collapse = ', ')}.",
      "i" = "Supported species: {paste(sort(unique(pars$Species)), collapse = ', ')}."
    ))
  }

  bad_zone <- setdiff(unique(zone), unique(pars$bec_zone))
  if (length(bad_zone) > 0) {
    valid <- setdiff(sort(unique(pars$bec_zone)), "PROV")
    cli::cli_abort(c(
      "Unknown {.arg bec_zone}: {paste(setdiff(bad_zone, 'PROV'), collapse = ', ')}.",
      "i" = "Valid zones: {paste(valid, collapse = ', ')} (or NULL for province-wide)."
    ))
  }

  out <- dplyr::tibble(
    age = as.numeric(age),
    Species = species,
    bec_zone = zone,
    x = as.numeric(x)
  ) |>
    dplyr::left_join(
      pars[, c("Species", "bec_zone", "a1", "a2", "a3", "a4")],
      by = c("Species", "bec_zone")
    )

  if (identical(x_name, "height")) {
    out$height <- out$x
  } else {
    out$si <- out$x
  }

  out
}


# internal
# Juvenile height at total age given site index (eq. 3). Vectorized over inputs
# sharing a common length; `pars` supplies a1/a2/a3/a4 columns.
.nigh2004_height <- function(age, si, pars) {
  pars$a1 * si * age^(pars$a2 + pars$a3 * si) * pars$a4^age
}


# internal
# Solve site index from an observed (age, height) pair by root-finding on
# height(age, si) - height = 0.
.nigh2004_si_from_height_one <- function(age, height, pars) {
  if (!is.finite(age) || !is.finite(height) || age <= 0 || height <= 0) {
    # nocov start
    # Defensive: valid inputs are screened as finite and positive in
    # `.nigh2004_prepare()`, so this guard is not reached via the public API.
    return(NaN)
    # nocov end
  }
  f <- function(s) .nigh2004_height(age = age, si = s, pars = pars) - height
  lo <- 1e-6
  hi <- 60
  if (!is.finite(f(lo)) || !is.finite(f(hi)) || f(lo) * f(hi) > 0) {
    # nocov start
    # Defensive: height is monotone increasing in SI over (0, 60], so a valid
    # positive height brackets a unique root; heights above the value at SI = 60
    # would fail to bracket and surface as a non-finite abort.
    return(NaN)
    # nocov end
  }
  stats::uniroot(f, c(lo, hi), tol = .Machine$double.eps^0.5)$root
}


# internal
.nigh2004_parameters <- function() {
  pars <- .get_internal_data("parameters_Nigh2004") |>
    dplyr::as_tibble()

  req <- c("Species", "bec_zone", "a1", "a2", "a3", "a4")
  assert_required_cols(pars, req, object = "parameters_Nigh2004")

  pars
}
