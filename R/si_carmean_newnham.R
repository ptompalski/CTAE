# Shared solver for the Carmean constrained-polymorphic (Newnham 1988) site-index
# models. si_carmean2001 (jack pine) and si_carmean2006 (black spruce, trembling
# aspen) use the identical height(age, si) equation form and the same
# root-finding inversion; only their coefficient sets differ. These helpers
# centralize that shared math so the two model files cannot drift apart.

# internal
# Predicted height (m) at breast-height age from site index (Newnham eq.).
# `si` must exceed breast height (1.3 m). `pars` supplies b1-b4 and base_age.
.carmean_newnham_height_one <- function(age, si, pars, fn) {
  if (si <= 1.3) {
    sp <- if ("Species" %in% names(pars)) pars$Species[[1]] else NA_character_
    if (!is.na(sp)) {
      cli::cli_abort(
        "{.arg si} must be > 1.3 for species {.val {sp}} in {.fn {fn}}."
      )
    }
    cli::cli_abort("{.arg si} must be > 1.3 in {.fn {fn}}.")
  }

  b1 <- pars$b1[[1]]
  b2 <- pars$b2[[1]]
  b3 <- pars$b3[[1]]
  b4 <- pars$b4[[1]]
  base_age <- pars$base_age[[1]]

  s13 <- si - 1.3
  p <- b3 * s13^b4
  k <- 1 - (s13 / (b1 * s13^b2))^(1 / p)

  1.3 + b1 * s13^b2 * (1 - k^(age / base_age))^p
}


# internal
# Solve site index from an observed (age, height) pair by root-finding on
# height(age, si) - height = 0. Height must exceed breast height (1.3 m).
.carmean_newnham_si_one <- function(age, height, pars, fn) {
  if (height <= 1.3) {
    sp <- if ("Species" %in% names(pars)) pars$Species[[1]] else NA_character_
    if (!is.na(sp)) {
      cli::cli_abort(
        "{.arg height} must be > 1.3 for species {.val {sp}} in {.fn {fn}}."
      )
    }
    cli::cli_abort("{.arg height} must be > 1.3 in {.fn {fn}}.")
  }

  base_age <- pars$base_age[[1]]

  # At base age the model is constrained so height == si exactly.
  if (isTRUE(all.equal(age, base_age))) {
    return(height)
  }

  f <- function(s) {
    .carmean_newnham_height_one(age = age, si = s, pars = pars, fn = fn) -
      height
  }

  # Adaptively bracket the (monotone increasing) height-vs-si curve. The upper
  # end can go non-finite for extreme si under some coefficient sets, so scan a
  # finite grid and expand until a sign change is found.
  lo <- 1.3 + 1e-6
  upper <- 60
  bracket <- NULL

  for (iter in seq_len(8)) {
    grid <- unique(c(lo, seq(lo + 0.25, upper, length.out = 400)))
    vals <- vapply(grid, f, numeric(1))
    keep <- is.finite(vals)
    grid <- grid[keep]
    vals <- vals[keep]

    if (length(vals) >= 2L) {
      exact <- which(abs(vals) < 1e-12)
      if (length(exact) > 0L) {
        return(grid[exact[[1]]]) # nocov: grid rarely lands exactly on the root
      }
      idx <- which(vals[-1] * vals[-length(vals)] < 0)
      if (length(idx) > 0L) {
        i <- idx[[1]]
        bracket <- c(grid[[i]], grid[[i + 1L]])
        break
      }
    }
    upper <- upper * 1.5
  }

  if (is.null(bracket)) {
    # nocov start
    # Defensive: for a valid (age, height) pair within the model domain the
    # monotone height curve brackets a unique site index. Out-of-domain heights
    # are screened above and surface as a non-finite abort in the caller.
    return(NaN)
    # nocov end
  }

  stats::uniroot(f, bracket, tol = .Machine$double.eps^0.5)$root
}
