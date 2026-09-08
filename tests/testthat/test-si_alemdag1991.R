# tests/testthat/test-si_alemdag1991.R
# testthat 3e.

test_that("si_alemdag1991 returns a well-formed tibble when predicting height", {
  out <- si_alemdag1991(age = c(25, 50), si = c(12, 15))
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
  expect_equal(nrow(out), 2L)
  expect_true(all(is.finite(out$height)))
})

test_that("si_alemdag1991 returns a well-formed tibble when predicting si", {
  out <- si_alemdag1991(age = c(25, 50), height = c(9, 15))
  expect_s3_class(out, "tbl_df")
  expect_named(out, "si")
  expect_equal(nrow(out), 2L)
  expect_true(all(is.finite(out$si)))
})

test_that("si_alemdag1991 recycles inputs to a common length", {
  out <- si_alemdag1991(age = c(20, 40, 60), si = 15)
  expect_equal(nrow(out), 3L)
})

test_that("si_alemdag1991 errors on incompatible input lengths", {
  expect_error(si_alemdag1991(age = c(20, 40), si = c(12, 15, 18)))
})

test_that("si_alemdag1991 validates inputs", {
  # both height and si supplied
  expect_error(si_alemdag1991(age = 50, height = 15, si = 15))
  # neither supplied
  expect_error(si_alemdag1991(age = 50))
  # non-positive / non-finite age
  expect_error(si_alemdag1991(age = 0, si = 15))
  expect_error(si_alemdag1991(age = NA_real_, si = 15))
  # height / si at or below breast height (1.3 m) is out of domain
  expect_error(si_alemdag1991(age = 50, si = 1.3))
  expect_error(si_alemdag1991(age = 50, height = 1.0))
  # zero-length inputs
  expect_error(si_alemdag1991(age = numeric(0), si = numeric(0)))
})

# --- Validation tier 1 (fidelity via exact model self-consistency) ---
# The source publishes no numeric prediction grid, but both equations are
# conditioned so that the predicted value equals the input at the base age
# (50 yr BH age). A faithful implementation must reproduce these identities.

test_that("height at the base age (50 yr) equals the input site index (model [4])", {
  si_in <- c(5, 10, 15, 20, 25)
  h50 <- si_alemdag1991(age = 50, si = si_in)$height
  expect_equal(h50, si_in, tolerance = 1e-8)
})

test_that("site index at the base age (50 yr) equals the input height (model [9])", {
  # Model [9] is conditioned via the c3 -> m substitution; with the rounded
  # published coefficients the identity holds to a few parts in 1e3.
  h_in <- c(5, 10, 15, 20, 25)
  s50 <- si_alemdag1991(age = 50, height = h_in)$si
  expect_equal(s50, h_in, tolerance = 5e-3)
})

# --- Structural behaviour expected of the model form ---

test_that("predicted height increases with age and with site index", {
  h_by_age <- si_alemdag1991(age = c(10, 20, 40, 80, 120, 150), si = 15)$height
  expect_true(all(diff(h_by_age) > 0))

  h_by_si <- si_alemdag1991(age = 40, si = c(6, 10, 14, 18, 22))$height
  expect_true(all(diff(h_by_si) > 0))
})

test_that("predicted site index decreases with age at fixed height", {
  # For a given observed height, an older stand implies a poorer site (lower SI).
  s_by_age <- si_alemdag1991(age = c(20, 40, 60, 100, 150), height = 15)$si
  expect_true(all(diff(s_by_age) < 0))
})

test_that("si_alemdag1991 reproduces the SI curve shape of Fig. 3", {
  # For H = 15 m, Fig. 3 gives SI ~15 at BH age 50 and declines with age.
  s <- si_alemdag1991(age = c(50, 150), height = 15)$si
  expect_equal(s[1], 15, tolerance = 5e-3)
  expect_true(s[2] > 5 && s[2] < 9) # ~7 m at age 150 (Fig. 3)
})

# --- Validation tier 2 (plausibility sanity check, NOT a fidelity test) ---
# Compare height predictions against si_thrower1994 white spruce (PICE.GLA) over
# overlapping BH ages / site indices. Different data and regions, so only similar
# magnitude and monotonic shape are expected -- this catches gross errors.

test_that("si_alemdag1991 is plausible vs si_thrower1994 white spruce (sanity)", {
  ages <- c(30, 50, 70)
  si <- 15
  al <- si_alemdag1991(age = ages, si = si)$height
  th <- si_thrower1994(age = ages, si = si, species = "PICE.GLA")$height
  expect_true(all(abs(al - th) / th < 0.30))
})

test_that("si_alemdag1991 aborts when the site-index estimate is non-finite", {
  # An extreme height combined with a near-zero breast-height age drives the
  # reciprocal in model [9] to overflow, yielding a non-finite site index.
  expect_error(
    si_alemdag1991(age = 1e-9, height = 1e8),
    regexp = "Non-finite site index"
  )
})

# --- Registry wiring ---

test_that("alemdag1991 is registered and discoverable", {
  reg <- si_model_registry()
  row <- reg[reg$model_id == "alemdag1991", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$engine, "si_alemdag1991")
  expect_equal(row$reference, "@Alemdag1991")
  expect_identical(row$species_manual[[1]], "PICE.GLA")
})
