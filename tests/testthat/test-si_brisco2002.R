# tests/testthat/test-si_brisco2002.R
# testthat 3e. Brisco, Klinka & Nigh (2002) western larch site index model.
# Target 100% coverage: exercise every branch (each error path, both prediction
# directions). Check with
# covr::file_coverage("R/si_brisco2002.R", "tests/testthat/test-si_brisco2002.R").

test_that("si_brisco2002 returns a well-formed tibble (predict height)", {
  out <- si_brisco2002(age = c(25, 50, 80), si = c(15, 20, 25))
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 3)
  expect_true(all(is.finite(out$height)))
})

test_that("si_brisco2002 returns a well-formed tibble (predict si)", {
  out <- si_brisco2002(age = c(25, 50, 80), height = c(10, 20, 28))
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "si")
  expect_equal(nrow(out), 3)
  expect_true(all(is.finite(out$si)))
})

test_that("si_brisco2002 recycles inputs to a common length", {
  out <- si_brisco2002(age = c(20, 40, 60), si = 20)
  expect_equal(nrow(out), 3)
})

test_that("si_brisco2002 errors on incompatible input lengths", {
  expect_error(si_brisco2002(age = c(20, 40), si = c(12, 16, 20)))
})

test_that("si_brisco2002 requires exactly one of height / si", {
  expect_error(si_brisco2002(age = 50), "exactly one")
  expect_error(si_brisco2002(age = 50, height = 20, si = 18), "exactly one")
})

test_that("si_brisco2002 validates inputs", {
  expect_error(si_brisco2002(age = -1, si = 18))
  expect_error(si_brisco2002(age = 50, si = -1))
  expect_error(si_brisco2002(age = NA_real_, si = 18))
  expect_error(si_brisco2002(age = 50, si = NA_real_))
})

# --- Self-consistency (near-fidelity) ---
# eq. 6 is the authors' recommended refit of the Chapman-Richards model to the
# complete data set. Because it is a fitted (not algebraically conditioned)
# model, the height predicted at BHA 50 approximately -- not exactly -- equals
# the site index. Across the fitting SI range (9.7-27.1 m) the bias is small
# and smooth, crossing zero near the data-mean SI. The <=0.35 m tolerance below
# reflects the model as published, and would flag a transcription error in the
# coefficients (which produce order-of-magnitude or wrong-shape departures).
test_that("si_brisco2002 reproduces si at BHA 50 within ~0.35 m", {
  si_vals <- seq(10, 27, by = 1)
  ht50 <- si_brisco2002(age = 50, si = si_vals)$height
  expect_true(all(abs(ht50 - si_vals) <= 0.35))
})

test_that("si_brisco2002 predict_si inverts predict_height exactly", {
  ages <- c(20, 35, 70, 100)
  si_in <- c(12, 16, 20, 25)
  ht <- si_brisco2002(age = ages, si = si_in)$height
  si_out <- si_brisco2002(age = ages, height = ht)$si
  expect_equal(si_out, si_in, tolerance = 1e-4)
})

# --- Regression / self-consistency guard over an input grid ---
# The committed comparison-value generator
# (tmp/generate_si_brisco2002_comparison_values.R) writes a CSV under tmp/, but
# tmp/ is .Rbuildignore'd and absent from installed-package check/coverage runs.
# The grid is inlined here so the test is portable.
test_that("si_brisco2002 produces finite, monotone heights over a grid", {
  ref <- expand.grid(
    age = c(10, 20, 30, 50, 70, 100, 130),
    si = c(10, 14, 18, 22, 26),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  height <- si_brisco2002(age = ref$age, si = ref$si)$height
  expect_equal(length(height), nrow(ref))
  expect_true(all(is.finite(height)))
  expect_true(all(height > 1.3))

  # For each site index, height must increase with age.
  for (s in unique(ref$si)) {
    idx <- order(ref$age[ref$si == s])
    h_s <- height[ref$si == s][idx]
    expect_true(all(diff(h_s) > 0))
  }
})

test_that("si_brisco2002 height increases with age (monotone)", {
  ht <- si_brisco2002(age = c(10, 20, 40, 80, 120), si = 20)$height
  expect_true(all(diff(ht) > 0))
})

test_that("si_brisco2002 aborts when site index cannot be recovered", {
  # A height far above the model's reachable range at the given age cannot be
  # bracketed by a site index, yielding a non-finite site index -> abort.
  expect_error(
    si_brisco2002(age = 10, height = 59),
    "Non-finite site index"
  )
})

test_that(".brisco2002_si_from_height_one returns NaN for out-of-domain inputs", {
  pars <- CanadaForestAllometry:::.brisco2002_parameters()
  expect_true(is.nan(
    CanadaForestAllometry:::.brisco2002_si_from_height_one(
      age = 50,
      height = 1.0,
      pars = pars
    )
  ))
  expect_true(is.nan(
    CanadaForestAllometry:::.brisco2002_si_from_height_one(
      age = 0.2,
      height = 15,
      pars = pars
    )
  ))
})

# --- Cross-check vs. existing same-family model (sanity, not fidelity) ---
# si_thrower1994 also covers BC interior species incl. western larch (LARI.OCC)
# on a BHA-50 basis. Expect similar magnitude/shape, not identical (different
# model form and data).
test_that("si_brisco2002 is plausible vs. si_thrower1994 for western larch (sanity)", {
  ages <- c(20, 40, 60, 80)
  new <- si_brisco2002(age = ages, si = 20)$height
  ref <- si_thrower1994(age = ages, si = 20, species = "LARI.OCC")$height
  expect_true(all(abs(new - ref) / ref < 0.25))
})
