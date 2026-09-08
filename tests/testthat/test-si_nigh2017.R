# tests/testthat/test-si_nigh2017.R
# testthat 3e. Nigh (2017) lodgepole pine g-GADA site index model.
# Target 100% coverage: exercise every branch (each error path, both
# prediction directions). Check with
# covr::file_coverage("R/si_nigh2017.R", "tests/testthat/test-si_nigh2017.R").

test_that("si_nigh2017 returns a well-formed tibble (predict height)", {
  out <- si_nigh2017(age = c(25, 50, 80), si = c(12, 18, 24))
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 3)
  expect_true(all(is.finite(out$height)))
})

test_that("si_nigh2017 returns a well-formed tibble (predict si)", {
  out <- si_nigh2017(age = c(25, 50, 80), height = c(8, 18, 26))
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "si")
  expect_equal(nrow(out), 3)
  expect_true(all(is.finite(out$si)))
})

test_that("si_nigh2017 recycles inputs to a common length", {
  out <- si_nigh2017(age = c(20, 40, 60), si = 18)
  expect_equal(nrow(out), 3)
})

test_that("si_nigh2017 errors on incompatible input lengths", {
  expect_error(si_nigh2017(age = c(20, 40), si = c(12, 16, 20)))
})

test_that("si_nigh2017 requires exactly one of height / si", {
  expect_error(si_nigh2017(age = 50), "exactly one")
  expect_error(si_nigh2017(age = 50, height = 20, si = 18), "exactly one")
})

test_that("si_nigh2017 validates inputs", {
  expect_error(si_nigh2017(age = -1, si = 18))
  expect_error(si_nigh2017(age = 50, si = -1))
  expect_error(si_nigh2017(age = NA_real_, si = 18))
  expect_error(si_nigh2017(age = 50, si = NA_real_))
})

# --- Direction round-trip / self-consistency (fidelity-style) ---
# The paper's cubic SI->beta0 conversion has a stated maximum error of 16 cm at
# BHA 50 (Discussion, p. 18). Predicting height at BHA 50 from a site index in
# 5-30 m must reproduce that site index to within ~16 cm.
test_that("si_nigh2017 reproduces si at BHA 50 within the paper's 16 cm tolerance", {
  si_vals <- seq(5, 30, by = 1)
  ht50 <- si_nigh2017(age = 50, si = si_vals)$height
  expect_true(all(abs(ht50 - si_vals) <= 0.16 + 1e-9))
})

test_that("si_nigh2017 predict_si inverts predict_height", {
  # height from (age, si), then recover si from (age, height): should match the
  # height-at-50 the model implies for that beta0 (i.e., a clean round-trip
  # through the calibrated beta0).
  ages <- c(20, 35, 70, 100)
  si_in <- c(10, 15, 20, 25)
  ht <- si_nigh2017(age = ages, si = si_in)$height
  si_out <- si_nigh2017(age = ages, height = ht)$si
  # si_out is height-at-50 for the beta0 calibrated to (age, ht); since ht came
  # from the same beta0, si_out equals height-at-50 for that beta0 exactly.
  ht50_direct <- si_nigh2017(age = 50, si = si_in)$height
  expect_equal(si_out, ht50_direct, tolerance = 1e-4)
})

# --- Regression / self-consistency guard over an input grid ---
# The committed comparison-value generator
# (tmp/generate_si_nigh2017_comparison_values.R) writes a CSV under tmp/, but
# tmp/ is .Rbuildignore'd and absent from installed-package check/coverage runs.
# The grid is therefore inlined here so the test is portable.
test_that("si_nigh2017 produces finite, monotone heights over a grid", {
  ref <- expand.grid(
    age = c(10, 20, 30, 50, 70, 100),
    si = c(5, 10, 15, 20, 25, 30),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  height <- si_nigh2017(age = ref$age, si = ref$si)$height
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

test_that("si_nigh2017 aborts when site index cannot be recovered", {
  # A height above the model's reachable range at the given age cannot be
  # bracketed by a beta0, yielding a non-finite site index -> abort.
  expect_error(
    si_nigh2017(age = 50, height = 80),
    "Non-finite site index"
  )
})

test_that(".nigh2017_si_from_height_one returns NaN for out-of-domain inputs", {
  pars <- CanadaForestAllometry:::.nigh2017_parameters()
  expect_true(is.nan(
    CanadaForestAllometry:::.nigh2017_si_from_height_one(
      bha = 50,
      height = 1.0,
      pars = pars
    )
  ))
  expect_true(is.nan(
    CanadaForestAllometry:::.nigh2017_si_from_height_one(
      bha = 0.2,
      height = 15,
      pars = pars
    )
  ))
})

test_that("si_nigh2017 height increases with age (monotone)", {
  ht <- si_nigh2017(age = c(10, 20, 40, 80, 120), si = 18)$height
  expect_true(all(diff(ht) > 0))
})

# --- Cross-check vs. existing same-family model (sanity, not fidelity) ---
# si_goudie1984 also covers BC lodgepole pine (PINU.CON) on a BHA-50 basis.
# Expect similar magnitude/shape, not identical (different model form/data).
test_that("si_nigh2017 is plausible vs. si_goudie1984 for lodgepole pine (sanity)", {
  ages <- c(20, 40, 60, 80)
  new <- si_nigh2017(age = ages, si = 18)$height
  ref <- si_goudie1984(age = ages, si = 18, species = "PINU.CON")$height
  expect_true(all(abs(new - ref) / ref < 0.25))
})
