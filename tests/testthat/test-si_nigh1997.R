# tests/testthat/test-si_nigh1997.R
# testthat 3e. Nigh (1997) Sitka spruce height-age model.
#
# NOTE: the source prints no worked example or reference table, so there is no
# Tier-1 source-fidelity test. The strongest fidelity guards available from the
# published equation are the exact-at-base-age identity (height == si at BHA 50)
# and the predict->invert round-trip; both are checked below.

test_that("si_nigh1997 returns a well-formed tibble (predict height)", {
  out <- si_nigh1997(age = c(25, 50, 80), si = c(20, 30, 38))
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
  expect_equal(nrow(out), 3)
})

test_that("si_nigh1997 returns a well-formed tibble (predict si)", {
  out <- si_nigh1997(age = c(25, 50, 80), height = c(15, 30, 42))
  expect_s3_class(out, "tbl_df")
  expect_named(out, "si")
  expect_equal(nrow(out), 3)
})

test_that("si_nigh1997 recycles inputs to a common length", {
  out <- si_nigh1997(age = 50, si = c(20, 30, 38))
  expect_equal(nrow(out), 3)
})

test_that("si_nigh1997 errors on incompatible input lengths", {
  expect_error(si_nigh1997(age = c(25, 50), si = c(20, 30, 38)))
})

test_that("si_nigh1997 requires exactly one of height / si", {
  expect_error(si_nigh1997(age = 50))
  expect_error(si_nigh1997(age = 50, height = 30, si = 30))
})

test_that("si_nigh1997 validates numeric inputs", {
  expect_error(si_nigh1997(age = -5, si = 30))
  expect_error(si_nigh1997(age = 50, si = -2))
  expect_error(si_nigh1997(age = 50, si = NA_real_))
})

test_that("si_nigh1997 aborts on non-finite predictions from degenerate inputs", {
  # si in (0, 1.3] makes log(si - 1.3) non-finite -> height prediction aborts.
  expect_error(
    suppressWarnings(si_nigh1997(age = 30, si = 0.5)),
    "Non-finite height"
  )
  # height at/below breast height (1.3 m) has no invertible site index ->
  # site-index prediction aborts.
  expect_error(
    si_nigh1997(age = 30, height = 1.0),
    "Non-finite site index"
  )
})

# --- Fidelity guard 1: model is exact at the base age (height == si at BHA 50) ---
test_that("si_nigh1997 returns height == si at breast-height age 50", {
  expect_equal(si_nigh1997(age = 50, si = 30)$height, 30)
})

# --- Fidelity guard 2: predict -> invert round-trip recovers site index ---
test_that("si_nigh1997 predict/invert round-trip recovers si", {
  ages <- c(15, 30, 80, 120, 200)
  sis <- c(18, 24, 30, 36, 40)
  h <- si_nigh1997(age = ages, si = sis)$height
  expect_equal(si_nigh1997(age = ages, height = h)$si, sis, tolerance = 1e-6)
})

# --- Plausibility (sanity) check: height increases with age and with si ---
test_that("si_nigh1997 height is monotonic in age and in si (sanity check)", {
  by_age <- si_nigh1997(age = seq(10, 200, by = 10), si = 30)$height
  expect_true(all(diff(by_age) > 0))

  by_si <- si_nigh1997(age = 60, si = seq(15, 40, by = 2))$height
  expect_true(all(diff(by_si) > 0))
})

# --- Coefficient guard: coefficients match the published Table 3 (model [7]) ---
test_that("si_nigh1997 uses the published coefficients", {
  # Reproduce eq. 8 directly and compare against the implementation.
  a0 <- 8.947
  a1 <- -1.357
  a2 <- -1.013
  age <- 35
  si <- 28
  z <- log(si - 1.3)
  num <- 1 + exp(a0 + a1 * log(49.5) + a2 * z)
  den <- 1 + exp(a0 + a1 * log(age - 0.5) + a2 * z)
  expected <- 1.3 + (si - 1.3) * num / den
  expect_equal(si_nigh1997(age = age, si = si)$height, expected, tolerance = 1e-9)
})

# --- Registry wiring ---
test_that("si_nigh1997 is registered", {
  reg <- si_model_registry()
  expect_true("nigh1997" %in% reg$model_id)
  row <- reg[reg$model_id == "nigh1997", ]
  expect_equal(row$engine, "si_nigh1997")
  expect_equal(row$reference, "@Nigh1997")
})
