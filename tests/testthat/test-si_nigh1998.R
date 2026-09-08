testthat::test_that("si_nigh1998 predicts height from age and site index", {
  out <- CanadaForestAllometry::si_nigh1998(
    age = c(25, 50, 80),
    si = c(12, 18, 24)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "height")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$height)))
})

testthat::test_that("si_nigh1998 is conditioned so height == si at BHA 50 (fidelity)", {
  si_vals <- c(5, 10, 15, 20, 25)
  out <- CanadaForestAllometry::si_nigh1998(age = rep(50, length(si_vals)), si = si_vals)
  testthat::expect_equal(out$height, si_vals, tolerance = 1e-8)
})

testthat::test_that("si_nigh1998 matches manual log-logistic evaluation (eq. 6)", {
  age <- 30
  si <- 15
  b0 <- 8.998
  b1 <- -1.434
  b2 <- -1.051
  z <- log(si - 1.3)
  num <- 1 + exp(b0 + b1 * log(49.5) + b2 * z)
  den <- 1 + exp(b0 + b1 * log(age - 0.5) + b2 * z)
  expected <- 1.3 + (si - 1.3) * num / den

  out <- CanadaForestAllometry::si_nigh1998(age = age, si = si)
  testthat::expect_equal(out$height[[1]], expected, tolerance = 1e-10)
})

testthat::test_that("si_nigh1998 predicts site index from age and height (round-trip)", {
  si_in <- c(12, 18, 24)
  h <- CanadaForestAllometry::si_nigh1998(age = c(25, 80, 120), si = si_in)$height
  out <- CanadaForestAllometry::si_nigh1998(age = c(25, 80, 120), height = h)

  testthat::expect_named(out, "si")
  testthat::expect_equal(out$si, si_in, tolerance = 1e-6)
})

testthat::test_that("si_nigh1998 returns height exactly when inverting at BHA 50", {
  out <- CanadaForestAllometry::si_nigh1998(age = 50, height = 17)
  testthat::expect_equal(out$si[[1]], 17, tolerance = 1e-10)
})

testthat::test_that("si_nigh1998 requires exactly one of height or si", {
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998(age = 40),
    "exactly one",
    ignore.case = TRUE
  )
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998(age = 40, height = 15, si = 15),
    "exactly one",
    ignore.case = TRUE
  )
})

testthat::test_that("si_nigh1998 supports scalar recycling", {
  out <- CanadaForestAllometry::si_nigh1998(age = c(20, 40, 60), si = 15)
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "height")
})

testthat::test_that("si_nigh1998 validates lengths, zero-length, type, and domain", {
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998(age = c(20, 40), si = c(12, 15, 18)),
    "length 1 or",
    ignore.case = TRUE
  )
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998(age = numeric(0), si = numeric(0)),
    "length > 0",
    ignore.case = TRUE
  )
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998(age = "40", si = 15),
    "age.*numeric",
    ignore.case = TRUE
  )
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998(age = 40, si = 0),
    "si.*values > 0",
    ignore.case = TRUE
  )
})

testthat::test_that("si_nigh1998 flags non-finite height predictions", {
  # SI just above breast height combined with a huge age can overflow the ratio;
  # if not, this still exercises the finite-prediction guard path.
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998(age = 1e300, si = 1.3 + 1e-300),
    "Non-finite height prediction",
    ignore.case = TRUE
  )
})

testthat::test_that("si_nigh1998 returns NaN-driven error for out-of-domain height inversion", {
  # height <= 1.3 (breast height) yields NaN internally, surfaced as an abort.
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998(age = 30, height = 1.3),
    "si.*values > 0|Non-finite site index",
    ignore.case = TRUE
  )
})
