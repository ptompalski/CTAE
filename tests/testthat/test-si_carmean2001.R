# tests/testthat/test-si_carmean2001.R
# testthat 3e. Carmean, Niznowski & Hazenberg (2001) Newnham polymorphic
# height-age / site-index model for jack pine in northern Ontario.
#
# Validation tier: TIER 1 fidelity. An independent re-implementation of the
# NRCan SAS macros %HT_Carmean_2001 / %SI_Carmean_2001 (jack pine coefficients
# b1=4.1459, b2=0.6224, b3=1.3723, b4=-0.0802; base age 50) is inlined below;
# the package function must match it to machine precision.
#
# This is a single-species (jack pine, PINU.BAN) model and takes no `species`
# argument.

# --- structural -------------------------------------------------------------

test_that("si_carmean2001 returns a well-formed tibble (predict height)", {
  out <- si_carmean2001(age = c(25, 50, 80), si = c(12, 16, 20))
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
  expect_equal(nrow(out), 3)
})

test_that("si_carmean2001 returns a well-formed tibble (predict si)", {
  out <- si_carmean2001(age = c(25, 80), height = c(9, 22))
  expect_s3_class(out, "tbl_df")
  expect_named(out, "si")
  expect_equal(nrow(out), 2)
})

test_that("si_carmean2001 recycles inputs to a common length", {
  out <- si_carmean2001(age = 50, si = c(12, 18))
  expect_equal(nrow(out), 2)
})

test_that("si_carmean2001 errors on incompatible input lengths", {
  expect_error(si_carmean2001(age = c(25, 50), si = c(12, 18, 24)))
})

test_that("si_carmean2001 requires exactly one of height / si", {
  expect_error(si_carmean2001(age = 50))
  expect_error(si_carmean2001(age = 50, height = 16, si = 16))
})

test_that("si_carmean2001 validates numeric inputs", {
  expect_error(si_carmean2001(age = -5, si = 16))
  expect_error(si_carmean2001(age = 50, si = -2))
  expect_error(si_carmean2001(age = 50, si = NA_real_))
})

test_that("si_carmean2001 rejects si / height at or below breast height", {
  expect_error(si_carmean2001(age = 25, si = 1.2), "1.3")
  expect_error(si_carmean2001(age = 25, height = 1.0), "1.3")
})

test_that("si_carmean2001 aborts on non-finite height from out-of-domain si", {
  # A site index far above the model's domain yields a non-finite height and
  # the forward prediction aborts.
  expect_error(
    suppressWarnings(si_carmean2001(age = 25, si = 60)),
    "Non-finite height"
  )
})

test_that("si_carmean2001 aborts on non-invertible height", {
  # A height above the maximum attainable by the curve cannot be bracketed to a
  # site index; the inverse solver returns NaN and aborts. This also exercises
  # the adaptive bracket-expansion path.
  expect_error(
    suppressWarnings(si_carmean2001(age = 20, height = 60)),
    "Non-finite site index"
  )
})

# --- fidelity guard 1: exact at base age (height == si at BHA 50) -----------

test_that("si_carmean2001 returns height == si at breast-height age 50", {
  expect_equal(si_carmean2001(age = 50, si = 14)$height, 14)
  expect_equal(si_carmean2001(age = 50, si = 20)$height, 20)
  # inverse is also exact at base age
  expect_equal(si_carmean2001(age = 50, height = 14)$si, 14)
})

# --- fidelity guard 2: predict -> invert round-trip -------------------------

test_that("si_carmean2001 predict/invert round-trip recovers si", {
  ages <- c(15, 30, 80, 100)
  for (s in c(10, 14, 18, 22)) {
    h <- si_carmean2001(age = ages, si = s)$height
    rec <- si_carmean2001(age = ages, height = h)$si
    expect_equal(rec, rep(s, length(ages)), tolerance = 1e-6, info = s)
  }
})

# --- TIER 1 fidelity: vs independent SAS re-implementation -------------------

test_that("si_carmean2001 height matches the SAS %HT_Carmean_2001 macro", {
  sas_ht_jp <- function(bhage, si) {
    b1 <- 4.1459
    b2 <- 0.6224
    b3 <- 1.3723
    b4 <- -0.0802
    Tr <- 50
    s13 <- si - 1.3
    K <- 1 - (s13 / (b1 * s13^b2))^(1 / (b3 * s13^b4))
    x1 <- b1 * (1 - K^(bhage / Tr))^(b3 * s13^b4)
    1.3 + (s13^b2) * x1
  }
  grid <- expand.grid(age = c(10, 25, 50, 80, 100), si = c(10, 15, 20))
  pkg <- mapply(
    function(a, s) si_carmean2001(age = a, si = s)$height,
    grid$age,
    grid$si
  )
  sas <- mapply(sas_ht_jp, grid$age, grid$si)
  expect_equal(pkg, sas, tolerance = 1e-10)
})

test_that("si_carmean2001 si matches the SAS %SI_Carmean_2001 macro", {
  sas_si_jp <- function(bhage, height) {
    b1 <- 4.1459
    b2 <- 0.6224
    b3 <- 1.3723
    b4 <- -0.0802
    Tr <- 50
    si0 <- 20
    si1 <- Inf
    n <- 1
    repeat {
      s13 <- si0 - 1.3
      K <- 1 - (s13 / (b1 * s13^b2))^(1 / (b3 * s13^b4))
      x1 <- b1 * (1 - K^(bhage / Tr))^(b3 * s13^b4)
      si1 <- 1.3 + ((height - 1.3) / x1)^(1 / b2)
      si0 <- (si0 + si1) / 2
      n <- n + 1
      if (abs(si0 - si1) < 1e-8 || n > 5000) break
    }
    si0
  }
  # heights generated from the forward SAS macro to stay in-domain, at
  # non-base ages so the constrained shortcut does not mask the solver
  ages <- c(20, 35, 70, 90)
  sis <- c(9, 13, 17, 21)
  heights <- mapply(
    function(a, s) {
      b1 <- 4.1459
      b2 <- 0.6224
      b3 <- 1.3723
      b4 <- -0.0802
      s13 <- s - 1.3
      K <- 1 - (s13 / (b1 * s13^b2))^(1 / (b3 * s13^b4))
      1.3 + (s13^b2) * b1 * (1 - K^(a / 50))^(b3 * s13^b4)
    },
    ages,
    sis
  )
  pkg <- mapply(
    function(a, h) si_carmean2001(age = a, height = h)$si,
    ages,
    heights
  )
  sas <- mapply(sas_si_jp, ages, heights)
  expect_equal(pkg, sas, tolerance = 1e-4)
})

# --- monotonicity sanity check ----------------------------------------------

test_that("si_carmean2001 height is monotonic in age and in si", {
  by_age <- si_carmean2001(age = seq(10, 100, by = 10), si = 16)$height
  expect_true(all(diff(by_age) > 0))
  by_si <- si_carmean2001(age = 60, si = seq(8, 22, by = 2))$height
  expect_true(all(diff(by_si) > 0))
})

# --- registry wiring --------------------------------------------------------

test_that("si_carmean2001 is registered with correct metadata", {
  reg <- si_model_registry()
  row <- reg[reg$model_id == "carmean2001", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$engine, "si_carmean2001")
  expect_equal(row$reference, "@Carmean2001")
  expect_false(row$requires_species)
  expect_equal(row$params_key, "parameters_Carmean2001")
})
