# tests/testthat/test-si_carmean2006.R
# testthat 3e. Carmean, Hazenberg & Deschamps (2006) Newnham polymorphic
# height-age / site-index model for black spruce and trembling aspen (NW Ontario).
#
# Validation tiers:
#   * Trembling aspen -> TIER 1 fidelity. An independent re-implementation of the
#     NRCan SAS macros %HT_Carmean_2006 / %SI_Carmean_2006 (aspen coefficients
#     b1=4.36, b2=0.6654, b3=1.2137, b4=-0.0761; base age 50) is inlined below;
#     the package function must match it to machine precision.
#   * Black spruce -> TIER 2 plausibility only. The source PDF has no worked
#     example / reference table and no independent implementation exists, and the
#     exponent b2 is a best reading of a degraded raster. The guards for black
#     spruce are the exact-at-base-age identity (height == si at BHA 50),
#     the predict->invert round-trip, and monotonicity.

# --- structural -------------------------------------------------------------

test_that("si_carmean2006 returns a well-formed tibble (predict height)", {
  out <- si_carmean2006(
    age = c(25, 50, 80),
    si = c(12, 17, 20),
    species = c("PICE.MAR", "PICE.MAR", "POPU.TRE")
  )
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
  expect_equal(nrow(out), 3)
})

test_that("si_carmean2006 returns a well-formed tibble (predict si)", {
  out <- si_carmean2006(
    age = c(25, 80),
    height = c(9, 22),
    species = c("PICE.MAR", "POPU.TRE")
  )
  expect_s3_class(out, "tbl_df")
  expect_named(out, "si")
  expect_equal(nrow(out), 2)
})

test_that("si_carmean2006 recycles inputs to a common length", {
  out <- si_carmean2006(age = 50, si = c(12, 18), species = "PICE.MAR")
  expect_equal(nrow(out), 2)
})

test_that("si_carmean2006 errors on incompatible input lengths", {
  expect_error(
    si_carmean2006(age = c(25, 50), si = c(12, 18, 24), species = "PICE.MAR")
  )
})

test_that("si_carmean2006 requires exactly one of height / si", {
  expect_error(si_carmean2006(age = 50, species = "PICE.MAR"))
  expect_error(
    si_carmean2006(age = 50, height = 17, si = 17, species = "PICE.MAR")
  )
})

test_that("si_carmean2006 validates numeric inputs", {
  expect_error(si_carmean2006(age = -5, si = 17, species = "PICE.MAR"))
  expect_error(si_carmean2006(age = 50, si = -2, species = "PICE.MAR"))
  expect_error(si_carmean2006(age = 50, si = NA_real_, species = "PICE.MAR"))
})

test_that("si_carmean2006 rejects unsupported species", {
  expect_error(
    si_carmean2006(age = 50, si = 17, species = "PINU.BAN"),
    "No Carmean2006 parameters"
  )
})

test_that("si_carmean2006 rejects si / height at or below breast height", {
  expect_error(si_carmean2006(age = 25, si = 1.2, species = "PICE.MAR"), "1.3")
  expect_error(
    si_carmean2006(age = 25, height = 1.0, species = "POPU.TRE"),
    "1.3"
  )
})

test_that("si_carmean2006 aborts on non-finite height from out-of-domain si", {
  # For black spruce, a site index far above the model's domain yields a
  # non-finite height and the forward prediction aborts.
  expect_error(
    suppressWarnings(si_carmean2006(age = 25, si = 60, species = "PICE.MAR")),
    "Non-finite height"
  )
})

test_that("si_carmean2006 aborts on non-invertible height", {
  # A height above the maximum attainable by the black-spruce curve cannot be
  # bracketed to a site index; the inverse solver returns NaN and aborts. This
  # also exercises the adaptive bracket-expansion path.
  expect_error(
    suppressWarnings(si_carmean2006(
      age = 25,
      height = 40,
      species = "PICE.MAR"
    )),
    "Non-finite site index"
  )
})

# --- fidelity guard 1: exact at base age (height == si at BHA 50) -----------

test_that("si_carmean2006 returns height == si at breast-height age 50", {
  expect_equal(
    si_carmean2006(age = 50, si = 14, species = "PICE.MAR")$height,
    14
  )
  expect_equal(
    si_carmean2006(age = 50, si = 19, species = "POPU.TRE")$height,
    19
  )
  # inverse is also exact at base age
  expect_equal(
    si_carmean2006(age = 50, height = 14, species = "PICE.MAR")$si,
    14
  )
  expect_equal(
    si_carmean2006(age = 50, height = 19, species = "POPU.TRE")$si,
    19
  )
})

# --- fidelity guard 2: predict -> invert round-trip -------------------------

test_that("si_carmean2006 predict/invert round-trip recovers si", {
  ages <- c(15, 30, 80, 100)
  for (sp in c("PICE.MAR", "POPU.TRE")) {
    sis <- if (sp == "PICE.MAR") c(8, 12, 16) else c(14, 18, 22)
    for (s in sis) {
      h <- si_carmean2006(age = ages, si = s, species = sp)$height
      rec <- si_carmean2006(age = ages, height = h, species = sp)$si
      expect_equal(
        rec,
        rep(s, length(ages)),
        tolerance = 1e-6,
        info = paste(sp, s)
      )
    }
  }
})

# --- TIER 1 fidelity: aspen vs independent SAS re-implementation -------------

test_that("si_carmean2006 aspen height matches the SAS %HT_Carmean_2006 macro", {
  sas_ht_aspen <- function(bhage, si) {
    b1 <- 4.36
    b2 <- 0.6654
    b3 <- 1.2137
    b4 <- -0.0761
    Tr <- 50
    s13 <- si - 1.3
    K <- 1 - (s13 / (b1 * s13^b2))^(1 / (b3 * s13^b4))
    x1 <- b1 * (1 - K^(bhage / Tr))^(b3 * s13^b4)
    1.3 + (s13^b2) * x1
  }
  grid <- expand.grid(age = c(10, 25, 50, 80, 100), si = c(15, 19, 23))
  pkg <- mapply(
    function(a, s) si_carmean2006(age = a, si = s, species = "POPU.TRE")$height,
    grid$age,
    grid$si
  )
  sas <- mapply(sas_ht_aspen, grid$age, grid$si)
  expect_equal(pkg, sas, tolerance = 1e-10)
})

test_that("si_carmean2006 aspen si matches the SAS %SI_Carmean_2006 macro", {
  sas_si_aspen <- function(bhage, height) {
    b1 <- 4.36
    b2 <- 0.6654
    b3 <- 1.2137
    b4 <- -0.0761
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
  # use heights generated from the forward SAS macro to stay in-domain, at a
  # non-base age so the constrained shortcut does not mask the solver
  heights <- c(6, 12, 18, 24)
  ages <- c(20, 35, 70, 90)
  pkg <- mapply(
    function(a, h) si_carmean2006(age = a, height = h, species = "POPU.TRE")$si,
    ages,
    heights
  )
  sas <- mapply(sas_si_aspen, ages, heights)
  expect_equal(pkg, sas, tolerance = 1e-4)
})

# --- TIER 2 plausibility: monotonicity + agreement with same-species models -

test_that("si_carmean2006 height is monotonic in age and in si (sanity check)", {
  for (sp in c("PICE.MAR", "POPU.TRE")) {
    by_age <- si_carmean2006(
      age = seq(10, 100, by = 10),
      si = 16,
      species = sp
    )$height
    expect_true(all(diff(by_age) > 0), info = sp)
    by_si <- si_carmean2006(
      age = 60,
      si = seq(8, 24, by = 2),
      species = sp
    )$height
    expect_true(all(diff(by_si) > 0), info = sp)
  }
})

test_that("si_carmean2006 black spruce agrees with cieszewskibella1991 within 20%", {
  # No source benchmark for black spruce; cross-check magnitude/shape against
  # another BHA-50 black-spruce SI model over the fitted age range.
  ages <- seq(10, 100, by = 10)
  c06 <- si_carmean2006(age = ages, si = 14, species = "PICE.MAR")$height
  cb91 <- si_cieszewskibella1991(
    age = ages,
    si = 14,
    species = "PICE.MAR"
  )$height
  rel <- abs(c06 - cb91) / cb91
  expect_true(all(rel < 0.20))
})
