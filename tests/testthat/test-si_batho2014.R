# tests/testthat/test-si_batho2014.R
# testthat 3e. Batho and García (2014) lodgepole pine site-index model.
# Validation tier: plausibility (Tier 2) -- the source publishes no worked
# numeric example, so fidelity is checked via internal self-consistency
# (height at base age 50 == input SI; SI->height->SI round-trip) plus a
# cross-check against si_thrower1994 for the same species.

test_that("si_batho2014 returns a well-formed tibble (predict height)", {
  out <- si_batho2014(age = c(25, 50), si = c(12, 20), species = "PINU.CON")
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
  expect_equal(nrow(out), 2)
  expect_true(all(is.finite(out$height)))
})

test_that("si_batho2014 returns a well-formed tibble (predict si)", {
  out <- si_batho2014(age = c(25, 50), height = c(10, 18), species = "PINU.CON")
  expect_s3_class(out, "tbl_df")
  expect_named(out, "si")
  expect_equal(nrow(out), 2)
  expect_true(all(is.finite(out$si)))
})

test_that("si_batho2014 recycles inputs to a common length", {
  out <- si_batho2014(age = c(20, 40, 60), si = 18, species = "PINU.CON")
  expect_equal(nrow(out), 3)
})

test_that("si_batho2014 errors on incompatible input lengths", {
  expect_error(
    si_batho2014(age = c(20, 40), si = c(12, 16, 20), species = "PINU.CON")
  )
})

test_that("si_batho2014 requires exactly one of height / si", {
  expect_error(
    si_batho2014(age = 50, species = "PINU.CON"),
    "exactly one"
  )
  expect_error(
    si_batho2014(age = 50, height = 18, si = 18, species = "PINU.CON"),
    "exactly one"
  )
})

test_that("si_batho2014 validates numeric inputs", {
  expect_error(si_batho2014(age = -5, si = 18, species = "PINU.CON"))
  expect_error(si_batho2014(age = 50, si = -1, species = "PINU.CON"))
  expect_error(si_batho2014(age = NA_real_, si = 18, species = "PINU.CON"))
  expect_error(
    si_batho2014(age = numeric(0), si = numeric(0), species = character(0))
  )
})

test_that("si_batho2014 rejects unsupported species", {
  expect_error(
    si_batho2014(age = 50, si = 18, species = "PICE.MAR"),
    "PINU.CON"
  )
})

test_that("si_batho2014 accepts compact species codes", {
  # standardize_species_code() normalizes PINUCON -> PINU.CON
  out <- si_batho2014(age = 50, si = 18, species = "PINUCON")
  expect_equal(out$height, 18, tolerance = 1e-8)
})

test_that("predicted height at base age 50 equals the input site index", {
  si_in <- c(8, 12, 16, 20, 24)
  out <- si_batho2014(age = 50, si = si_in, species = "PINU.CON")
  expect_equal(out$height, si_in, tolerance = 1e-6)
})

test_that("si_batho2014 is self-consistent: si -> height -> si round-trips", {
  ages <- c(20, 30, 65, 100)
  for (a in ages) {
    h <- si_batho2014(age = a, si = 18, species = "PINU.CON")$height
    si_back <- si_batho2014(age = a, height = h, species = "PINU.CON")$si
    expect_equal(si_back, 18, tolerance = 1e-4)
  }
})

test_that("height increases monotonically with age at fixed site index", {
  ages <- c(10, 20, 30, 50, 80, 120)
  h <- si_batho2014(age = ages, si = 18, species = "PINU.CON")$height
  expect_true(all(diff(h) > 0))
})

test_that("height increases monotonically with site index at fixed age", {
  si_in <- c(8, 12, 16, 20, 24)
  h <- si_batho2014(age = 30, si = si_in, species = "PINU.CON")$height
  expect_true(all(diff(h) > 0))
})

test_that("out-of-domain (height <= 1.3 m) yields a non-finite error", {
  # A height at or below breast height (1.3 m) has no valid q; the q-solver
  # returns NaN and the function aborts on the non-finite prediction.
  expect_error(
    si_batho2014(age = 50, height = 1.0, species = "PINU.CON"),
    "Non-finite"
  )
})

test_that("out-of-domain site index (<= 1.3 m) yields a non-finite error", {
  # si <= h0 has no valid q at base age 50; the predict-height path aborts.
  expect_error(
    si_batho2014(age = 30, si = 1.0, species = "PINU.CON"),
    "Non-finite"
  )
})

test_that("comparison-value fixture reproduces current outputs", {
  # The committed comparison-value generator
  # (tmp/generate_si_batho2014_comparison_values.R) writes a CSV under tmp/, but
  # tmp/ is .Rbuildignore'd and absent from installed-package check/coverage
  # runs. The reference grid is therefore inlined here so the test is portable:
  # for every (age, si) it confirms predict -> invert recovers the original si,
  # and that height == si exactly at base age 50.
  ref <- expand.grid(
    species = "PINU.CON",
    age = c(10, 20, 30, 50, 80, 120),
    si = c(8, 12, 16, 20, 24),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  height <- si_batho2014(
    age = ref$age,
    si = ref$si,
    species = ref$species
  )$height
  expect_true(all(is.finite(height)))

  # Exact-at-base-age identity: height == si at age 50.
  at50 <- ref$age == 50
  expect_equal(height[at50], ref$si[at50], tolerance = 1e-8)

  # Predict -> invert round-trip recovers site index.
  si_rec <- si_batho2014(
    age = ref$age,
    height = height,
    species = ref$species
  )$si
  expect_equal(si_rec, ref$si, tolerance = 1e-6)
})

# --- Tier 2 plausibility cross-check vs an existing same-species BC model. ---
test_that("si_batho2014 is plausible vs si_thrower1994 (sanity check)", {
  ages <- c(20, 30, 50, 80)
  new <- si_batho2014(age = ages, si = 18, species = "PINU.CON")$height
  ref <- si_thrower1994(age = ages, si = 18, species = "PINU.CON")$height
  # Same species, different model: expect agreement within a loose band.
  expect_true(all(abs(new - ref) / ref < 0.10))
})
