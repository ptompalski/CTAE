# tests/testthat/test-si_hugarcia2009.R
# testthat 3e.

test_that("si_hugarcia2009 returns a well-formed tibble when predicting height", {
  out <- si_hugarcia2009(age = c(25, 50), si = c(12, 18), species = "PICE.GLA")
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
  expect_equal(nrow(out), 2L)
  expect_true(all(is.finite(out$height)))
})

test_that("si_hugarcia2009 returns a well-formed tibble when predicting si", {
  out <- si_hugarcia2009(
    age = c(25, 50),
    height = c(8, 18),
    species = "PICE.GLA"
  )
  expect_s3_class(out, "tbl_df")
  expect_named(out, "si")
  expect_equal(nrow(out), 2L)
  expect_true(all(is.finite(out$si)))
})

test_that("si_hugarcia2009 recycles inputs to a common length", {
  out <- si_hugarcia2009(age = c(20, 40, 60), si = 18, species = "PICE.GLA")
  expect_equal(nrow(out), 3L)
})

test_that("si_hugarcia2009 errors on incompatible input lengths", {
  expect_error(
    si_hugarcia2009(age = c(20, 40), si = c(12, 18, 24), species = "PICE.GLA")
  )
})

test_that("si_hugarcia2009 validates inputs", {
  # both height and si supplied
  expect_error(
    si_hugarcia2009(age = 50, height = 18, si = 18, species = "PICE.GLA")
  )
  # neither supplied
  expect_error(si_hugarcia2009(age = 50, species = "PICE.GLA"))
  # non-positive / non-finite age
  expect_error(si_hugarcia2009(age = 0, si = 18, species = "PICE.GLA"))
  expect_error(si_hugarcia2009(age = 50, si = -5, species = "PICE.GLA"))
  # unknown species
  expect_error(si_hugarcia2009(age = 50, si = 18, species = "PINU.CON"))
  # zero-length inputs
  expect_error(
    si_hugarcia2009(
      age = numeric(0),
      si = numeric(0),
      species = character(0)
    )
  )
})

test_that("si_hugarcia2009 aborts when the q-solve cannot succeed", {
  # A height at or below the breast-height origin (H0 = 1.3 m) has no valid site
  # parameter, so the site-index solve returns NaN and the function aborts.
  expect_error(
    si_hugarcia2009(age = 50, height = 1.3, species = "PICE.GLA"),
    regexp = "Non-finite site index"
  )

  # A site index at or below the breast-height origin likewise has no valid site
  # parameter, so height prediction aborts.
  expect_error(
    si_hugarcia2009(age = 50, si = 1.0, species = "PICE.GLA"),
    regexp = "Non-finite height"
  )
})

test_that("si_hugarcia2009 recovers site index from an extreme height (uniroot fallback)", {
  # A height well above the asymptote implied by the fixed-point start value
  # forces the solver's uniroot fallback branch. Round-tripping must still hold.
  h_big <- si_hugarcia2009(age = 50, si = 100, species = "PICE.GLA")$height
  si_rec <- si_hugarcia2009(age = 50, height = h_big, species = "PICE.GLA")$si
  expect_equal(si_rec, 100, tolerance = 1e-4)

  # A height above the asymptote at q = 1 (a = 283.9 m) forces the fallback's
  # bracket-expansion loop before uniroot succeeds.
  si_extreme <- si_hugarcia2009(age = 20, height = 350, species = "PICE.GLA")$si
  expect_true(is.finite(si_extreme) && si_extreme > 0)
})


test_that("both interior-spruce codes give identical predictions", {
  hg <- si_hugarcia2009(age = 50, si = 18, species = "PICE.GLA")$height
  he <- si_hugarcia2009(age = 50, si = 18, species = "PICE.ENG")$height
  expect_equal(hg, he)
})

# --- Validation tier 1 (fidelity via exact model self-consistency) ---
# The source publishes no numeric prediction grid, but the model form implies two
# exact identities that a faithful implementation must satisfy.

test_that("height at the base age (50 yr) equals the input site index", {
  si_in <- c(10, 15, 20, 25, 30)
  h50 <- si_hugarcia2009(age = 50, si = si_in, species = "PICE.GLA")$height
  expect_equal(h50, si_in, tolerance = 1e-6)
})

test_that("si_hugarcia2009 round-trips si -> height -> si over a grid", {
  # Self-consistency check on the numeric q-solver: for every (species, age, si)
  # combination, predicting height then recovering site index must return the
  # original si. Reference grid mirrors the committed comparison-value generator
  # (tmp/generate_si_hugarcia2009_comparison_values.R) but is inlined here so the
  # test is portable to an installed-package coverage/check run.
  ref <- expand.grid(
    species = c("PICE.GLA", "PICE.ENG"),
    age = c(15, 25, 50, 80, 120),
    si = c(10, 15, 20, 25, 30),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  height <- mapply(
    function(a, s, sp) si_hugarcia2009(age = a, si = s, species = sp)$height,
    ref$age,
    ref$si,
    ref$species
  )
  expect_true(all(is.finite(height)))

  si_rec <- mapply(
    function(a, h, sp) si_hugarcia2009(age = a, height = h, species = sp)$si,
    ref$age,
    height,
    ref$species
  )
  expect_equal(si_rec, ref$si, tolerance = 1e-4)
})

# --- Structural behaviour expected of the model form ---

test_that("predicted height increases with age and with site index", {
  h_by_age <- si_hugarcia2009(
    age = c(10, 20, 40, 80, 120),
    si = 18,
    species = "PICE.GLA"
  )$height
  expect_true(all(diff(h_by_age) > 0))

  h_by_si <- si_hugarcia2009(
    age = 40,
    si = c(8, 12, 16, 20, 24),
    species = "PICE.GLA"
  )$height
  expect_true(all(diff(h_by_si) > 0))
})

# --- Validation tier 2 (plausibility sanity check, NOT a fidelity test) ---
# Compare against si_thrower1994 white spruce (PICE.GLA) over overlapping BC
# interior ages/site indices. Different models, so only similar magnitude and
# shape are expected -- this catches gross errors, not fidelity.

test_that("si_hugarcia2009 is plausible vs si_thrower1994 white spruce (sanity)", {
  ages <- c(30, 50, 70)
  si <- 18
  hg <- si_hugarcia2009(age = ages, si = si, species = "PICE.GLA")$height
  th <- si_thrower1994(age = ages, si = si, species = "PICE.GLA")$height
  expect_true(all(abs(hg - th) / th < 0.30))
})
