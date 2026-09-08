# tests/testthat/test-si_nigh2004.R
# testthat 3e. Nigh (2004) juvenile height-age (site index) model for lodgepole
# pine and interior spruce in British Columbia.
#
# Validation tier: TIER 2 (plausibility). Nigh (2004) provides no numeric
# reference table (Figure 4 is graphical only), so there is no source fidelity
# benchmark. Tests rely on: the H = 0 at A = 0 conditioning, monotonicity in age
# and site index, an exact predict -> invert round-trip, and a cross-check that
# the new province-wide pine curve tracks the superseded Nigh & Love (1999)
# equation (eq. 1) in magnitude while avoiding its negative heights at low SI.

test_that("si_nigh2004 returns a well-formed tibble (predict height)", {
  out <- si_nigh2004(age = c(5, 10, 15), species = "PINU.CON", si = 20)
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
  expect_equal(nrow(out), 3)
})

test_that("si_nigh2004 returns a well-formed tibble (predict si)", {
  out <- si_nigh2004(age = c(5, 10), species = "PINU.CON", height = c(0.8, 2.5))
  expect_s3_class(out, "tbl_df")
  expect_named(out, "si")
  expect_equal(nrow(out), 2)
})

test_that("si_nigh2004 recycles inputs to a common length", {
  out <- si_nigh2004(age = 10, species = c("PINU.CON", "PICE.GLA"), si = 20)
  expect_equal(nrow(out), 2)
  out2 <- si_nigh2004(
    age = c(8, 8), species = c("PINU.CON", "PICE.GLA"),
    si = 20, bec_zone = c("ICH", "IDF")
  )
  expect_equal(nrow(out2), 2)
})

test_that("si_nigh2004 errors on incompatible input lengths", {
  expect_error(si_nigh2004(age = c(5, 10), species = "PINU.CON", si = c(12, 18, 24)))
})

test_that("si_nigh2004 requires exactly one of height / si", {
  expect_error(si_nigh2004(age = 10, species = "PINU.CON"))
  expect_error(si_nigh2004(age = 10, species = "PINU.CON", height = 2, si = 18))
})

test_that("si_nigh2004 validates numeric inputs", {
  expect_error(si_nigh2004(age = -5, species = "PINU.CON", si = 18))
  expect_error(si_nigh2004(age = 10, species = "PINU.CON", si = -2))
  expect_error(si_nigh2004(age = 10, species = "PINU.CON", si = NA_real_))
})

test_that("si_nigh2004 rejects unknown species and zones", {
  expect_error(
    si_nigh2004(age = 10, species = "ABIE.BAL", si = 18),
    "species"
  )
  expect_error(
    si_nigh2004(age = 10, species = "PINU.CON", si = 18, bec_zone = "XYZ"),
    "bec_zone"
  )
})

test_that("si_nigh2004 accepts both species and all seven zones plus province-wide", {
  zones <- c("BWBS", "ESSF", "ICH", "IDF", "MS", "SBS", "SBPS")
  for (sp in c("PINU.CON", "PICE.GLA")) {
    prov <- si_nigh2004(age = 10, species = sp, si = 18)$height
    expect_true(is.finite(prov) && prov > 0)
    for (z in zones) {
      h <- si_nigh2004(age = 10, species = sp, si = 18, bec_zone = z)$height
      expect_true(is.finite(h) && h > 0)
    }
  }
})

test_that("si_nigh2004 is conditioned to height ~ 0 as age approaches 0", {
  h <- si_nigh2004(age = 1e-4, species = "PINU.CON", si = 20)$height
  expect_lt(h, 1e-3)
})

test_that("si_nigh2004 height increases with age and with site index", {
  ages <- si_nigh2004(age = c(2, 5, 8, 12, 15), species = "PICE.GLA", si = 18)$height
  expect_true(all(diff(ages) > 0))
  sis <- si_nigh2004(age = 10, species = "PICE.GLA", si = c(10, 15, 20, 25))$height
  expect_true(all(diff(sis) > 0))
})

test_that("si_nigh2004 predict -> invert round-trips site index", {
  for (sp in c("PINU.CON", "PICE.GLA")) {
    for (z in list(NULL, "SBS")) {
      h <- si_nigh2004(age = 10, species = sp, si = 15, bec_zone = z)$height
      si_back <- si_nigh2004(age = 10, species = sp, height = h, bec_zone = z)$si
      expect_equal(si_back, 15, tolerance = 1e-4)
    }
  }
})

test_that("si_nigh2004 aborts on non-finite si prediction (height out of range)", {
  # A height far above what any SI in (0, 60] produces at this age cannot be
  # bracketed, surfacing as a non-finite abort.
  expect_error(
    si_nigh2004(age = 5, species = "PINU.CON", height = 1e6),
    "Non-finite"
  )
})

# --- Cross-check vs. the superseded Nigh & Love (1999) pine model (eq. 1) ---
test_that("si_nigh2004 province-wide pine tracks Nigh & Love (1999) in magnitude", {
  old_eq1 <- function(A, SI) (-0.03993 + 0.004828 * SI) * A^1.902 * 0.9645^A
  ages <- c(5, 8, 10, 12, 15)
  new_h <- si_nigh2004(age = ages, species = "PINU.CON", si = 20)$height
  old_h <- old_eq1(ages, 20)
  # same order of magnitude across the juvenile range
  expect_true(all(abs(new_h - old_h) / old_h < 0.35))
  # model 3 stays positive at low SI where eq. 1 goes negative
  expect_true(all(si_nigh2004(age = ages, species = "PINU.CON", si = 5)$height > 0))
  expect_true(any(old_eq1(ages, 5) < 0))
})
