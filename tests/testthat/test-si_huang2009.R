# tests for si_huang2009 (Huang, Meng & Yang 2009 GYPSY Alberta models)
# testthat 3e. Fidelity benchmark: Appendix-1 worked-example output (p. 22).

sp4 <- c("POPU.TRE", "PICE.MAR", "PINU.CON", "PICE.GLA")

test_that("si_huang2009 returns a well-formed tibble (predict si)", {
  out <- si_huang2009(
    age = c(50, 70),
    height = c(20, 20),
    species = c("PICE.MAR", "PINU.CON")
  )
  expect_s3_class(out, "tbl_df")
  expect_named(out, "si")
  expect_equal(nrow(out), 2)
})

test_that("si_huang2009 returns a well-formed tibble (predict height)", {
  out <- si_huang2009(
    age = c(50, 90),
    si = c(20, 12),
    species = c("POPU.TRE", "PICE.GLA"),
    index_age = "total"
  )
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
  expect_equal(nrow(out), 2)
})

test_that("si_huang2009 recycles inputs to a common length", {
  out <- si_huang2009(age = c(40, 60, 80), height = 18, species = "PINU.CON")
  expect_equal(nrow(out), 3)
})

test_that("si_huang2009 errors on incompatible input lengths", {
  expect_error(si_huang2009(
    age = c(30, 50),
    height = c(10, 15, 20),
    species = "PICE.MAR"
  ))
})

test_that("si_huang2009 requires exactly one of height/si", {
  expect_error(si_huang2009(age = 50, species = "PICE.MAR"), "exactly one")
  expect_error(
    si_huang2009(age = 50, height = 20, si = 18, species = "PICE.MAR"),
    "exactly one"
  )
})

test_that("si_huang2009 validates index_age", {
  expect_error(si_huang2009(
    age = 50,
    height = 20,
    species = "PICE.MAR",
    index_age = "bogus"
  ))
})

test_that("si_huang2009 validates numeric inputs", {
  expect_error(si_huang2009(age = -1, height = 20, species = "PICE.MAR"))
  expect_error(si_huang2009(age = 50, height = -20, species = "PICE.MAR"))
  expect_error(si_huang2009(age = NA_real_, height = 20, species = "PICE.MAR"))
  expect_error(si_huang2009(
    age = numeric(0),
    height = numeric(0),
    species = character(0)
  ))
})

test_that("si_huang2009 errors on unknown species", {
  expect_error(
    si_huang2009(age = 50, height = 20, species = "ABIE.BAL"),
    "No Huang2009 parameters"
  )
})

# --- TIER 1 fidelity: Appendix-1 worked example (p. 22) ---
# Values transcribed directly from the source report's SAS worked-example
# output (topht = 20 m). The comparison-value generator under tmp/ regenerates
# these, but tmp/ is .Rbuildignore'd and absent from installed-package check
# runs, so the reference grid is inlined here to keep the test portable.
huang2009_worked_example <- function() {
  data.frame(
    species = c(
      "POPU.TRE",
      "POPU.TRE",
      "PICE.MAR",
      "PICE.MAR",
      "PINU.CON",
      "PINU.CON",
      "PICE.GLA",
      "PICE.GLA"
    ),
    totage = c(50, 60, 50, 70, 50, 80, 50, 90),
    topht = 20,
    exp_SIt = c(20, 18.1340, 20, 16.1033, 20, 15.0905, 20, 12.2179),
    exp_SIbh = c(
      20.5285,
      18.7356,
      21.3726,
      17.7449,
      21.3314,
      16.5410,
      21.7585,
      14.3948
    ),
    stringsAsFactors = FALSE
  )
}

test_that("si_huang2009 matches published SIt worked-example values", {
  ref <- huang2009_worked_example()
  sit <- si_huang2009(
    age = ref$totage,
    height = ref$topht,
    species = ref$species,
    index_age = "total"
  )$si
  expect_equal(sit, ref$exp_SIt, tolerance = 1e-3)
})

test_that("si_huang2009 matches published SIbh worked-example values", {
  ref <- huang2009_worked_example()
  sibh <- si_huang2009(
    age = ref$totage,
    height = ref$topht,
    species = ref$species,
    index_age = "breast_height"
  )$si
  expect_equal(sibh, ref$exp_SIbh, tolerance = 1e-3)
})

test_that("si_huang2009 is self-consistent at total age 50 (SIt == topht)", {
  # At base age 50, Htop(50, SIt) = SIt for every species.
  h <- si_huang2009(
    age = rep(50, 4),
    si = rep(18, 4),
    species = sp4,
    index_age = "total"
  )$height
  expect_equal(h, rep(18, 4), tolerance = 1e-6)
})

test_that("si_huang2009 round-trips SIt (height -> si -> height)", {
  h <- si_huang2009(
    age = c(40, 60, 80, 90),
    si = c(15, 18, 20, 22),
    species = sp4,
    index_age = "total"
  )$height
  si_back <- si_huang2009(
    age = c(40, 60, 80, 90),
    height = h,
    species = sp4,
    index_age = "total"
  )$si
  expect_equal(si_back, c(15, 18, 20, 22), tolerance = 1e-4)
})

test_that("si_huang2009 round-trips SIbh (si -> height -> si)", {
  sibh <- si_huang2009(
    age = c(60, 70, 80, 90),
    height = c(18, 16, 20, 15),
    species = sp4
  )$si
  h <- si_huang2009(age = c(60, 70, 80, 90), si = sibh, species = sp4)$height
  expect_equal(h, c(18, 16, 20, 15), tolerance = 1e-3)
})

# --- Cross-check vs. si_huang1994 (overlapping Alberta species) ---
test_that("si_huang2009 is broadly consistent with si_huang1994 (sanity check)", {
  # Both cover PICE.GLA, PICE.MAR, PINU.CON, POPU.TRE. huang1994 is bh-age;
  # huang2009 age is total age (offset by the report's bhage->totage factors).
  off <- c(POPU.TRE = 4, PINU.CON = 8, PICE.GLA = 12, PICE.MAR = 15)
  for (sp in names(off)) {
    h94 <- si_huang1994(age = 50, si = 18, species = sp)$height
    h09 <- si_huang2009(
      age = 50 + off[[sp]],
      si = 18,
      species = sp,
      index_age = "breast_height"
    )$height
    expect_lt(abs(h09 - h94) / h94, 0.25)
  }
})

test_that("si_huang2009 predicts increasing height with total age", {
  h <- si_huang2009(
    age = c(20, 40, 60, 80),
    si = 18,
    species = "PICE.GLA",
    index_age = "total"
  )$height
  expect_true(all(diff(h) > 0))
})

test_that("si_huang2009 recovers SIt across a wide valid SIbh range", {
  # Exercises the lower-bound search and upper bracket-expansion in the
  # SIbh -> SIt inversion.
  sibh <- c(8, 15, 22, 30)
  out <- si_huang2009(
    age = rep(50, length(sibh)),
    si = sibh,
    species = rep("PICE.MAR", length(sibh))
  )
  expect_equal(nrow(out), length(sibh))
  expect_true(all(is.finite(out$height)))
})

test_that("si_huang2009 aborts when SIbh is below the achievable domain", {
  # Very small SIbh for black spruce is unreachable given its Y2BH offset;
  # the inversion cannot bracket a solution and errors informatively.
  expect_error(
    si_huang2009(age = 50, si = 2, species = "PICE.MAR"),
    "Failed to recover total-age site index"
  )
})
