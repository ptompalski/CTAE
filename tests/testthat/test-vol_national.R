# tests/testthat/test-vol_national_ung.R

testthat::test_that("vol_national_dbh returns a tibble with expected columns", {
  out <- CTAE::vol_national_dbh(
    DBH = 20,
    species = "PICE.GLA",
    jurisdiction = "AB"
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, c("vol_merchantable", "vol_total"))
  testthat::expect_type(out$vol_merchantable, "double")
  testthat::expect_type(out$vol_total, "double")
  testthat::expect_equal(nrow(out), 1L)
})

testthat::test_that("vol_national_dbh_ht returns a tibble with expected columns", {
  out <- CTAE::vol_national_dbh_ht(
    DBH = 20,
    height = 20,
    species = "PICE.GLA",
    jurisdiction = "AB"
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, c("vol_merchantable", "vol_total"))
  testthat::expect_type(out$vol_merchantable, "double")
  testthat::expect_type(out$vol_total, "double")
  testthat::expect_equal(nrow(out), 1L)
})

testthat::test_that("recycling works (dbh, species, jurisdiction; and height)", {
  out1 <- CTAE::vol_national_dbh(
    DBH = c(10, 20, 30),
    species = "PICE.GLA",
    jurisdiction = "AB"
  )
  testthat::expect_equal(nrow(out1), 3L)

  out2 <- CTAE::vol_national_dbh_ht(
    DBH = c(10, 20, 30),
    height = 20,
    species = "PICE.GLA",
    jurisdiction = "AB"
  )
  testthat::expect_equal(nrow(out2), 3L)

  # Vector species/jurisdiction too
  out3 <- CTAE::vol_national_dbh(
    DBH = c(10, 20, 30),
    species = c("PICE.GLA", "PICE.GLA", "PICE.GLA"),
    jurisdiction = c("AB", "AB", "AB")
  )
  testthat::expect_equal(nrow(out3), 3L)
})

testthat::test_that("species and jurisdiction are standardized (no-dot species, messy province)", {
  # Species without dot should standardize to dot form
  out_dot <- CTAE::vol_national_dbh(
    DBH = 20,
    species = "POPU.TRE",
    jurisdiction = "AB"
  )
  out_nodot <- CTAE::vol_national_dbh(
    DBH = 20,
    species = "POPUTRE",
    jurisdiction = "AB"
  )

  testthat::expect_equal(
    out_dot$vol_total,
    out_nodot$vol_total,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    out_dot$vol_merchantable,
    out_nodot$vol_merchantable,
    tolerance = 1e-12
  )

  # Jurisdiction alias should standardize (example: "PEI" -> "PE" if you support it)
  # If you don't support PEI alias yet, remove this test.
  out_pe <- CTAE::vol_national_dbh(
    DBH = 20,
    species = "PICE.GLA",
    jurisdiction = "PE"
  )
  out_pei <- CTAE::vol_national_dbh(
    DBH = 20,
    species = "PICE.GLA",
    jurisdiction = "PEI"
  )

  testthat::expect_equal(out_pe$vol_total, out_pei$vol_total, tolerance = 1e-12)
  testthat::expect_equal(
    out_pe$vol_merchantable,
    out_pei$vol_merchantable,
    tolerance = 1e-12
  )
})

testthat::test_that("returns NA volumes when parameters are missing for a valid-form species code", {
  sp_missing <- "ZZZZ.ZZZ" # valid format but should not exist in params

  out1 <- CTAE::vol_national_dbh(
    DBH = c(10, 20),
    species = sp_missing,
    jurisdiction = "AB"
  )
  testthat::expect_true(all(is.na(out1$vol_total)))
  testthat::expect_true(all(is.na(out1$vol_merchantable)))

  out2 <- CTAE::vol_national_dbh_ht(
    DBH = c(10, 20),
    height = c(15, 20),
    species = sp_missing,
    jurisdiction = "AB"
  )
  testthat::expect_true(all(is.na(out2$vol_total)))
  testthat::expect_true(all(is.na(out2$vol_merchantable)))
})

testthat::test_that("errors on unrecognized species codes", {
  testthat::expect_error(
    CTAE::vol_national_dbh(
      DBH = 20,
      species = "NOPE.NOPE",
      jurisdiction = "AB"
    ),
    "Unrecognized species codes",
    fixed = TRUE
  )

  testthat::expect_error(
    CTAE::vol_national_dbh_ht(
      DBH = 20,
      height = 20,
      species = "NOPE.NOPE",
      jurisdiction = "AB"
    ),
    "Unrecognized species codes",
    fixed = TRUE
  )
})

testthat::test_that("NA DBH and/or height propagate to NA volumes", {
  out1 <- CTAE::vol_national_dbh(
    DBH = c(20, NA_real_),
    species = "PICE.GLA",
    jurisdiction = "AB"
  )
  testthat::expect_true(is.finite(out1$vol_total[1]))
  testthat::expect_true(is.na(out1$vol_total[2]))
  testthat::expect_true(is.na(out1$vol_merchantable[2]))

  out2 <- CTAE::vol_national_dbh_ht(
    DBH = c(20, 20, NA_real_),
    height = c(20, NA_real_, 20),
    species = "PICE.GLA",
    jurisdiction = "AB"
  )
  testthat::expect_true(is.finite(out2$vol_total[1]))
  testthat::expect_true(is.na(out2$vol_total[2]))
  testthat::expect_true(is.na(out2$vol_total[3]))
})

testthat::test_that("volumes are non-negative and total >= merchantable when finite", {
  out <- CTAE::vol_national_dbh(
    DBH = c(10, 20, 30),
    species = "PICE.GLA",
    jurisdiction = "AB"
  )

  ok <- is.finite(out$vol_total) & is.finite(out$vol_merchantable)
  testthat::expect_true(all(out$vol_total[ok] >= 0))
  testthat::expect_true(all(out$vol_merchantable[ok] >= 0))
  testthat::expect_true(all(out$vol_total[ok] >= out$vol_merchantable[ok]))
})

testthat::test_that("merchantable volume is zero when DBH < mindbh", {
  # Use a jurisdiction/species combo with mindbh that is typically > 1 cm.
  # AB + ALL row often has mindbh around 10+ cm.
  merch <- CTAE::get_merch_criteria("AB", "POPU.TRE")
  testthat::expect_true(is.finite(merch$mindbh_cm))
  testthat::expect_true(merch$mindbh_cm > 0)

  # pick a DBH that is guaranteed smaller than mindbh
  dbh_small <- max(0.1, merch$mindbh_cm - 1)

  out1 <- CTAE::vol_national_dbh(
    DBH = dbh_small,
    species = "POPU.TRE",
    jurisdiction = "AB"
  )
  testthat::expect_equal(out1$vol_merchantable, 0, tolerance = 0)

  out2 <- CTAE::vol_national_dbh_ht(
    DBH = dbh_small,
    height = 15,
    species = "POPU.TRE",
    jurisdiction = "AB"
  )
  testthat::expect_equal(out2$vol_merchantable, 0, tolerance = 0)
})

testthat::test_that("input validation: DBH and height must be numeric", {
  testthat::expect_error(
    CTAE::vol_national_dbh(
      DBH = "20",
      species = "PICE.GLA",
      jurisdiction = "AB"
    ),
    "`DBH` must be numeric",
    fixed = TRUE
  )

  testthat::expect_error(
    CTAE::vol_national_dbh_ht(
      DBH = 20,
      height = "20",
      species = "PICE.GLA",
      jurisdiction = "AB"
    ),
    "`height` must be numeric",
    fixed = TRUE
  )
})

testthat::test_that("BC missing species in get_merch_criteria falls back to ALL (indirect)", {
  warns <- character(0)

  out <- withCallingHandlers(
    CTAE::get_merch_criteria("BC", NA_character_),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(out$jurisdiction, "BC")
  testthat::expect_equal(out$species, "ALL")

  # Two warnings are expected: species fallback + BEC fallback
  testthat::expect_true(any(grepl(
    "species.*not provided|Species='ALL'",
    warns
  )))
  testthat::expect_true(any(grepl("BEC_zone missing|UNKNOWN", warns)))
})
