# tests/testthat/test-vol_zakrzewski2013.R

test_that("vol_zakrzewski2013() validates input lengths", {
  testthat::local_mocked_bindings(
    standardize_species_code = function(x) x
  )

  testthat::expect_error(
    vol_zakrzewski2013(DBH = c(10, 20), height = 20, species = "PICE.MAR"),
    "must have the same length"
  )
})

test_that("vol_zakrzewski2013() fails fast on non-finite / non-positive DBH/height with row context", {
  testthat::local_mocked_bindings(
    standardize_species_code = function(x) x,
    get_merch_criteria = function(jurisdiction, species) {
      dplyr::tibble(
        jurisdiction = jurisdiction,
        species = species,
        stumpht_m = 0.3,
        topdbh_cm = 999, # avoid merch-height solver in tests
        mindbh_cm = 10
      )
    },
    get_volume_params = function(model_id, species) {
      dplyr::tibble(
        model_id = model_id,
        species = species,
        delta = 0.95,
        nu = 0.10,
        rho = 1.20,
        beta = 0.02,
        gamma = 0.03,
        chi = 0
      )
    }
  )

  # First row is valid, second is NA -> should fail for row 2 (now it reaches row 2)
  testthat::expect_error(
    vol_zakrzewski2013(
      DBH = c(20, NA),
      height = c(22, 22),
      species = c("PICE.MAR", "PICE.MAR")
    ),
    "failed for row 2"
  )

  testthat::expect_error(
    vol_zakrzewski2013(DBH = c(-1), height = c(22), species = c("PICE.MAR")),
    "DBH must be > 0"
  )

  testthat::expect_error(
    vol_zakrzewski2013(DBH = c(20), height = c(0), species = c("PICE.MAR")),
    "height must be > 0"
  )
})

test_that("vol_zakrzewski2013() returns zeros when DBH < mindbh", {
  testthat::local_mocked_bindings(
    standardize_species_code = function(x) x,
    get_merch_criteria = function(jurisdiction, species) {
      dplyr::tibble(
        jurisdiction = jurisdiction,
        species = species,
        stumpht_m = 0.3,
        topdbh_cm = 999, # irrelevant (dbh<mindbh path)
        mindbh_cm = 30
      )
    },
    get_volume_params = function(model_id, species) {
      dplyr::tibble(
        model_id = model_id,
        species = species,
        delta = 0.95,
        nu = 0.10,
        rho = 1.20,
        beta = 0.02,
        gamma = 0.03,
        chi = 0
      )
    }
  )

  out <- vol_zakrzewski2013(
    DBH = c(10, 29.9),
    height = c(20, 25),
    species = c("PICE.MAR", "PICE.MAR")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, c("vol_total", "vol_merchantable"))
  testthat::expect_equal(out$vol_total, c(0, 0))
  testthat::expect_equal(out$vol_merchantable, c(0, 0))
})

test_that("vol_zakrzewski2013() errors when merch criteria missing for a species", {
  testthat::local_mocked_bindings(
    standardize_species_code = function(x) x,
    get_merch_criteria = function(jurisdiction, species) {
      if (identical(species, "MISSING.SPP")) {
        dplyr::tibble(
          jurisdiction = character(),
          species = character(),
          stumpht_m = numeric(),
          topdbh_cm = numeric(),
          mindbh_cm = numeric()
        )
      } else {
        dplyr::tibble(
          jurisdiction = jurisdiction,
          species = species,
          stumpht_m = 0.3,
          topdbh_cm = 999,
          mindbh_cm = 10
        )
      }
    },
    get_volume_params = function(model_id, species) {
      dplyr::tibble(
        model_id = model_id,
        species = species,
        delta = 0.95,
        nu = 0.10,
        rho = 1.20,
        beta = 0.02,
        gamma = 0.03,
        chi = 0
      )
    }
  )

  testthat::expect_error(
    vol_zakrzewski2013(DBH = 20, height = 22, species = "MISSING.SPP"),
    "No merchantability criteria found"
  )
})

test_that("vol_zakrzewski2013() errors when Zakrzewski parameters missing for a species", {
  testthat::local_mocked_bindings(
    standardize_species_code = function(x) x,
    get_merch_criteria = function(jurisdiction, species) {
      dplyr::tibble(
        jurisdiction = jurisdiction,
        species = species,
        stumpht_m = 0.3,
        topdbh_cm = 999,
        mindbh_cm = 10
      )
    },
    get_volume_params = function(model_id, species) {
      if (identical(species, "NOPARAM.SPP")) {
        dplyr::tibble(
          model_id = character(),
          species = character(),
          delta = numeric(),
          nu = numeric(),
          rho = numeric(),
          beta = numeric(),
          gamma = numeric(),
          chi = numeric()
        )
      } else {
        dplyr::tibble(
          model_id = model_id,
          species = species,
          delta = 0.95,
          nu = 0.10,
          rho = 1.20,
          beta = 0.02,
          gamma = 0.03,
          chi = 0
        )
      }
    }
  )

  testthat::expect_error(
    vol_zakrzewski2013(DBH = 20, height = 22, species = "NOPARAM.SPP"),
    "No Zakrzewski2013 parameters found"
  )
})

test_that("vol_zakrzewski2013() errors when gamma is zero", {
  testthat::local_mocked_bindings(
    standardize_species_code = function(x) x,
    get_merch_criteria = function(jurisdiction, species) {
      dplyr::tibble(
        jurisdiction = jurisdiction,
        species = species,
        stumpht_m = 0.3,
        topdbh_cm = 999,
        mindbh_cm = 10
      )
    },
    get_volume_params = function(model_id, species) {
      dplyr::tibble(
        model_id = model_id,
        species = species,
        delta = 0.95,
        nu = 0.10,
        rho = 1.20,
        beta = 0.02,
        gamma = 0,
        chi = 0
      )
    }
  )

  testthat::expect_error(
    vol_zakrzewski2013(DBH = 20, height = 22, species = "PICE.MAR"),
    "gamma must be non-zero"
  )
})

test_that("vol_zakrzewski2013() is vectorized and returns finite volumes for typical inputs", {
  testthat::local_mocked_bindings(
    standardize_species_code = function(x) x,
    get_merch_criteria = function(jurisdiction, species) {
      dplyr::tibble(
        jurisdiction = jurisdiction,
        species = species,
        stumpht_m = 0.3,
        topdbh_cm = 999, # avoid merch-height solver
        mindbh_cm = 10
      )
    },
    get_volume_params = function(model_id, species) {
      dplyr::tibble(
        model_id = model_id,
        species = species,
        delta = 0.95,
        nu = 0.10,
        rho = 1.20,
        beta = 0.02,
        gamma = 0.03,
        chi = 0
      )
    }
  )

  out <- vol_zakrzewski2013(
    DBH = c(20, 30, 40),
    height = c(18, 22, 28),
    species = c("PICE.MAR", "PINU.STR", "THUJ.OCC")
  )

  testthat::expect_equal(nrow(out), 3)
  testthat::expect_true(all(is.finite(out$vol_total)))
  testthat::expect_true(all(out$vol_total >= 0))
  testthat::expect_true(all(is.finite(out$vol_merchantable)))
  testthat::expect_true(all(out$vol_merchantable >= 0))
  testthat::expect_true(all(out$vol_merchantable <= out$vol_total + 1e-12))
})

test_that("vol_zakrzewski2013() respects standardized species codes (one call, cached lookups)", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L

  testthat::local_mocked_bindings(
    standardize_species_code = function(x) rep("PICE.MAR", length(x)),
    get_merch_criteria = function(jurisdiction, species) {
      calls$n <- calls$n + 1L
      dplyr::tibble(
        jurisdiction = jurisdiction,
        species = species,
        stumpht_m = 0.3,
        topdbh_cm = 999, # avoid merch-height solver
        mindbh_cm = 10
      )
    },
    get_volume_params = function(model_id, species) {
      dplyr::tibble(
        model_id = model_id,
        species = species,
        delta = 0.95,
        nu = 0.10,
        rho = 1.20,
        beta = 0.02,
        gamma = 0.03,
        chi = 0
      )
    }
  )

  out <- vol_zakrzewski2013(
    DBH = c(20, 25, 30),
    height = c(20, 22, 24),
    species = c("PICEGLA", "PICE.MAR", "PICEGLA")
  )

  testthat::expect_equal(nrow(out), 3)
  testthat::expect_equal(calls$n, 1L)
})
testthat::test_that("zakrzewski2013 returns positive total volume below mindbh", {
  # pick an ON conifer (mindbh ~ 9.1 cm) and a small tree
  out <- vol_zakrzewski2013(DBH = 6, height = 8, species = "PICE.MAR")

  testthat::expect_gt(out$vol_total, 0)
  testthat::expect_equal(out$vol_merchantable, 0)
})
