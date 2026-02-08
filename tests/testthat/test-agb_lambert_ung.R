# tests/testthat/test-agb_lambert_ung.R

test_that("agb_lambert_ung returns expected columns and row counts", {
  skip_if_not(exists("agb_lambert_ung", mode = "function"))
  skip_if_not(
    exists("parameters_LambertUng", mode = "function") ||
      exists("parameters_LambertUng", mode = "numeric") ||
      exists("parameters_LambertUng", mode = "list") ||
      exists("parameters_LambertUng", mode = "environment") ||
      exists("parameters_LambertUng", inherits = TRUE)
  )

  x <- agb_lambert_ung(DBH = 20, species = "PICE.MAR")

  expect_s3_class(x, "tbl_df")
  expect_equal(nrow(x), 1L)

  # default output: components only
  expect_named(
    x,
    c("Bwood", "Bbark", "Bstem", "Bfoliage", "Bbranches", "Bcrown", "Btotal")
  )
})

test_that("keep_model_id adds model_id as last column and does not include species", {
  skip_if_not(exists("agb_lambert_ung", mode = "function"))

  x <- agb_lambert_ung(DBH = 20, species = "PICE.MAR", keep_model_id = TRUE)

  expect_s3_class(x, "tbl_df")
  expect_equal(nrow(x), 1L)

  # model_id present and last
  expect_true("model_id" %in% names(x))
  expect_equal(tail(names(x), 1), "model_id")

  # species should not be returned
  expect_false("species" %in% names(x))
})

test_that("equation_set='auto' chooses DBH vs DBHHT per row based on height availability", {
  skip_if_not(exists("agb_lambert_ung", mode = "function"))

  trees <- tibble::tibble(
    DBH = c(22, 30, 18, 35),
    height = c(18, 24, NA_real_, 27),
    species = c("PICE.GLA", "PICE.GLA", "ABIE.BAL", "PINU.BAN")
  )

  out <- agb_lambert_ung(
    DBH = trees$DBH,
    height = trees$height,
    species = trees$species,
    equation_set = "auto",
    keep_model_id = TRUE
  )

  expect_equal(nrow(out), nrow(trees))
  expect_equal(
    out$model_id,
    ifelse(is.na(trees$height), "DBH", "DBHHT")
  )

  # model_id is last
  expect_equal(tail(names(out), 1), "model_id")
})

test_that("output row order matches input order (no reordering across groups)", {
  skip_if_not(exists("agb_lambert_ung", mode = "function"))

  # intentionally mix species + missing height to create multiple (model_id, species) groups
  trees <- tibble::tibble(
    tree_id = 101:106,
    DBH = c(10, 25, 12, 30, 18, 35),
    height = c(NA, 18, 22, NA, 15, 27),
    species = c(
      "PICE.GLA",
      "PICE.GLA",
      "PINU.BAN",
      "PINU.BAN",
      "ABIE.BAL",
      "PICE.MAR"
    )
  )

  out <- agb_lambert_ung(
    DBH = trees$DBH,
    height = trees$height,
    species = trees$species,
    keep_model_id = TRUE
  )

  # Check that model_id aligns with the input row positions
  expect_equal(
    out$model_id,
    ifelse(is.na(trees$height), "DBH", "DBHHT")
  )
})

test_that("can be attached via mutate() without rowwise/list-column explosion", {
  skip_if_not(exists("agb_lambert_ung", mode = "function"))

  trees <- tibble::tibble(
    tree_id = 1:4,
    DBH = c(22, 30, 18, 35),
    height = c(18, 24, NA_real_, 27),
    species = c("PICE.GLA", "PICE.GLA", "ABIE.BAL", "PINU.BAN")
  )

  # preferred pattern: mutate with data-frame columns (vectorized)
  out <- trees |>
    dplyr::mutate(
      agb_lambert_ung(DBH, height, species, keep_model_id = TRUE)
    )

  expect_equal(nrow(out), nrow(trees))
  expect_true(all(c("Bwood", "Bbark", "Btotal", "model_id") %in% names(out)))

  # sanity: model_id aligns with height presence
  expect_equal(out$model_id, ifelse(is.na(trees$height), "DBH", "DBHHT"))
})

test_that("agb_lambert_ung matches manual DBH-only calculations (hard-coded parameters)", {
  DBH <- c(10, 20, 30)
  spp <- "PICE.MAR"

  # ---------------------------------------------------------------------
  # HARD-CODED COEFFICIENTS (pasted from Ung et al table 3)
  # Model: DBH-only
  # Species: PICE.MAR
  # ---------------------------------------------------------------------
  coefs <- c(
    bwood1 = 0.0494,
    bwood2 = 2.5025,
    bbark1 = 0.0148,
    bbark2 = 2.2494,
    bfoliage1 = 0.1631,
    bfoliage2 = 1.4222,
    bbranches1 = 0.0291,
    bbranches2 = 2.0751
  )

  # ---------------------------------------------------------------------
  # MANUAL CALCULATIONS (formula fixed; only coefficients change)
  # ---------------------------------------------------------------------
  Ywood <- coefs["bwood1"] * DBH^(coefs["bwood2"])
  Ybark <- coefs["bbark1"] * DBH^(coefs["bbark2"])
  Yfoliage <- coefs["bfoliage1"] * DBH^(coefs["bfoliage2"])
  Ybranches <- coefs["bbranches1"] * DBH^(coefs["bbranches2"])

  expected <- tibble::tibble(
    Bwood = as.numeric(Ywood),
    Bbark = as.numeric(Ybark),
    Bstem = as.numeric(Ywood + Ybark),
    Bfoliage = as.numeric(Yfoliage),
    Bbranches = as.numeric(Ybranches),
    Bcrown = as.numeric(Yfoliage + Ybranches),
    Btotal = as.numeric(Ywood + Ybark + Yfoliage + Ybranches)
  )

  # ---------------------------------------------------------------------
  # FUNCTION OUTPUT
  # ---------------------------------------------------------------------
  got <- agb_lambert_ung(
    DBH = DBH,
    species = spp,
    equation_set = "dbh"
  )

  expect_equal(got, expected, tolerance = 1e-10)
})

test_that("agb_lambert_ung matches manual DBH+height calculations (hard-coded parameters)", {
  DBH <- c(10, 20, 30)
  H <- c(12, 18, 25)
  spp <- "PICE.MAR"

  # ---------------------------------------------------------------------
  # HARD-CODED COEFFICIENTS (pasted from Ung et al table 4)
  # Model: DBH + height
  # Species: PICE.MAR
  # ---------------------------------------------------------------------
  coefs <- c(
    bwood1 = 0.0335,
    bwood2 = 1.7389,
    bwood3 = 0.9835,
    bbark1 = 0.0132,
    bbark2 = 1.7657,
    bbark3 = 0.5775,
    bfoliage1 = 0.2078,
    bfoliage2 = 2.5517,
    bfoliage3 = -1.3453,
    bbranches1 = 0.0405,
    bbranches2 = 3.1917,
    bbranches3 = -1.3674
  )
  # ---------------------------------------------------------------------
  # MANUAL CALCULATIONS (formula fixed; only coefficients change)
  # ---------------------------------------------------------------------
  Ywood <- coefs["bwood1"] * DBH^(coefs["bwood2"]) * H^(coefs["bwood3"])
  Ybark <- coefs["bbark1"] * DBH^(coefs["bbark2"]) * H^(coefs["bbark3"])
  Yfoliage <- coefs["bfoliage1"] *
    DBH^(coefs["bfoliage2"]) *
    H^(coefs["bfoliage3"])
  Ybranches <- coefs["bbranches1"] *
    DBH^(coefs["bbranches2"]) *
    H^(coefs["bbranches3"])

  expected <- tibble::tibble(
    Bwood = as.numeric(Ywood),
    Bbark = as.numeric(Ybark),
    Bstem = as.numeric(Ywood + Ybark),
    Bfoliage = as.numeric(Yfoliage),
    Bbranches = as.numeric(Ybranches),
    Bcrown = as.numeric(Yfoliage + Ybranches),
    Btotal = as.numeric(Ywood + Ybark + Yfoliage + Ybranches)
  )

  # ---------------------------------------------------------------------
  # FUNCTION OUTPUT
  # ---------------------------------------------------------------------
  got <- agb_lambert_ung(
    DBH = DBH,
    height = H,
    species = spp,
    equation_set = "dbh_height"
  )

  expect_equal(got, expected, tolerance = 1e-10)
})
