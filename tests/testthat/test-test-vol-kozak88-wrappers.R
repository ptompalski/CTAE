# tests/testthat/test-vol-kozak88-wrappers.R

test_that("vol_* kozak88 wrappers return expected shape and non-negative volumes", {
  skip_if_not(exists("get_volume_params", mode = "function"))
  skip_if_not(exists("get_merch_criteria", mode = "function"))

  # pick species that should exist in each parameter table (adjust if needed)
  ab_sp <- "PICE.MAR"
  mb_sp <- "PINU.BAN"
  sk_sp <- "PICE.GLA"

  ab <- vol_huang94(
    DBH = 20,
    height = 20,
    species = ab_sp,
    subregion = "Province"
  )
  mb <- vol_klos2007(
    DBH = 20,
    height = 20,
    species = mb_sp,
    subregion = "Province"
  )
  sk <- vol_galbella94(DBH = 20, height = 20, species = sk_sp)

  for (x in list(ab, mb, sk)) {
    expect_s3_class(x, "tbl_df")
    expect_named(x, c("vol_total", "vol_merchantable"))
    expect_equal(nrow(x), 1)
    expect_true(all(is.finite(unlist(x))))
    expect_true(all(unlist(x) >= 0))
    expect_true(x$vol_total[[1]] >= x$vol_merchantable[[1]])
  }
})

test_that("vol_kozak88_engine recycles scalars and returns correct row count", {
  # call engine directly (internal); use ::: in tests
  skip_if_not(
    "vol_kozak88_engine" %in%
      getNamespaceExports("CTAE") ||
      exists(
        "vol_kozak88_engine",
        envir = asNamespace("CTAE"),
        inherits = FALSE
      )
  )

  eng <- CTAE:::vol_kozak88_engine

  # use a wrapper-known model/species (AB example); adjust if needed
  res <- eng(
    DBH = c(15, 25, 35),
    height = 20,
    species = "PICE.MAR",
    subregion = "Province",
    jurisdiction = "AB",
    model_id = "regional_huang94"
  )

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("vol_total", "vol_merchantable"))
  expect_equal(nrow(res), 3)
  expect_true(all(is.finite(unlist(res))))
  expect_true(all(unlist(res) >= 0))
  expect_true(all(res$vol_total >= res$vol_merchantable))
})

test_that("length incompatibility aborts with clear message", {
  eng <- CTAE:::vol_kozak88_engine

  expect_error(
    eng(
      DBH = c(10, 20),
      height = c(15, 20, 25),
      species = "PICE.MAR",
      subregion = "Province",
      jurisdiction = "AB",
      model_id = "regional_huang94"
    ),
    "compatible lengths",
    fixed = FALSE
  )
})

test_that("invalid DBH/height inputs abort with row-context", {
  eng <- CTAE:::vol_kozak88_engine

  expect_error(
    eng(
      DBH = c(20, NA_real_),
      height = c(20, 20),
      species = "PICE.MAR",
      subregion = "Province",
      jurisdiction = "AB",
      model_id = "regional_huang94"
    ),
    "failed for row 2",
    fixed = FALSE
  )

  expect_error(
    eng(
      DBH = c(0, 20),
      height = c(20, 20),
      species = "PICE.MAR",
      subregion = "Province",
      jurisdiction = "AB",
      model_id = "regional_huang94"
    ),
    "DBH must be > 0",
    fixed = FALSE
  )

  expect_error(
    eng(
      DBH = c(20, 20),
      height = c(-1, 20),
      species = "PICE.MAR",
      subregion = "Province",
      jurisdiction = "AB",
      model_id = "regional_huang94"
    ),
    "height must be > 0",
    fixed = FALSE
  )
})

test_that("DBH below mindbh returns zeros (both total and merch)", {
  skip_if_not(exists("get_merch_criteria", mode = "function"))

  sp <- "PICE.MAR"
  mc <- get_merch_criteria("AB", species = sp, verbose = FALSE)
  if (nrow(mc) == 0) {
    mc <- get_merch_criteria("AB", species = "ALL", verbose = FALSE)
  }
  skip_if(nrow(mc) == 0)

  mindbh <- mc$mindbh_cm[[1]]
  skip_if(!is.finite(mindbh))

  # ensure strictly below
  dbh <- max(0.1, mindbh - 0.1)

  out <- vol_huang94(
    DBH = dbh,
    height = 20,
    species = sp,
    subregion = "Province"
  )
  expect_equal(out$vol_total[[1]], 0)
  expect_equal(out$vol_merchantable[[1]], 0)
})

test_that("subregion fallback emits a warning when requested subregion missing", {
  eng <- CTAE:::vol_kozak88_engine

  # Choose a clearly bogus subregion to force fallback.
  # If your parameter tables actually include 'BOGUS', change this string.
  expect_warning(
    eng(
      DBH = 25,
      height = 20,
      species = "PICE.MAR",
      subregion = "BOGUS_SUBREGION",
      jurisdiction = "AB",
      model_id = "regional_huang94",
      fallback_subregion = "Province",
      subregion_map = c("UB" = "LBH")
    ),
    "using Province parameters instead",
    fixed = FALSE
  )
})

test_that("AB subregion_map UB -> LBH behaves consistently when UB is used", {
  # This test checks mapping runs without error and returns a valid tibble.
  # It does NOT require UB params to exist; UB should be mapped to LBH before lookup.
  out <- vol_huang94(
    DBH = 25,
    height = 20,
    species = "PICE.MAR",
    subregion = "UB"
  )
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("vol_total", "vol_merchantable"))
  expect_true(all(is.finite(unlist(out))))
  expect_true(all(unlist(out) >= 0))
  expect_true(out$vol_total[[1]] >= out$vol_merchantable[[1]])
})

test_that("missing parameters aborts with informative message (valid model_id)", {
  eng <- CTAE:::vol_kozak88_engine

  expect_warning(
    expect_error(
      eng(
        DBH = 25,
        height = 20,
        species = "PICE.MAR",
        subregion = "NOT_A_REAL_SUBREGION",
        jurisdiction = "AB",
        model_id = "regional_huang94",
        fallback_subregion = "ALSO_NOT_REAL"
      ),
      "No parameters found",
      fixed = FALSE
    ),
    "no parameters for",
    fixed = FALSE
  )
})
