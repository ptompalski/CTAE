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
      getNamespaceExports("CanadaForestAllometry") ||
      exists(
        "vol_kozak88_engine",
        envir = asNamespace("CanadaForestAllometry"),
        inherits = FALSE
      )
  )

  eng <- CanadaForestAllometry:::vol_kozak88_engine

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
  eng <- CanadaForestAllometry:::vol_kozak88_engine

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
  eng <- CanadaForestAllometry:::vol_kozak88_engine

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

test_that("DBH below mindbh returns zero merchantable but non-zero total", {
  skip_if_not(exists("get_merch_criteria", mode = "function"))

  sp <- "PICE.MAR"
  mc <- get_merch_criteria("AB", species = sp, verbose = FALSE)
  if (nrow(mc) == 0) {
    mc <- get_merch_criteria("AB", species = "ALL", verbose = FALSE)
  }
  skip_if(nrow(mc) == 0)

  mindbh <- mc$mindbh_cm[[1]]
  skip_if(!is.finite(mindbh))

  dbh <- max(0.5, mindbh - 0.1)

  out <- vol_huang94(
    DBH = dbh,
    height = 20,
    species = sp,
    subregion = "Province"
  )

  expect_equal(out$vol_merchantable[[1]], 0)
  expect_true(out$vol_total[[1]] > 0)
})

test_that("subregion fallback emits a warning when requested subregion missing", {
  eng <- CanadaForestAllometry:::vol_kozak88_engine

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
  eng <- CanadaForestAllometry:::vol_kozak88_engine

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
test_that("Kozak88 wrappers compute total volume for small trees (below mindbh)", {
  skip_if_not(exists("get_merch_criteria", mode = "function"))

  sp <- "PICE.MAR"

  mc <- get_merch_criteria("AB", species = sp, verbose = FALSE)
  if (nrow(mc) == 0) {
    mc <- get_merch_criteria("AB", species = "ALL", verbose = FALSE)
  }
  skip_if(nrow(mc) == 0)

  mindbh <- mc$mindbh_cm[[1]]
  skip_if(!is.finite(mindbh))

  # choose DBH strictly below merchantability threshold
  dbh_small <- max(0.5, mindbh - 1)

  out <- vol_huang94(
    DBH = dbh_small,
    height = 15,
    species = sp,
    subregion = "Province"
  )

  # merchantable volume must be zero
  expect_equal(out$vol_merchantable[[1]], 0)

  # total volume must still be positive (regression target)
  expect_true(out$vol_total[[1]] > 0)
})

test_that("vol_kozak88_engine errors when merch criteria schema/values are invalid", {
  ns <- asNamespace("CanadaForestAllometry")

  mc_missing <- tibble::tibble(stumpht_m = 0.3, topdbh_cm = 10)
  mc_badvals <- tibble::tibble(stumpht_m = 0.3, topdbh_cm = 10, mindbh_cm = Inf)

  valid_p <- tibble::tibble(
    a0 = 1, a1 = 1, a2 = 1.01,
    b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, p = 0.225
  )

  expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak88_engine(
        DBH = 20, height = 20, species = "PICE.MAR", subregion = "Province",
        jurisdiction = "AB", model_id = "regional_huang94"
      ),
      get_merch_criteria = function(...) mc_missing,
      get_volume_params = function(...) valid_p,
      .package = "CanadaForestAllometry"
    ),
    "missing columns",
    fixed = FALSE
  )

  expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak88_engine(
        DBH = 20, height = 20, species = "PICE.MAR", subregion = "Province",
        jurisdiction = "AB", model_id = "regional_huang94"
      ),
      get_merch_criteria = function(...) mc_badvals,
      get_volume_params = function(...) valid_p,
      .package = "CanadaForestAllometry"
    ),
    "non-finite values",
    fixed = FALSE
  )
})

test_that("vol_kozak88_engine errors when returned parameter row is invalid", {
  ns <- asNamespace("CanadaForestAllometry")

  mc_ok <- tibble::tibble(stumpht_m = 0.3, topdbh_cm = 10, mindbh_cm = 5)
  p_bad <- tibble::tibble(
    a0 = 1, a1 = 1, a2 = 1.01,
    b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, p = 1
  )

  expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak88_engine(
        DBH = 20, height = 20, species = "PICE.MAR", subregion = "Province",
        jurisdiction = "AB", model_id = "regional_huang94"
      ),
      get_merch_criteria = function(...) mc_ok,
      get_volume_params = function(...) p_bad,
      .package = "CanadaForestAllometry"
    ),
    "missing required coefficients|contains NA/Inf",
    fixed = FALSE
  )
})

test_that("vol_kozak88_engine errors when merch criteria are unavailable for species and ALL", {
  ns <- asNamespace("CanadaForestAllometry")

  empty_mc <- tibble::tibble(
    jurisdiction = character(0),
    species = character(0),
    stumpht_m = numeric(0),
    topdbh_cm = numeric(0),
    mindbh_cm = numeric(0)
  )

  valid_p <- tibble::tibble(
    a0 = 1, a1 = 1, a2 = 1.01,
    b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, p = 0.225
  )

  expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak88_engine(
        DBH = 20, height = 20, species = "PICE.MAR", subregion = "Province",
        jurisdiction = "AB", model_id = "regional_huang94"
      ),
      get_merch_criteria = function(...) empty_mc,
      get_volume_params = function(...) valid_p,
      .package = "CanadaForestAllometry"
    ),
    "returned no rows",
    fixed = FALSE
  )
})

test_that("vol_kozak88_engine covers height clamp and bad stump DIB path", {
  ns <- asNamespace("CanadaForestAllometry")

  mc_ok <- tibble::tibble(stumpht_m = 0.3, topdbh_cm = 10, mindbh_cm = 5)

  # p omitted -> default p=0.225 path
  p_no_p <- tibble::tibble(
    a0 = 1, a1 = 1, a2 = 1.01,
    b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0
  )

  out <- testthat::with_mocked_bindings(
    ns$vol_kozak88_engine(
      DBH = 20, height = 1.2, species = "PICE.MAR", subregion = "Province",
      jurisdiction = "AB", model_id = "regional_huang94"
    ),
    get_merch_criteria = function(...) mc_ok,
    get_volume_params = function(...) p_no_p,
    .package = "CanadaForestAllometry"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)
  expect_true(is.finite(out$vol_total[[1]]))

  # Negative a0 keeps params finite but makes form factor invalid at stump DIB step
  p_bad_dib <- tibble::tibble(
    a0 = -1, a1 = 1, a2 = 1.01,
    b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, p = 0.225
  )

  expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak88_engine(
        DBH = 20, height = 20, species = "PICE.MAR", subregion = "Province",
        jurisdiction = "AB", model_id = "regional_huang94"
      ),
      get_merch_criteria = function(...) mc_ok,
      get_volume_params = function(...) p_bad_dib,
      .package = "CanadaForestAllometry"
    ),
    "Failed computing DIB at stump height",
    fixed = FALSE
  )
})
