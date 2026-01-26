testthat::test_that("vol_huang94 returns a tibble with expected columns and length", {
  # Mock merch criteria (AB is ALL, but we return what vol_huang94 needs)
  mock_get_merch_criteria <- function(jurisdiction, species = NULL) {
    testthat::expect_equal(jurisdiction, "AB")
    tibble::tibble(
      jurisdiction = "AB",
      species = "ALL",
      stumpht_m = 0.3,
      topdbh_cm = 7,
      mindbh_cm = 13
    )
  }

  # A simple, stable set of Huang params (long format)
  mk_params <- function(species, subregion) {
    tibble::tibble(
      Species = species,
      parameter = c("a0", "a1", "a2", "b1", "b2", "b3", "b4", "b5"),
      estimate = c(1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0),
      NaturalSubregionNum = NA_character_,
      NaturalSubregionCode = subregion,
      Subregion = subregion
    )
  }

  mock_get_volume_params <- function(model_id, species, subregion) {
    testthat::expect_equal(model_id, "regional_huang94")
    # Always return Province params here (for this test)
    testthat::expect_true(subregion %in% c("Province", "PROVINCE"))
    mk_params(species, "Province")
  }

  testthat::local_mocked_bindings(
    get_merch_criteria = mock_get_merch_criteria,
    get_volume_params = mock_get_volume_params,
    standardize_species_code = function(x) x,
    .package = "CTAE"
  )

  out <- CTAE::vol_huang94(
    DBH = c(20, 30),
    height = c(20, 25),
    species = c("PICE.MAR", "PICE.MAR"),
    subregion = "Province"
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, c("vol_total", "vol_merchantable"))
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_type(out$vol_total, "double")
  testthat::expect_type(out$vol_merchantable, "double")
  testthat::expect_true(all(is.finite(out$vol_total)))
  testthat::expect_true(all(is.finite(out$vol_merchantable)))
  testthat::expect_true(all(out$vol_total >= out$vol_merchantable))
})

testthat::test_that("vol_huang94 recycles scalars to common length", {
  mock_get_merch_criteria <- function(jurisdiction, species = NULL) {
    tibble::tibble(
      jurisdiction = "AB",
      species = "ALL",
      stumpht_m = 0.3,
      topdbh_cm = 7,
      mindbh_cm = 13
    )
  }

  mk_params <- function(species, subregion) {
    tibble::tibble(
      Species = species,
      parameter = c("a0", "a1", "a2", "b1", "b2", "b3", "b4", "b5"),
      estimate = c(1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0),
      NaturalSubregionNum = NA_character_,
      NaturalSubregionCode = subregion,
      Subregion = subregion
    )
  }

  mock_get_volume_params <- function(model_id, species, subregion) {
    mk_params(species, "Province")
  }

  testthat::local_mocked_bindings(
    get_merch_criteria = mock_get_merch_criteria,
    get_volume_params = mock_get_volume_params,
    standardize_species_code = function(x) x,
    .package = "CTAE"
  )

  out <- CTAE::vol_huang94(
    DBH = c(20, 25, 30),
    height = 20, # scalar
    species = "PICE.MAR", # scalar
    subregion = "Province" # scalar
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$vol_total)))
})

testthat::test_that("vol_huang94 returns zeros for DBH below mindbh", {
  mock_get_merch_criteria <- function(jurisdiction, species = NULL) {
    tibble::tibble(
      jurisdiction = "AB",
      species = "ALL",
      stumpht_m = 0.3,
      topdbh_cm = 7,
      mindbh_cm = 13
    )
  }

  # Should never be called for DBH < mindbh; but return something anyway
  mock_get_volume_params <- function(model_id, species, subregion) {
    tibble::tibble(
      Species = species,
      parameter = c("a0", "a1", "a2", "b1", "b2", "b3", "b4", "b5"),
      estimate = c(1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0),
      NaturalSubregionNum = NA_character_,
      NaturalSubregionCode = "Province",
      Subregion = "Province"
    )
  }

  testthat::local_mocked_bindings(
    get_merch_criteria = mock_get_merch_criteria,
    get_volume_params = mock_get_volume_params,
    standardize_species_code = function(x) x,
    .package = "CTAE"
  )

  out <- CTAE::vol_huang94(
    DBH = c(10, 12.9),
    height = c(20, 20),
    species = c("PICE.MAR", "PICE.MAR"),
    subregion = c("DMW", "Province")
  )

  testthat::expect_equal(out$vol_total, c(0, 0))
  testthat::expect_equal(out$vol_merchantable, c(0, 0))
})

testthat::test_that("vol_huang94 warns when subregion is missing and falls back to Province", {
  mock_get_merch_criteria <- function(jurisdiction, species = NULL) {
    tibble::tibble(
      jurisdiction = "AB",
      species = "ALL",
      stumpht_m = 0.3,
      topdbh_cm = 7,
      mindbh_cm = 13
    )
  }

  mk_params <- function(species, subregion) {
    tibble::tibble(
      Species = species,
      parameter = c("a0", "a1", "a2", "b1", "b2", "b3", "b4", "b5"),
      estimate = c(1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0),
      NaturalSubregionNum = NA_character_,
      NaturalSubregionCode = subregion,
      Subregion = subregion
    )
  }

  mock_get_volume_params <- function(model_id, species, subregion) {
    # Simulate: subregion has no params; Province does
    if (identical(subregion, "DMW")) {
      return(tibble::tibble())
    }
    mk_params(species, "Province")
  }

  testthat::local_mocked_bindings(
    get_merch_criteria = mock_get_merch_criteria,
    get_volume_params = mock_get_volume_params,
    standardize_species_code = function(x) x,
    .package = "CTAE"
  )

  testthat::expect_warning(
    CTAE::vol_huang94(
      DBH = 20,
      height = 20,
      species = "PICE.MAR",
      subregion = "DMW"
    ),
    regexp = "using Province-wide parameters",
    fixed = FALSE
  )
})

testthat::test_that("vol_huang94 does NOT warn when subregion is Province", {
  mock_get_merch_criteria <- function(jurisdiction, species = NULL) {
    tibble::tibble(
      jurisdiction = "AB",
      species = "ALL",
      stumpht_m = 0.3,
      topdbh_cm = 7,
      mindbh_cm = 13
    )
  }

  mk_params <- function(species, subregion) {
    tibble::tibble(
      Species = species,
      parameter = c("a0", "a1", "a2", "b1", "b2", "b3", "b4", "b5"),
      estimate = c(1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0),
      NaturalSubregionNum = NA_character_,
      NaturalSubregionCode = subregion,
      Subregion = subregion
    )
  }

  mock_get_volume_params <- function(model_id, species, subregion) {
    # Province params exist
    mk_params(species, "Province")
  }

  testthat::local_mocked_bindings(
    get_merch_criteria = mock_get_merch_criteria,
    get_volume_params = mock_get_volume_params,
    standardize_species_code = function(x) x,
    .package = "CTAE"
  )

  testthat::expect_no_warning(
    CTAE::vol_huang94(
      DBH = 20,
      height = 20,
      species = "PICE.MAR",
      subregion = "Province"
    )
  )
})

testthat::test_that("vol_huang94 maps UB to LBH when fetching parameters", {
  mock_get_merch_criteria <- function(jurisdiction, species = NULL) {
    tibble::tibble(
      jurisdiction = "AB",
      species = "ALL",
      stumpht_m = 0.3,
      topdbh_cm = 7,
      mindbh_cm = 13
    )
  }

  mk_params <- function(species, subregion) {
    tibble::tibble(
      Species = species,
      parameter = c("a0", "a1", "a2", "b1", "b2", "b3", "b4", "b5"),
      estimate = c(1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0),
      NaturalSubregionNum = NA_character_,
      NaturalSubregionCode = subregion,
      Subregion = subregion
    )
  }

  mock_get_volume_params <- function(model_id, species, subregion) {
    # If function mistakenly passes UB through, fail the test loudly
    if (identical(subregion, "UB")) {
      rlang::abort("UB should have been mapped to LBH before parameter lookup.")
    }
    if (identical(subregion, "LBH")) {
      return(mk_params(species, "LBH"))
    }
    # fallback
    mk_params(species, "Province")
  }

  testthat::local_mocked_bindings(
    get_merch_criteria = mock_get_merch_criteria,
    get_volume_params = mock_get_volume_params,
    standardize_species_code = function(x) x,
    .package = "CTAE"
  )

  out <- CTAE::vol_huang94(
    DBH = 20,
    height = 20,
    species = "PICE.MAR",
    subregion = "UB"
  )
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_true(is.finite(out$vol_total))
})

testthat::test_that("vol_huang94 errors on incompatible lengths (non-recyclable)", {
  testthat::local_mocked_bindings(
    get_merch_criteria = function(...) {
      tibble::tibble(
        jurisdiction = "AB",
        species = "ALL",
        stumpht_m = 0.3,
        topdbh_cm = 7,
        mindbh_cm = 13
      )
    },
    get_volume_params = function(...) tibble::tibble(),
    standardize_species_code = function(x) x,
    .package = "CTAE"
  )

  testthat::expect_error(
    CTAE::vol_huang94(
      DBH = c(20, 25),
      height = c(20, 20, 20),
      species = "PICE.MAR",
      subregion = "Province"
    ),
    regexp = "compatible lengths"
  )
})
