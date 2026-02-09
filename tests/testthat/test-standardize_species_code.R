# tests/testthat/test-standardize_species_code.R

test_that("standardize_species_code supports NFI, compact, SPP, and variety formats", {
  # NFI dotted (4.3)
  expect_equal(
    standardize_species_code("PICE.GLA", keep_all = FALSE),
    "PICE.GLA"
  )
  expect_equal(
    standardize_species_code(" pice.gla ", keep_all = FALSE),
    "PICE.GLA"
  )

  # Compact (7 letters -> 4.3)
  expect_equal(
    standardize_species_code("PICEGLA", keep_all = FALSE),
    "PICE.GLA"
  )
  expect_equal(
    standardize_species_code("picegla", keep_all = FALSE),
    "PICE.GLA"
  )

  # Genus-level (4.SPP)
  expect_equal(
    standardize_species_code("PICE.SPP", keep_all = FALSE),
    "PICE.SPP"
  )
  expect_equal(
    standardize_species_code(" pice.spp ", keep_all = FALSE),
    "PICE.SPP"
  )

  # Variety (4.3.3)
  expect_equal(
    standardize_species_code("PICE.MAR.AAA", keep_all = FALSE),
    "PICE.MAR.AAA"
  )
  expect_equal(
    standardize_species_code(" pice.mar.aaa ", keep_all = FALSE),
    "PICE.MAR.AAA"
  )

  # Compact variety (10 letters -> 4.3.3)
  expect_equal(
    standardize_species_code("PICEMARAAA", keep_all = FALSE),
    "PICE.MAR.AAA"
  )
  expect_equal(
    standardize_species_code("picemaraaa", keep_all = FALSE),
    "PICE.MAR.AAA"
  )

  # ALL handling (only if keep_all = TRUE)
  expect_equal(standardize_species_code("ALL", keep_all = TRUE), "ALL")
  expect_error(
    standardize_species_code("ALL", keep_all = FALSE),
    class = "ctae_invalid_species_code"
  )

  # Invalid codes
  expect_error(
    standardize_species_code("PICE.GL", keep_all = FALSE),
    class = "ctae_invalid_species_code"
  )
  expect_error(
    standardize_species_code("PICE.123", keep_all = FALSE),
    class = "ctae_invalid_species_code"
  )
  expect_error(
    standardize_species_code("NOT_A_SPECIES", keep_all = FALSE),
    class = "ctae_invalid_species_code"
  )
})
