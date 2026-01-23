test_that("get_merch_criteria returns expected structure and types", {
  out <- get_merch_criteria("AB")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_named(
    out,
    c("jurisdiction", "species", "stumpht_m", "topdbh_cm", "mindbh_cm"),
    ignore.order = FALSE
  )

  expect_type(out$jurisdiction, "character")
  expect_type(out$species, "character")
  expect_type(out$stumpht_m, "double")
  expect_type(out$topdbh_cm, "double")
  expect_type(out$mindbh_cm, "double")
})

test_that("jurisdiction is standardized via standardize_jurisdiction_code()", {
  # This assumes your helper maps PEI -> PE (as discussed earlier)
  out <- get_merch_criteria("PEI")
  expect_identical(out$jurisdiction, "PE")
})

test_that("BC requires species", {
  expect_error(
    get_merch_criteria("BC"),
    "For jurisdiction 'BC', `species` must be provided\\.",
    fixed = FALSE
  )
  expect_error(
    get_merch_criteria("BC", NA_character_),
    "For jurisdiction 'BC', `species` must be provided\\.",
    fixed = FALSE
  )
  expect_error(
    get_merch_criteria("BC", ""),
    "For jurisdiction 'BC', `species` must be provided\\.",
    fixed = FALSE
  )
})

test_that("Species is standardized via standardize_species_code()", {
  # Only run this test if we can find a BC row and a species value to test against
  skip_if_not(
    exists("merchcrit", inherits = TRUE),
    "Internal dataset merchcrit not available"
  )

  if (!"Species" %in% names(merchcrit)) {
    skip("merchcrit has no Species column in this build")
  }

  bc_species <- merchcrit$Species[
    merchcrit$Province == "BC" & !is.na(merchcrit$Species)
  ]
  bc_species <- bc_species[bc_species != "ALL"]

  if (length(bc_species) == 0) {
    skip("No BC species-specific rows found in merchcrit")
  }

  # pick one known-good standardized code from the table, then create an "unstandardized" version
  sp_std <- bc_species[[1]]
  sp_unstd <- gsub("\\.", "", sp_std) # e.g., PICE.GLA -> PICEGLA

  out <- get_merch_criteria("BC", sp_unstd)
  expect_identical(out$species, CTAE:::standardize_species_code(sp_unstd))
})

test_that("Non-BC lookups are jurisdiction-only (species ignored)", {
  skip_if_not(
    exists("merchcrit", inherits = TRUE),
    "Internal dataset merchcrit not available"
  )

  provs <- unique(merchcrit$Province)
  provs <- setdiff(provs, "BC")
  if (length(provs) == 0) {
    skip("No non-BC provinces found in merchcrit")
  }

  prov <- provs[[1]]

  out1 <- get_merch_criteria(prov)
  out2 <- get_merch_criteria(prov, "PICEGLA") # should not change lookup for non-BC

  expect_identical(out1$jurisdiction, out2$jurisdiction)
  expect_equal(out1$stumpht_m, out2$stumpht_m)
  expect_equal(out1$topdbh_cm, out2$topdbh_cm)
  expect_equal(out1$mindbh_cm, out2$mindbh_cm)
})

test_that("BC uses species-specific row when present (and converts stump height to m)", {
  skip_if_not(
    exists("merchcrit", inherits = TRUE),
    "Internal dataset merchcrit not available"
  )
  if (!"Species" %in% names(merchcrit)) {
    skip("merchcrit has no Species column in this build")
  }

  # Find a BC species-specific row
  bc_rows <- merchcrit[
    merchcrit$Province == "BC" &
      !is.na(merchcrit$Species) &
      merchcrit$Species != "ALL",
    ,
    drop = FALSE
  ]
  if (nrow(bc_rows) == 0) {
    skip("No BC species-specific rows found in merchcrit")
  }

  r <- bc_rows[1, , drop = FALSE]
  sp <- r$Species

  out <- get_merch_criteria("BC", sp)

  expect_identical(out$jurisdiction, "BC")
  expect_identical(out$species, CTAE:::standardize_species_code(sp))

  # values should match the table (stump converted cm -> m)
  expect_equal(out$stumpht_m, as.numeric(r$StumpHT) / 100)
  expect_equal(out$topdbh_cm, as.numeric(r$TopDBH))
  expect_equal(out$mindbh_cm, as.numeric(r$MinDBH))
})

test_that("Unknown jurisdiction codes error (validated by standardize_jurisdiction_code)", {
  expect_error(
    get_merch_criteria("ZZ"),
    "Unknown province/territory codes found",
    fixed = FALSE
  )
})

test_that("Returns NA row when jurisdiction is valid but missing in merchcrit", {
  skip_if_not(
    exists("merchcrit", inherits = TRUE),
    "Internal dataset merchcrit not available"
  )

  # Candidate valid codes (from your internal prov_codes / helper)
  # We rely on the helper itself as the source of truth for what's "valid".
  candidates <- c(
    "AB",
    "BC",
    "MB",
    "NB",
    "NL",
    "NT",
    "NS",
    "NU",
    "ON",
    "PE",
    "QC",
    "SK",
    "YT"
  )

  # Keep only codes that are accepted by the standardizer
  valid <- candidates[
    !vapply(
      candidates,
      function(x) {
        inherits(
          try(CTAE:::standardize_jurisdiction_code(x), silent = TRUE),
          "try-error"
        )
      },
      logical(1)
    )
  ]

  if (length(valid) == 0) {
    skip("No valid jurisdiction codes available for this test")
  }

  # Find a valid code that has no row in merchcrit
  missing_in_merchcrit <- setdiff(valid, unique(merchcrit$Province))

  if (length(missing_in_merchcrit) == 0) {
    skip(
      "All valid jurisdictions have rows in merchcrit; cannot test NA return path"
    )
  }

  j <- missing_in_merchcrit[[1]]
  out <- get_merch_criteria(j)

  expect_equal(out$jurisdiction, j)
  expect_true(is.na(out$stumpht_m))
  expect_true(is.na(out$topdbh_cm))
  expect_true(is.na(out$mindbh_cm))
})
