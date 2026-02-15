# tests/testthat/test-get_merch_criteria.R

testthat::test_that("get_merch_criteria: ON uses species/genus criteria when available; otherwise falls back to ALL", {
  testthat::skip_if_not(exists("merchcrit", inherits = TRUE))

  # Basic ON lookup (province-level default)
  out_on <- CanadaForestAllometry::get_merch_criteria("ON")
  testthat::expect_s3_class(out_on, "tbl_df")
  testthat::expect_equal(out_on$jurisdiction, "ON")
  testthat::expect_equal(out_on$species, "ALL")
  testthat::expect_true(is.finite(out_on$stumpht_m))
  testthat::expect_true(is.finite(out_on$topdbh_cm))
  testthat::expect_true(is.finite(out_on$mindbh_cm))

  # ON may provide species- or genus-level criteria (and may warn about fallback).
  # It should NOT depend on BC's BEC logic, but species can matter if present in the table.
  warns <- character(0)
  out_on2 <- withCallingHandlers(
    CanadaForestAllometry::get_merch_criteria(
      "ON",
      species = "PICE.GLA",
      BEC_zone = "CWH"
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  testthat::expect_equal(out_on2$jurisdiction, "ON")
  testthat::expect_true(is.finite(out_on2$stumpht_m))
  testthat::expect_true(is.finite(out_on2$topdbh_cm))
  testthat::expect_true(is.finite(out_on2$mindbh_cm))

  # Acceptable outcomes:
  # - exact species match (PICE.GLA)
  # - genus fallback (PICE.SPP)
  # - no species info in ON table -> ALL (same as out_on)
  testthat::expect_true(out_on2$species %in% c("PICE.GLA", "PICE.SPP", "ALL"))

  if (out_on2$species == "ALL") {
    testthat::expect_equal(out_on2, out_on)
  }

  # If a genus fallback happens, we expect a warning mentioning it (but don't force it
  # because ON tables may evolve and provide exact species rows).
  if (out_on2$species == "PICE.SPP") {
    testthat::expect_true(any(grepl("genus-level fallback|PICE\\.SPP", warns)))
  }

  # Alias standardization (PEI -> PE)
  out_pe <- CanadaForestAllometry::get_merch_criteria("PEI")
  testthat::expect_equal(out_pe$jurisdiction, "PE")
  testthat::expect_equal(out_pe$species, "ALL")
})

testthat::test_that("get_merch_criteria: BC allows missing species (falls back to ALL) and warns when verbose", {
  testthat::skip_if_not(exists("merchcrit", inherits = TRUE))

  # BC with missing species and missing BEC_zone emits TWO warnings:
  # - species not provided -> ALL
  # - BEC_zone missing -> UNKNOWN
  warns <- character(0)
  out <- withCallingHandlers(
    CanadaForestAllometry::get_merch_criteria("BC"),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  testthat::expect_equal(out$jurisdiction, "BC")
  testthat::expect_equal(out$species, "ALL")
  testthat::expect_true(is.finite(out$stumpht_m))
  testthat::expect_true(is.finite(out$topdbh_cm))
  testthat::expect_true(is.finite(out$mindbh_cm))

  testthat::expect_true(any(grepl(
    "species.*not provided|Species='ALL'",
    warns
  )))
  testthat::expect_true(any(grepl("BEC_zone missing|UNKNOWN", warns)))

  # If verbose=FALSE, no warning
  testthat::expect_silent(
    out2 <- CanadaForestAllometry::get_merch_criteria("BC", verbose = FALSE)
  )
  testthat::expect_equal(out2$jurisdiction, "BC")
  testthat::expect_equal(out2$species, "ALL")
})

testthat::test_that("get_merch_criteria: BC missing BEC_zone uses conservative UNKNOWN and warns (verbose=TRUE)", {
  testthat::skip_if_not(exists("merchcrit", inherits = TRUE))

  testthat::expect_warning(
    out <- CanadaForestAllometry::get_merch_criteria(
      "BC",
      species = "PSEU.MEN"
    ),
    "BEC_zone missing|UNKNOWN",
    fixed = FALSE
  )
  testthat::expect_equal(out$jurisdiction, "BC")
  testthat::expect_equal(out$species, "PSEU.MEN")

  # BEC missing but verbose=FALSE -> no warning
  testthat::expect_silent(
    out2 <- CanadaForestAllometry::get_merch_criteria(
      "BC",
      species = "PSEU.MEN",
      verbose = FALSE
    )
  )
  testthat::expect_equal(out2$jurisdiction, "BC")
  testthat::expect_equal(out2$species, "PSEU.MEN")
})

testthat::test_that("get_merch_criteria: BC unknown BEC_zone string falls back to UNKNOWN and warns (verbose=TRUE)", {
  testthat::skip_if_not(exists("merchcrit", inherits = TRUE))

  testthat::expect_warning(
    out <- CanadaForestAllometry::get_merch_criteria(
      "BC",
      species = "PSEU.MEN",
      BEC_zone = "NOT_A_BEC"
    ),
    "could not be mapped|UNKNOWN",
    fixed = FALSE
  )
  testthat::expect_equal(out$jurisdiction, "BC")
  testthat::expect_equal(out$species, "PSEU.MEN")
})

testthat::test_that("get_merch_criteria: BC BEC mapping routes through multiple groups and supports ALL fallback warning", {
  testthat::skip_if_not(exists("merchcrit", inherits = TRUE))

  # Coast_dry mapping branch (CDF)
  out_cdf <- CanadaForestAllometry::get_merch_criteria(
    "BC",
    species = "ALNU.RUB",
    BEC_zone = "CDF",
    verbose = FALSE
  )
  testthat::expect_equal(out_cdf$jurisdiction, "BC")
  testthat::expect_equal(out_cdf$species, "ALNU.RUB")

  # Interior_wet mapping branch (ICH)
  out_ich <- CanadaForestAllometry::get_merch_criteria(
    "BC",
    species = "PICE.SPP",
    BEC_zone = "ICH",
    verbose = FALSE
  )
  testthat::expect_equal(out_ich$jurisdiction, "BC")

  # High_elevation mapping branch (ESSF)
  out_essf <- CanadaForestAllometry::get_merch_criteria(
    "BC",
    species = "ABIE.LAS",
    BEC_zone = "ESSF",
    verbose = FALSE
  )
  testthat::expect_equal(out_essf$jurisdiction, "BC")

  # Force ALL fallback warning with a syntactically valid but unsupported species
  testthat::expect_warning(
    out_all <- CanadaForestAllometry::get_merch_criteria(
      "BC",
      species = "ZYZZ.ABC",
      BEC_zone = "CDF",
      verbose = TRUE
    ),
    "using BC fallback Species='ALL'",
    fixed = FALSE
  )
  testthat::expect_equal(out_all$jurisdiction, "BC")
  testthat::expect_equal(out_all$species, "ZYZZ.ABC")
})

testthat::test_that("get_merch_criteria: BC Interior_dry BEC mapping branch works (e.g., PP)", {
  testthat::skip_if_not(exists("merchcrit", inherits = TRUE))

  out_pp <- CanadaForestAllometry::get_merch_criteria(
    "BC",
    species = "PSEU.MEN",
    BEC_zone = "PP",
    verbose = FALSE
  )
  testthat::expect_equal(out_pp$jurisdiction, "BC")
  testthat::expect_equal(out_pp$species, "PSEU.MEN")
})

testthat::test_that("get_merch_criteria: errors when merchcrit is missing required columns", {
  bad_merchcrit <- tibble::tibble(
    Province = "ON",
    Species = "ALL",
    BEC_group = "UNKNOWN",
    StumpHT = 30,
    TopDBH = 9
  )

  testthat::with_mocked_bindings(
    testthat::expect_error(
      CanadaForestAllometry::get_merch_criteria("ON"),
      "missing required columns",
      ignore.case = TRUE
    ),
    .get_merchcrit_tbl = function() bad_merchcrit,
    .package = "CanadaForestAllometry"
  )
})

testthat::test_that("get_merch_criteria: non-BC throws if jurisdiction has no rows after legacy fallback", {
  empty_merchcrit <- tibble::tibble(
    Province = character(),
    Species = character(),
    BEC_group = character(),
    StumpHT = numeric(),
    TopDBH = numeric(),
    MinDBH = numeric()
  )

  testthat::with_mocked_bindings(
    testthat::expect_error(
      CanadaForestAllometry::get_merch_criteria("ON"),
      "No merchantability criteria found for jurisdiction",
      ignore.case = TRUE
    ),
    .get_merchcrit_tbl = function() empty_merchcrit,
    .package = "CanadaForestAllometry"
  )
})

testthat::test_that("get_merch_criteria: non-BC errors on ambiguous province-only fallback rows", {
  dup_merchcrit <- tibble::tibble(
    Province = c("ON", "ON"),
    Species = c("ABCD.SPP", "EFGH.SPP"),
    BEC_group = c("UNKNOWN", "UNKNOWN"),
    StumpHT = c(30, 35),
    TopDBH = c(9, 9),
    MinDBH = c(12, 12)
  )

  testthat::with_mocked_bindings(
    testthat::expect_error(
      CanadaForestAllometry::get_merch_criteria("ON"),
      "Multiple merchantability rows found for 'ON'",
      fixed = TRUE
    ),
    .get_merchcrit_tbl = function() dup_merchcrit,
    .package = "CanadaForestAllometry"
  )
})

testthat::test_that("get_merch_criteria: BC errors when no candidate row exists", {
  bc_unmatched <- tibble::tibble(
    Province = "BC",
    Species = "PICE.SPP",
    BEC_group = "Coast_wet",
    StumpHT = 30,
    TopDBH = 9,
    MinDBH = 12
  )

  testthat::with_mocked_bindings(
    testthat::expect_error(
      CanadaForestAllometry::get_merch_criteria(
        "BC",
        species = "ABIE.BAL",
        BEC_zone = "UNKNOWN_CODE",
        verbose = FALSE
      ),
      "No merchantability criteria found for BC",
      ignore.case = TRUE
    ),
    .get_merchcrit_tbl = function() bc_unmatched,
    .package = "CanadaForestAllometry"
  )
})

testthat::test_that("get_merch_criteria: BC errors on duplicate match rows", {
  bc_dups <- tibble::tibble(
    Province = c("BC", "BC"),
    Species = c("PSEU.MEN", "PSEU.MEN"),
    BEC_group = c("Coast_wet", "Coast_wet"),
    StumpHT = c(30, 30),
    TopDBH = c(9, 9),
    MinDBH = c(12, 12)
  )

  testthat::with_mocked_bindings(
    testthat::expect_error(
      CanadaForestAllometry::get_merch_criteria(
        "BC",
        species = "PSEU.MEN",
        BEC_zone = "CWH",
        verbose = FALSE
      ),
      "Multiple merchantability rows found for BC",
      ignore.case = TRUE
    ),
    .get_merchcrit_tbl = function() bc_dups,
    .package = "CanadaForestAllometry"
  )
})

# testthat::test_that("get_merch_criteria: BC species falls back to genus-level SPP when available", {
#   testthat::skip_if_not(exists("merchcrit", inherits = TRUE))

#   # Only run this test if there is a PICE.SPP row in merchcrit for Coast_wet or UNKNOWN
#   has_pice_spp <- any(
#     merchcrit$Province == "BC" &
#       merchcrit$Species == "PICE.SPP" &
#       merchcrit$BEC_group %in% c("Coast_wet", "UNKNOWN"),
#     na.rm = TRUE
#   )
#   testthat::skip_if_not(has_pice_spp)

#   testthat::expect_warning(
#     out <- CanadaForestAllometry::get_merch_criteria(
#       "BC",
#       species = "PICE.GLA",
#       BEC_zone = "CWH"
#     ),
#     "genus-level fallback|PICE\\.SPP",
#     fixed = FALSE
#   )
#   testthat::expect_equal(out$jurisdiction, "BC")
#   testthat::expect_equal(out$species, "PICE.GLA")
# })

# # internal
# .get_merchcrit_tbl <- function() {
#   # Prefer internal (if you ever move it to sysdata later)
#   ns <- asNamespace("CanadaForestAllometry")
#   if (exists("merchcrit", envir = ns, inherits = FALSE)) {
#     return(get("merchcrit", envir = ns, inherits = FALSE))
#   }

#   # External dataset in data/
#   e <- new.env(parent = emptyenv())
#   utils::data(list = "merchcrit", package = "CanadaForestAllometry", envir = e)
#   if (!exists("merchcrit", envir = e, inherits = FALSE)) {
#     rlang::abort("Dataset `merchcrit` not found in package data.")
#   }
#   get("merchcrit", envir = e, inherits = FALSE)
# }

# testthat::test_that("get_merch_criteria: fails loud if merchcrit lacks required BC ALL+UNKNOWN safety net", {
#   pkg <- "CanadaForestAllometry"

#   # Load the real table (so we can create a broken copy)
#   merchcrit_orig <- CanadaForestAllometry:::.get_merchcrit_tbl()

#   broken <- merchcrit_orig[
#     !(merchcrit_orig$Province == "BC" &
#         merchcrit_orig$Species == "ALL" &
#         merchcrit_orig$BEC_group == "UNKNOWN"),
#     ,
#     drop = FALSE
#   ]

#   # Mock the internal getter so get_merch_criteria() sees `broken`
#   testthat::local_mocked_bindings(
#     .get_merchcrit_tbl = function() broken,
#     .package = pkg
#   )

#   testthat::expect_error(
#     CanadaForestAllometry::get_merch_criteria("BC", verbose = FALSE),
#     "No merchantability criteria found for BC|\\('BC','ALL','UNKNOWN'\\)",
#     fixed = FALSE
#   )
# })
