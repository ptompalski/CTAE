testthat::test_that("vol_honer83 returns expected structure and types", {
  out <- vol_honer83(
    DBH = c(22, 30, 18),
    height = c(18, 24, 15),
    species = c("PICE.GLA", "ABIE.BAL", "PINU.BAN")
  )

  testthat::expect_s3_class(out, c("tbl_df", "tbl", "data.frame"))
  testthat::expect_named(out, c("vol_total", "vol_merchantable"))
  testthat::expect_equal(nrow(out), 3)
  testthat::expect_type(out$vol_total, "double")
  testthat::expect_type(out$vol_merchantable, "double")
})

testthat::test_that("vol_honer83 is vectorized and recycles inputs", {
  out1 <- vol_honer83(
    DBH = c(20, 25, 30),
    height = c(18, 21, 24),
    species = "PICE.GLA"
  )
  testthat::expect_equal(nrow(out1), 3)

  out2 <- vol_honer83(
    DBH = 30,
    height = c(20, 22),
    species = c("PICE.GLA", "PICE.GLA")
  )
  testthat::expect_equal(nrow(out2), 2)
})

testthat::test_that("merchantable volume is non-negative and is zero below minimum DBH", {
  out <- vol_honer83(
    DBH = c(8.9, 9.0, 12),
    height = c(15, 15, 15),
    species = c("PICE.GLA", "PICE.GLA", "PICE.GLA")
  )

  # below threshold -> forced to 0
  testthat::expect_equal(out$vol_merchantable[1], 0)

  # threshold and above -> should be >= 0 (not necessarily > 0)
  testthat::expect_true(all(out$vol_merchantable[-1] >= 0, na.rm = TRUE))

  # total volume is also constrained to be non-negative
  testthat::expect_true(all(out$vol_total >= 0, na.rm = TRUE))
})

testthat::test_that("invalid DBH/height yield NA volumes (but do not error)", {
  out <- vol_honer83(
    DBH = c(30, NA, -5, 25),
    height = c(22, 20, 18, 0),
    species = c("PICE.GLA", "PICE.GLA", "PICE.GLA", "PICE.GLA")
  )

  # valid row (1) should be finite
  testthat::expect_true(is.finite(out$vol_total[1]))
  testthat::expect_true(is.finite(out$vol_merchantable[1]))

  # NA / negative / zero height should propagate to NA (by current implementation)
  testthat::expect_true(is.na(out$vol_total[2]))
  testthat::expect_true(is.na(out$vol_total[3]))
  testthat::expect_true(is.na(out$vol_total[4]))
})

testthat::test_that("species codes are standardized internally", {
  # This assumes standardize_species_code normalizes common variants.
  # Example: lowercase / whitespace / hyphenation. Adjust to match your helper’s behavior.
  out_a <- vol_honer83(DBH = 30, height = 22, species = "PICE.GLA")
  out_b <- vol_honer83(DBH = 30, height = 22, species = "pice.gla")

  testthat::expect_equal(out_a$vol_total, out_b$vol_total, tolerance = 1e-12)
  testthat::expect_equal(
    out_a$vol_merchantable,
    out_b$vol_merchantable,
    tolerance = 1e-12
  )
})

testthat::test_that("works in tidyverse pipe with list-column + unnest", {
  skip_if_not_installed <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      testthat::skip(paste0("Package '", pkg, "' not installed"))
    }
  }
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("tibble")

  trees <- tibble::tibble(
    tree_id = 1:4,
    dbh = c(22, 30, 18, 35),
    ht = c(18, 24, 15, 27),
    species = c("PICE.GLA", "PICE.GLA", "ABIE.BAL", "PINU.BAN")
  )

  out <- trees %>%
    dplyr::mutate(vol = vol_honer83(dbh, ht, species)) %>%
    tidyr::unnest(vol)

  testthat::expect_equal(nrow(out), 4)
  testthat::expect_true(all(c("vol_total", "vol_merchantable") %in% names(out)))
})
testthat::test_that("total volume is calculated for small trees (below minimum DBH)", {
  out <- vol_honer83(
    DBH = c(2, 4, 6, 8),
    height = c(4, 6, 8, 10),
    species = rep("PICE.GLA", 4)
  )

  # merchantable should be zero for these small DBHs (given typical thresholds)
  testthat::expect_true(all(out$vol_merchantable == 0, na.rm = TRUE))

  # total volume should still be computed for valid positive DBH/height
  testthat::expect_true(all(out$vol_total > 0, na.rm = TRUE))
})
