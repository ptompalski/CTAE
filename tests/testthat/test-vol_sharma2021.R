# tests/testthat/test-vol_sharma2021.R

testthat::test_that("vol_sharma2021: returns tibble with correct columns and length", {
  # Basic single-tree call
  out1 <- CTAE::vol_sharma2021(
    DBH = 30,
    height = 22,
    species = "PICE.MAR"
  )

  testthat::expect_s3_class(out1, "tbl_df")
  testthat::expect_named(out1, c("vol_total", "vol_merchantable"))
  testthat::expect_equal(nrow(out1), 1L)

  # Vectorized call
  out2 <- CTAE::vol_sharma2021(
    DBH = c(25, 32, 18),
    height = c(20, 24, 15),
    species = c("PICE.GLA", "ABIE.BAL", "PINU.BAN")
  )

  testthat::expect_s3_class(out2, "tbl_df")
  testthat::expect_named(out2, c("vol_total", "vol_merchantable"))
  testthat::expect_equal(nrow(out2), 3L)
})

testthat::test_that("vol_sharma2021: recycling works like base rep(length.out = n)", {
  out <- CTAE::vol_sharma2021(
    DBH = c(20, 30),
    height = 22,
    species = "PICE.MAR"
  )

  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true(all(is.finite(out$vol_total)))
  testthat::expect_true(all(is.finite(out$vol_merchantable)))
})

testthat::test_that("vol_sharma2021: invalid geometry returns NA (non-positive or non-finite inputs)", {
  out <- CTAE::vol_sharma2021(
    DBH = c(30, 0, -5, NA, 25),
    height = c(22, 22, 22, 22, Inf),
    species = rep("PICE.MAR", 5)
  )

  # Row 1 valid
  testthat::expect_true(is.finite(out$vol_total[1]))
  testthat::expect_true(is.finite(out$vol_merchantable[1]))

  # Others invalid -> NA
  testthat::expect_true(is.na(out$vol_total[2]))
  testthat::expect_true(is.na(out$vol_merchantable[2]))
  testthat::expect_true(is.na(out$vol_total[3]))
  testthat::expect_true(is.na(out$vol_merchantable[3]))
  testthat::expect_true(is.na(out$vol_total[4]))
  testthat::expect_true(is.na(out$vol_merchantable[4]))
  testthat::expect_true(is.na(out$vol_total[5]))
  testthat::expect_true(is.na(out$vol_merchantable[5]))
})

testthat::test_that("vol_sharma2021: merchantable and total are non-negative", {
  out <- CTAE::vol_sharma2021(
    DBH = c(5, 10, 20, 40),
    height = c(8, 10, 18, 28),
    species = c("PICE.MAR", "PICE.MAR", "PICE.MAR", "PICE.MAR")
  )

  testthat::expect_true(all(is.na(out$vol_total) | out$vol_total >= 0))
  testthat::expect_true(all(
    is.na(out$vol_merchantable) | out$vol_merchantable >= 0
  ))
})

testthat::test_that("vol_sharma2021: merchantable volume does not exceed total volume", {
  out <- CTAE::vol_sharma2021(
    DBH = c(15, 25, 35),
    height = c(14, 20, 26),
    species = c("PICE.MAR", "ABIE.BAL", "PINU.BAN")
  )

  ok <- is.finite(out$vol_total) & is.finite(out$vol_merchantable)
  testthat::expect_true(all(out$vol_merchantable[ok] <= out$vol_total[ok]))
})

testthat::test_that("vol_sharma2021: unknown species produces an informative error", {
  testthat::expect_error(
    CTAE::vol_sharma2021(
      DBH = 30,
      height = 22,
      species = "NOT.A.REAL.SPECIES"
    ),
    regexp = "missing parameter|no parameters|regional_sharma2021|species",
    ignore.case = TRUE
  )
})
testthat::test_that("vol_sharma2021: total volume is calculated for small trees (below minimum DBH)", {
  out <- CTAE::vol_sharma2021(
    DBH = c(2, 4, 6, 8),
    height = c(4, 6, 8, 10),
    species = rep("PICE.MAR", 4)
  )

  # Total volume should still be computed (positive) for valid DBH/height
  testthat::expect_true(all(out$vol_total > 0, na.rm = TRUE))
})
