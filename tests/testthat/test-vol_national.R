test_that("vol_national_dbh returns correct structure", {
  out <- vol_national_dbh(DBH = 20, species = "PICE.GLA", jurisdiction = "AB")

  expect_s3_class(out, "tbl_df") # tibble
  expect_equal(nrow(out), 1)
  expect_named(out, c("vol_merchantable", "vol_total"))
  expect_true(all(vapply(out, is.numeric, logical(1))))
})

test_that("vol_national_dbh is vectorized over DBH", {
  DBH <- c(15, 25, 35)
  out <- vol_national_dbh(DBH = DBH, species = "PICE.GLA", jurisdiction = "AB")

  expect_equal(nrow(out), length(DBH))
  expect_true(all(out$vol_total >= 0, na.rm = TRUE))
  expect_true(all(out$vol_merchantable >= 0, na.rm = TRUE))
})

test_that("merchantable volume is never greater than total volume", {
  out <- vol_national_dbh(
    DBH = c(10, 20, 30),
    species = "PICE.GLA",
    jurisdiction = "AB"
  )
  expect_true(all(out$vol_merchantable <= out$vol_total, na.rm = TRUE))
})

test_that("species not in params returns NA rows", {
  out <- vol_national_dbh(
    DBH = c(10, 20),
    species = "NOT.A.SPP",
    jurisdiction = "AB"
  )
  expect_true(all(is.na(out$vol_total)))
  expect_true(all(is.na(out$vol_merchantable)))
})

test_that("NA DBH propagates to NA volumes", {
  out <- vol_national_dbh(
    DBH = c(20, NA),
    species = "PICE.GLA",
    jurisdiction = "AB"
  )
  expect_true(is.finite(out$vol_total[1]))
  expect_true(is.na(out$vol_total[2]))
  expect_true(is.na(out$vol_merchantable[2]))
})

test_that("vol_national_dbh_ht handles vector H and recycled H", {
  out1 <- vol_national_dbh_ht(
    DBH = c(20, 25),
    H = c(18, 22),
    species = "PICE.GLA",
    jurisdiction = "AB"
  )
  out2 <- vol_national_dbh_ht(
    DBH = c(20, 25),
    H = 20,
    species = "PICE.GLA",
    jurisdiction = "AB"
  )

  expect_equal(nrow(out1), 2)
  expect_equal(nrow(out2), 2)
  expect_named(out1, c("vol_merchantable", "vol_total"))
  expect_true(all(out1$vol_merchantable <= out1$vol_total, na.rm = TRUE))
})

test_that("vol_national_dbh_ht errors on wrong H length", {
  expect_error(
    vol_national_dbh_ht(
      DBH = c(20, 25),
      H = c(18, 22, 24),
      species = "PICE.GLA",
      jurisdiction = "AB"
    ),
    "length"
  )
})

test_that("dbh+H volumes are close to dbh-only volumes when H follows the internal height curve", {
  DBH <- c(15, 25, 35)

  p <- CTAE::parameters_NationalTaperModelsDBH
  pidx <- match("PICE.GLA", p$Species)
  skip_if(is.na(pidx), "PICE.GLA parameters not available in test environment")

  H <- p$b0[pidx] * DBH^p$b1[pidx]

  out_dbh <- vol_national_dbh(
    DBH = DBH,
    species = "PICE.GLA",
    jurisdiction = "AB"
  )
  out_dbh_ht <- vol_national_dbh_ht(
    DBH = DBH,
    H = H,
    species = "PICE.GLA",
    jurisdiction = "AB"
  )

  # relative differences should be small (choose a defensible threshold)
  rel_diff_total <- abs(out_dbh_ht$vol_total - out_dbh$vol_total) /
    pmax(out_dbh$vol_total, 1e-12)
  rel_diff_merch <- abs(
    out_dbh_ht$vol_merchantable - out_dbh$vol_merchantable
  ) /
    pmax(out_dbh$vol_merchantable, 1e-12)

  expect_true(all(rel_diff_total < 0.05, na.rm = TRUE))
  expect_true(all(rel_diff_merch < 0.05, na.rm = TRUE))

  # also ensure they rank the same (monotonic increasing with DBH)
  expect_true(is.unsorted(out_dbh$vol_total, strictly = FALSE) == FALSE)
  expect_true(is.unsorted(out_dbh_ht$vol_total, strictly = FALSE) == FALSE)
})
