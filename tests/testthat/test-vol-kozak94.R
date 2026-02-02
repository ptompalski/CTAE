testthat::test_that("vol_kozak94: basic output structure and types", {
  out <- vol_kozak94(
    DBH = 20,
    height = 20,
    species = "PSEU.MEN",
    BEC_zone = "CWH"
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), 1)

  # Always required
  testthat::expect_true(all(c("vol_total", "vol_merchantable") %in% names(out)))

  testthat::expect_true(is.numeric(out$vol_total))
  testthat::expect_true(is.numeric(out$vol_merchantable))

  testthat::expect_true(is.finite(out$vol_total))
  testthat::expect_true(is.finite(out$vol_merchantable))

  testthat::expect_gte(out$vol_total, 0)
  testthat::expect_gte(out$vol_merchantable, 0)

  # Merchantable should never exceed total
  testthat::expect_lte(out$vol_merchantable, out$vol_total)

  # Optional columns (only test if your function returns them)
  if (all(c("vol_stump", "vol_top") %in% names(out))) {
    testthat::expect_true(is.numeric(out$vol_stump))
    testthat::expect_true(is.numeric(out$vol_top))

    testthat::expect_true(is.finite(out$vol_stump))
    testthat::expect_true(is.finite(out$vol_top))

    testthat::expect_gte(out$vol_stump, 0)
    testthat::expect_gte(out$vol_top, 0)

    testthat::expect_equal(
      out$vol_total,
      out$vol_stump + out$vol_merchantable + out$vol_top,
      tolerance = 1e-10
    )
  }
})


testthat::test_that("vol_kozak94: input length checks", {
  testthat::expect_error(
    vol_kozak94(
      DBH = c(10, 20),
      height = 20,
      species = "PSEU.MEN",
      BEC_zone = "CWH"
    ),
    "must have the same length"
  )

  testthat::expect_error(
    vol_kozak94(
      DBH = 20,
      height = c(10, 20),
      species = "PSEU.MEN",
      BEC_zone = "CWH"
    ),
    "must have the same length"
  )

  testthat::expect_error(
    vol_kozak94(
      DBH = 20,
      height = 20,
      species = c("PSEU.MEN", "PSEU.MEN"),
      BEC_zone = "CWH"
    ),
    "must have the same length"
  )

  testthat::expect_error(
    vol_kozak94(
      DBH = 20,
      height = 20,
      species = "PSEU.MEN",
      BEC_zone = c("CWH", "CWH")
    ),
    "must have the same length"
  )
})


testthat::test_that("vol_kozak94: rejects invalid numeric inputs", {
  testthat::expect_error(
    vol_kozak94(
      DBH = NA_real_,
      height = 20,
      species = "PSEU.MEN",
      BEC_zone = "CWH"
    ),
    "finite"
  )

  testthat::expect_error(
    vol_kozak94(
      DBH = 20,
      height = NA_real_,
      species = "PSEU.MEN",
      BEC_zone = "CWH"
    ),
    "finite"
  )

  testthat::expect_error(
    vol_kozak94(DBH = -1, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
    "DBH must be > 0"
  )

  testthat::expect_error(
    vol_kozak94(DBH = 20, height = -1, species = "PSEU.MEN", BEC_zone = "CWH"),
    "height must be > 0"
  )
})


testthat::test_that("vol_kozak94: BEC zone is case/whitespace insensitive", {
  out1 <- vol_kozak94(
    DBH = 20,
    height = 20,
    species = "PSEU.MEN",
    BEC_zone = "CWH"
  )
  out2 <- vol_kozak94(
    DBH = 20,
    height = 20,
    species = "PSEU.MEN",
    BEC_zone = "  cwh  "
  )

  testthat::expect_equal(out1$vol_total, out2$vol_total, tolerance = 1e-12)
  testthat::expect_equal(
    out1$vol_merchantable,
    out2$vol_merchantable,
    tolerance = 1e-12
  )
})


testthat::test_that("vol_kozak94: total volume is calculated for small trees (dbh < mindbh)", {
  mc <- get_merch_criteria(
    "BC",
    species = "PSEU.MEN",
    BEC_zone = "CWH",
    verbose = FALSE
  ) |>
    dplyr::slice(1)

  testthat::expect_true(all(c("mindbh_cm") %in% names(mc)))
  mindbh <- mc$mindbh_cm[[1]]

  # choose DBH just below mindbh but still > 0
  dbh_small <- max(0.5, mindbh - 1)

  out <- vol_kozak94(
    DBH = dbh_small,
    height = 10,
    species = "PSEU.MEN",
    BEC_zone = "CWH"
  )

  # merchantable must be zero below mindbh
  testthat::expect_equal(out$vol_merchantable, 0)

  # but total should still be > 0 (regression target)
  testthat::expect_true(out$vol_total > 0)
})


testthat::test_that("vol_kozak94: merchantable volume is positive for a clearly merchantable tree (when feasible)", {
  mc <- get_merch_criteria(
    "BC",
    species = "PSEU.MEN",
    BEC_zone = "CWH",
    verbose = FALSE
  ) |>
    dplyr::slice(1)

  mindbh <- mc$mindbh_cm[[1]]
  topdbh <- mc$topdbh_cm[[1]]

  dbh_big <- max(mindbh, topdbh) + 10

  out <- vol_kozak94(
    DBH = dbh_big,
    height = 25,
    species = "PSEU.MEN",
    BEC_zone = "CWH"
  )

  testthat::expect_true(out$vol_merchantable > 0)
  testthat::expect_lte(out$vol_merchantable, out$vol_total)
})


testthat::test_that("vol_kozak94: vectorized inputs return same-length output and sane values", {
  DBH <- c(4, 6, 8, 12, 25)
  H <- c(6, 8, 10, 16, 28)

  out <- vol_kozak94(
    DBH = DBH,
    height = H,
    species = rep("PSEU.MEN", length(DBH)),
    BEC_zone = rep("CWH", length(DBH))
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), length(DBH))

  testthat::expect_true(all(is.finite(out$vol_total)))
  testthat::expect_true(all(is.finite(out$vol_merchantable)))

  testthat::expect_true(all(out$vol_total >= 0))
  testthat::expect_true(all(out$vol_merchantable >= 0))

  testthat::expect_true(all(out$vol_merchantable <= out$vol_total))

  # Optional: if stump/top are returned, also check total = parts
  if (all(c("vol_stump", "vol_top") %in% names(out))) {
    testthat::expect_true(all(is.finite(out$vol_stump)))
    testthat::expect_true(all(is.finite(out$vol_top)))
    testthat::expect_true(all(out$vol_stump >= 0))
    testthat::expect_true(all(out$vol_top >= 0))

    testthat::expect_equal(
      out$vol_total,
      out$vol_stump + out$vol_merchantable + out$vol_top,
      tolerance = 1e-10
    )
  }
})
