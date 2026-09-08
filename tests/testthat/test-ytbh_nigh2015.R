testthat::test_that("ytbh_nigh2015 predicts years-to-breast-height", {
  out <- CanadaForestAllometry::ytbh_nigh2015(
    si = c(10, 15, 20)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "ytbh")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$ytbh)))
  testthat::expect_true(all(out$ytbh > 0))
})

testthat::test_that("ytbh_nigh2015 matches manual equation evaluation", {
  si <- c(5, 15, 24)
  y_expected <- 4.465 + 154.6 / si

  out <- CanadaForestAllometry::ytbh_nigh2015(
    si = si
  )

  testthat::expect_equal(out$ytbh, y_expected, tolerance = 1e-12)
})

testthat::test_that("ytbh_nigh2015 predictions are plausible against Figure 1", {
  # Figure 1 of Nigh (2015) anchors: si ~ 5 -> ytbh ~ 35; si ~ 24 -> ytbh ~ 11.
  out <- CanadaForestAllometry::ytbh_nigh2015(si = c(5, 24))

  testthat::expect_true(out$ytbh[[1]] > 30 && out$ytbh[[1]] < 40)
  testthat::expect_true(out$ytbh[[2]] > 9 && out$ytbh[[2]] < 13)
  # ytbh decreases monotonically with site index
  testthat::expect_true(out$ytbh[[1]] > out$ytbh[[2]])
})

testthat::test_that("ytbh_nigh2015 input validation is informative", {
  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh2015(
      si = c(10, 15),
      c(1, 2, 3)
    ),
    "unused argument",
    ignore.case = TRUE
  )
})

testthat::test_that("ytbh_nigh2015 supports scalar recycling", {
  out <- CanadaForestAllometry::ytbh_nigh2015(
    si = c(10, 15, 20)
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "ytbh")
  testthat::expect_true(all(is.finite(out$ytbh)))
})

testthat::test_that("ytbh_nigh2015 validates zero-length and type inputs", {
  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh2015(
      si = numeric(0)
    ),
    "length > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh2015(
      si = "10"
    ),
    "si.*numeric",
    ignore.case = TRUE
  )
})

testthat::test_that("ytbh_nigh2015 validates positive finite predictors", {
  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh2015(
      si = 0
    ),
    "si.*values > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh2015(
      si = NA_real_
    ),
    "si.*cannot contain NA",
    ignore.case = TRUE
  )
})
