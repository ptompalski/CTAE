testthat::test_that("ytbh_nigh1998 predicts years-to-breast-height from site index", {
  out <- CanadaForestAllometry::ytbh_nigh1998(si = c(10, 15, 20))

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "ytbh")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$ytbh)))
  # matches eq. 5: YTBH = 446.6 * SI^-1.432
  testthat::expect_equal(out$ytbh, 446.6 * c(10, 15, 20)^(-1.432), tolerance = 1e-10)
})

testthat::test_that("ytbh_nigh1998 inverts to site index from ytbh (round-trip)", {
  si_in <- c(8, 12, 20)
  ytbh <- CanadaForestAllometry::ytbh_nigh1998(si = si_in)$ytbh
  out <- CanadaForestAllometry::ytbh_nigh1998(ytbh = ytbh)

  testthat::expect_named(out, "si")
  testthat::expect_equal(out$si, si_in, tolerance = 1e-10)
})

testthat::test_that("ytbh_nigh1998 requires exactly one of si or ytbh", {
  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh1998(),
    "exactly one",
    ignore.case = TRUE
  )
  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh1998(si = 10, ytbh = 5),
    "exactly one",
    ignore.case = TRUE
  )
})

testthat::test_that("ytbh_nigh1998 validates zero-length and non-positive inputs", {
  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh1998(si = numeric(0)),
    "length > 0",
    ignore.case = TRUE
  )
  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh1998(ytbh = numeric(0)),
    "length > 0",
    ignore.case = TRUE
  )
  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh1998(si = 0),
    "si.*values > 0",
    ignore.case = TRUE
  )
  testthat::expect_error(
    CanadaForestAllometry::ytbh_nigh1998(ytbh = -1),
    "ytbh.*values > 0",
    ignore.case = TRUE
  )
})
