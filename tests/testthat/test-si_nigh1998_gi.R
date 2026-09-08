testthat::test_that("si_nigh1998_gi predicts site index and returns single-column tibble", {
  out <- CanadaForestAllometry::si_nigh1998_gi(
    age = c(5, 20, 50),
    gi = c(30, 18, 12)
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, "si")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$si)))
  testthat::expect_true(all(out$si > 1.3))
})

testthat::test_that("si_nigh1998_gi uses exp(b1_log) transformation (eq. 4)", {
  ns <- asNamespace("CanadaForestAllometry")
  pars <- get("parameters_Nigh1998_gi", envir = ns, inherits = FALSE) |>
    dplyr::as_tibble() |>
    dplyr::filter(.data$bha == 20L) |>
    dplyr::slice(1)

  testthat::skip_if_not(nrow(pars) == 1)

  gi <- 18
  si_expected <- with(pars, 1.3 + exp(b1_log) * (gi^b2))

  out <- CanadaForestAllometry::si_nigh1998_gi(age = 20, gi = gi)
  testthat::expect_equal(out$si[[1]], si_expected, tolerance = 1e-10)
})

testthat::test_that("si_nigh1998_gi supports scalar recycling", {
  out <- CanadaForestAllometry::si_nigh1998_gi(age = c(5, 20, 50), gi = 15)
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(out, "si")
})

testthat::test_that("si_nigh1998_gi validates lengths, zero-length, and type", {
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998_gi(age = c(5, 10), gi = c(8, 9, 10)),
    "length 1 or",
    ignore.case = TRUE
  )
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998_gi(age = numeric(0), gi = numeric(0)),
    "length > 0",
    ignore.case = TRUE
  )
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998_gi(age = "20", gi = 10),
    "age.*numeric",
    ignore.case = TRUE
  )
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998_gi(age = 20, gi = "10"),
    "gi.*numeric",
    ignore.case = TRUE
  )
})

testthat::test_that("si_nigh1998_gi validates positive finite predictors", {
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998_gi(age = 20, gi = 0),
    "gi.*values > 0",
    ignore.case = TRUE
  )
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998_gi(age = 20, gi = NA_real_),
    "gi.*cannot contain NA",
    ignore.case = TRUE
  )
})

testthat::test_that("si_nigh1998_gi rejects ages not in the tabulated set", {
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998_gi(age = 15, gi = 10),
    "age.*must be one of",
    ignore.case = TRUE
  )
  testthat::expect_error(
    CanadaForestAllometry::si_nigh1998_gi(age = 60, gi = 10),
    "age.*must be one of",
    ignore.case = TRUE
  )
})
