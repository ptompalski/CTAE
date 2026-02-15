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

testthat::test_that("vol_kozak94: errors when multiple parameter rows are returned", {
  ns <- asNamespace("CanadaForestAllometry")

  mock_mc <- tibble::tibble(
    stumpht_m = 0.3,
    topdbh_cm = 10,
    mindbh_cm = 5
  )

  mock_p <- tibble::tibble(
    a0 = c(1, 1),
    a1 = c(1, 1),
    a2 = c(1.01, 1.01),
    b0 = c(0, 0),
    b1 = c(0, 0),
    b2 = c(0, 0),
    b3 = c(0, 0),
    b4 = c(0, 0),
    b5 = c(0, 0),
    b6 = c(0, 0),
    log_bias_factor = c(1, 1)
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak94(DBH = 20, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
      get_merch_criteria = function(...) mock_mc,
      get_volume_params = function(...) mock_p,
      .package = "CanadaForestAllometry"
    ),
    "Multiple Kozak94 parameter rows returned",
    fixed = FALSE
  )
})

testthat::test_that("vol_kozak94: errors on missing/non-finite/invalid parameters", {
  ns <- asNamespace("CanadaForestAllometry")

  mock_mc <- tibble::tibble(
    stumpht_m = 0.3,
    topdbh_cm = 10,
    mindbh_cm = 5
  )

  p_missing <- tibble::tibble(
    a0 = 1, a1 = 1, a2 = 1.01,
    b0 = 0, b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0,
    log_bias_factor = 1
  )
  p_nonfinite <- tibble::tibble(
    a0 = 1, a1 = 1, a2 = 1.01,
    b0 = 0, b1 = 0, b2 = 0, b3 = 0, b4 = NA_real_, b5 = 0, b6 = 0,
    log_bias_factor = 1
  )
  p_bias_bad <- tibble::tibble(
    a0 = 1, a1 = 1, a2 = 1.01,
    b0 = 0, b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0,
    log_bias_factor = 0
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak94(DBH = 20, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
      get_merch_criteria = function(...) mock_mc,
      get_volume_params = function(...) p_missing,
      .package = "CanadaForestAllometry"
    ),
    "Missing parameter columns",
    fixed = FALSE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak94(DBH = 20, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
      get_merch_criteria = function(...) mock_mc,
      get_volume_params = function(...) p_nonfinite,
      .package = "CanadaForestAllometry"
    ),
    "is not finite",
    fixed = FALSE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak94(DBH = 20, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
      get_merch_criteria = function(...) mock_mc,
      get_volume_params = function(...) p_bias_bad,
      .package = "CanadaForestAllometry"
    ),
    "log_bias_factor must be > 0",
    fixed = FALSE
  )
})

testthat::test_that("vol_kozak94: errors on missing/invalid merchantability criteria", {
  ns <- asNamespace("CanadaForestAllometry")

  p_ok <- tibble::tibble(
    a0 = 1, a1 = 1, a2 = 1.01,
    b0 = 0, b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0,
    log_bias_factor = 1
  )

  mc_missing_col <- tibble::tibble(stumpht_m = 0.3, topdbh_cm = 10)
  mc_empty <- tibble::tibble(
    jurisdiction = character(0),
    species = character(0),
    stumpht_m = numeric(0),
    topdbh_cm = numeric(0),
    mindbh_cm = numeric(0)
  )
  mc_bad_stump <- tibble::tibble(stumpht_m = -1, topdbh_cm = 10, mindbh_cm = 5)
  mc_bad_top <- tibble::tibble(stumpht_m = 0.3, topdbh_cm = 0, mindbh_cm = 5)
  mc_bad_mindbh <- tibble::tibble(stumpht_m = 0.3, topdbh_cm = 10, mindbh_cm = -1)

  testthat::expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak94(DBH = 20, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
      get_merch_criteria = function(...) mc_missing_col,
      get_volume_params = function(...) p_ok,
      .package = "CanadaForestAllometry"
    ),
    "get_merch_criteria\\(\\) missing columns",
    fixed = FALSE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak94(DBH = 20, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
      get_merch_criteria = function(...) mc_empty,
      get_volume_params = function(...) p_ok,
      .package = "CanadaForestAllometry"
    ),
    "No merchantability criteria found",
    fixed = FALSE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak94(DBH = 20, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
      get_merch_criteria = function(...) mc_bad_stump,
      get_volume_params = function(...) p_ok,
      .package = "CanadaForestAllometry"
    ),
    "Invalid stumpht_m",
    fixed = FALSE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak94(DBH = 20, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
      get_merch_criteria = function(...) mc_bad_top,
      get_volume_params = function(...) p_ok,
      .package = "CanadaForestAllometry"
    ),
    "Invalid topdbh_cm",
    fixed = FALSE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak94(DBH = 20, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
      get_merch_criteria = function(...) mc_bad_mindbh,
      get_volume_params = function(...) p_ok,
      .package = "CanadaForestAllometry"
    ),
    "Invalid mindbh_cm",
    fixed = FALSE
  )
})

testthat::test_that("vol_kozak94: errors on no params, bad form factor, bad DIB, and solver issues", {
  ns <- asNamespace("CanadaForestAllometry")

  mc_ok <- tibble::tibble(stumpht_m = 0.3, topdbh_cm = 10, mindbh_cm = 5)

  p_empty <- tibble::tibble(
    a0 = numeric(0), a1 = numeric(0), a2 = numeric(0),
    b0 = numeric(0), b1 = numeric(0), b2 = numeric(0), b3 = numeric(0),
    b4 = numeric(0), b5 = numeric(0), b6 = numeric(0),
    log_bias_factor = numeric(0)
  )
  p_ff_bad <- tibble::tibble(
    a0 = -1, a1 = 1, a2 = 1.01,
    b0 = 0, b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0,
    log_bias_factor = 1
  )
  p_dib_bad <- tibble::tibble(
    a0 = 1, a1 = 1, a2 = 1.01,
    b0 = -1e308, b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0,
    log_bias_factor = 1
  )
  p_solver_bad <- tibble::tibble(
    a0 = 1, a1 = 1, a2 = 1.01,
    b0 = 0, b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0,
    log_bias_factor = 1
  )
  mc_hm_bad <- tibble::tibble(stumpht_m = 30, topdbh_cm = 10, mindbh_cm = 5)

  testthat::expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak94(DBH = 20, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
      get_merch_criteria = function(...) mc_ok,
      get_volume_params = function(...) p_empty,
      .package = "CanadaForestAllometry"
    ),
    "No Kozak94 parameters found",
    fixed = FALSE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak94(DBH = 20, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
      get_merch_criteria = function(...) mc_ok,
      get_volume_params = function(...) p_ff_bad,
      .package = "CanadaForestAllometry"
    ),
    "Form factor is non-finite or <= 0",
    fixed = FALSE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak94(DBH = 20, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
      get_merch_criteria = function(...) mc_ok,
      get_volume_params = function(...) p_dib_bad,
      .package = "CanadaForestAllometry"
    ),
    "Failed computing DIB at 0.3 m",
    fixed = FALSE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak94(DBH = 20, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
      get_merch_criteria = function(...) mc_ok,
      get_volume_params = function(...) p_solver_bad,
      .package = "CanadaForestAllometry"
    ),
    "Merchantable height solver failed",
    fixed = FALSE
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      ns$vol_kozak94(DBH = 20, height = 20, species = "PSEU.MEN", BEC_zone = "CWH"),
      get_merch_criteria = function(...) mc_hm_bad,
      get_volume_params = function(...) p_dib_bad,
      .package = "CanadaForestAllometry"
    ),
    "Failed computing DIB at 0.3 m|merchantable height is invalid",
    fixed = FALSE
  )
})
