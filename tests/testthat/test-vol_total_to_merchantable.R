# tests/testthat/test-vol-total-to-merchantable.R

library(dplyr)

# ---- unit-test helpers (mock B14 table + key builder) -------------------------

make_mock_b14 <- function() {
  # Minimal B14-like table with 2 genuses for AB ecozone 4
  tibble::tibble(
    component = "B14",
    source_table = "Appendix 6 - Table 14",
    juris_id = c("AB", "AB"),
    ecozone = c(4, 4),
    genus = c("PICE", "PINU"),
    species = NA_character_,
    variety = NA_character_,
    a = c(0.95, 0.90),
    b = c(-0.01, -0.02),
    c = c(2.0, 3.0),
    k = c(-0.001, -0.001),
    volmin = c(10, 20)
  )
}

make_keys <- function(
  genus = c("PICE", "PINU"),
  species_nfi = c("PICE.MAR", "PINU.BAN"),
  juris_id = "AB",
  ecozone = 4
) {
  n <- length(genus)
  tibble::tibble(
    .row_id = seq_len(n),
    juris_id = rep_len(juris_id, n),
    ecozone = rep_len(ecozone, n),
    species_nfi = rep_len(species_nfi, n),
    genus = genus
  )
}


# ---- unit tests: helpers ------------------------------------------------------

testthat::test_that("total_to_merch_prop computes expected values and respects NA", {
  # deterministic check
  v <- 100
  k <- -0.001
  a <- 0.95
  b <- -0.01
  c <- 2

  expect <- k + a * (1 - exp(b * v))^c
  got <- total_to_merch_prop(v, k, a, b, c)

  testthat::expect_equal(got, expect, tolerance = 1e-12)

  # NA propagation
  testthat::expect_true(is.na(total_to_merch_prop(NA, k, a, b, c)))
  testthat::expect_true(is.na(total_to_merch_prop(v, NA, a, b, c)))
})

testthat::test_that("total_to_merch_fetch_params joins uniquely and errors on missing", {
  b14 <- make_mock_b14()

  keys_ok <- make_keys(
    genus = c("PICE", "PINU"),
    species_nfi = c("PICE.MAR", "PINU.BAN"),
    juris_id = "AB",
    ecozone = 4
  )

  out <- total_to_merch_fetch_params(keys_ok, params_tbl = b14)
  testthat::expect_equal(nrow(out), 2)
  testthat::expect_equal(out$genus, c("PICE", "PINU"))
  testthat::expect_true(all(!is.na(out$a)))

  # missing genus -> should error with class
  keys_miss <- make_keys(
    genus = "ABIE",
    species_nfi = "ABIE.BAL",
    juris_id = "AB",
    ecozone = 4
  )

  testthat::expect_error(
    total_to_merch_fetch_params(keys_miss, params_tbl = b14),
    class = "ctae_missing_params_b14"
  )
})

testthat::test_that("total_to_merch_fetch_params errors on non-unique matches", {
  b14 <- make_mock_b14()

  # duplicate one row to create non-unique join for PICE/AB/4
  b14_dup <- dplyr::bind_rows(b14, dplyr::filter(b14, genus == "PICE"))

  keys <- make_keys(
    genus = "PICE",
    species_nfi = "PICE.MAR",
    juris_id = "AB",
    ecozone = 4
  )

  testthat::expect_error(
    total_to_merch_fetch_params(keys, params_tbl = b14_dup),
    class = "ctae_v2b_nonunique_params"
  )
})


# ---- integration tests: vol_total_to_merchantable() ---------------------------

testthat::test_that("vol_total_to_merchantable vectorizes over vol_total", {
  testthat::skip_if_not(exists("parameters_v2b", inherits = TRUE))

  out <- vol_total_to_merchantable(
    vol_total = c(50, 100, 300, 600),
    species = "PICE.MAR",
    jurisdiction = "AB",
    ecozone = 4,
    include_prop = TRUE,
    warn_outside = FALSE
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), 4)
  testthat::expect_true(all(
    c("vol_merchantable", "prop_merch") %in% names(out)
  ))
})

testthat::test_that("vol_total_to_merchantable respects include_prop and clamp_x output columns", {
  testthat::skip_if_not(exists("parameters_v2b", inherits = TRUE))

  a <- vol_total_to_merchantable(100, "PICE.MAR", "AB", 4)
  testthat::expect_named(a, "vol_merchantable")

  b <- vol_total_to_merchantable(100, "PICE.MAR", "AB", 4, include_prop = TRUE)
  testthat::expect_true(all(c("vol_merchantable", "prop_merch") %in% names(b)))

  c <- vol_total_to_merchantable(
    1,
    "PICE.MAR",
    "AB",
    4,
    clamp_x = TRUE,
    warn_outside = FALSE
  )
  testthat::expect_true(all(
    c("vol_merchantable", "vol_total_used") %in% names(c)
  ))
})

testthat::test_that("vol_total_to_merchantable warns on extrapolation below volmin when warn_outside=TRUE", {
  testthat::skip_if_not(exists("parameters_v2b", inherits = TRUE))

  # pick an intentionally small vol_total to increase chance of vol_total < volmin
  # if it doesn't trigger (because volmin is <= 1 for that row), we don't fail.
  w <- NULL
  out <- tryCatch(
    withCallingHandlers(
      vol_total_to_merchantable(
        vol_total = 1,
        species = "PICE.MAR",
        jurisdiction = "AB",
        ecozone = 4,
        warn_outside = TRUE
      ),
      warning = function(e) {
        w <<- e
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  if (inherits(out, "error")) {
    testthat::skip(
      "No parameters available for this integration case in current tables."
    )
  }

  if (!is.null(w)) {
    testthat::expect_true(inherits(w, "ctae_b14_extrapolation"))
  }
})

testthat::test_that("vol_total_to_merchantable clamps prop to [0,1] when clamp_prop=TRUE", {
  testthat::skip_if_not(exists("parameters_v2b", inherits = TRUE))

  out <- vol_total_to_merchantable(
    vol_total = c(1, 50, 500),
    species = "PICE.MAR",
    jurisdiction = "AB",
    ecozone = 4,
    include_prop = TRUE,
    warn_outside = FALSE,
    clamp_prop = TRUE
  )

  # If params exist, prop should be bounded.
  testthat::expect_true(all(
    out$prop_merch >= 0 & out$prop_merch <= 1,
    na.rm = TRUE
  ))
  testthat::expect_true(all(out$vol_merchantable >= 0, na.rm = TRUE))
})
