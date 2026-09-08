# tests/testthat/test-si_nigh2009.R
# testthat 3e. Nigh, Thomas, Yearsley & Wang (2009) paper birch height-age model.
#
# Fidelity: Model 1 (base) reproduces the Saskatchewan SAS reference macros
# SI_Nigh_2009 / HT_Nigh_2009. The SAS math is re-derived independently below and
# used as a Tier-1 benchmark for Model 1. Models 2 and 3 have no published
# numeric benchmark table, so they rely on the exact-at-base-age identity, the
# predict->invert round-trip, and monotonicity (Tier-2 plausibility).

test_that("si_nigh2009 returns a well-formed tibble (predict height)", {
  out <- si_nigh2009(age = c(25, 50, 80), si = c(12, 18, 24))
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
  expect_equal(nrow(out), 3)
})

test_that("si_nigh2009 returns a well-formed tibble (predict si)", {
  out <- si_nigh2009(age = c(25, 50, 80), height = c(8, 18, 26))
  expect_s3_class(out, "tbl_df")
  expect_named(out, "si")
  expect_equal(nrow(out), 3)
})

test_that("si_nigh2009 recycles inputs to a common length", {
  out <- si_nigh2009(age = 50, si = c(12, 18, 24))
  expect_equal(nrow(out), 3)
})

test_that("si_nigh2009 errors on incompatible input lengths", {
  expect_error(si_nigh2009(age = c(25, 50), si = c(12, 18, 24)))
})

test_that("si_nigh2009 requires exactly one of height / si", {
  expect_error(si_nigh2009(age = 50))
  expect_error(si_nigh2009(age = 50, height = 18, si = 18))
})

test_that("si_nigh2009 validates numeric inputs", {
  expect_error(si_nigh2009(age = -5, si = 18))
  expect_error(si_nigh2009(age = 50, si = -2))
  expect_error(si_nigh2009(age = 50, si = NA_real_))
})

test_that("si_nigh2009 validates the model argument", {
  expect_error(si_nigh2009(age = 50, si = 18, model = 4))
  expect_error(si_nigh2009(age = 50, si = 18, model = c(1, 2)))
})

test_that("si_nigh2009 requires bec_zone for model 3 and rejects unknown zones", {
  expect_error(si_nigh2009(age = 50, si = 18, model = 3), "bec_zone")
  expect_error(si_nigh2009(age = 50, si = 18, model = 3, bec_zone = "XYZ"))
})

test_that("si_nigh2009 warns when bec_zone is supplied with model 1 or 2", {
  expect_warning(si_nigh2009(age = 50, si = 18, model = 1, bec_zone = "SBS"))
  expect_warning(si_nigh2009(age = 50, si = 18, model = 2, bec_zone = "SBS"))
})

test_that("si_nigh2009 aborts on non-finite predictions from degenerate inputs", {
  expect_error(
    suppressWarnings(si_nigh2009(age = 30, si = 0.5)),
    "Non-finite height"
  )
  expect_error(
    si_nigh2009(age = 30, height = 1.0),
    "Non-finite site index"
  )
})

# --- Tier 1 (fidelity): Model 1 matches the SAS reference implementation ---
# Independent re-derivation of the SAS HT_Nigh_2009 macro (b1=8.842, b2=-1.124,
# b3=-1.561, Tr=50), evaluated directly, must equal si_nigh2009(model = 1).
test_that("si_nigh2009 model 1 matches the SAS HT_Nigh_2009 reference", {
  sas_ht <- function(bha, si) {
    b1 <- 8.842
    b2 <- -1.124
    b3 <- -1.561
    tr <- 50
    x1 <- 1 + exp(b1 + b2 * log(tr - 0.5) + b3 * log(si - 1.3))
    x2 <- 1 + exp(b1 + b2 * log(bha - 0.5) + b3 * log(si - 1.3))
    1.3 + (si - 1.3) * (x1 / x2)
  }

  grid <- expand.grid(
    bha = c(5, 15, 25, 50, 80, 120),
    si = c(8, 14, 20, 26)
  )
  ref <- mapply(sas_ht, grid$bha, grid$si)
  got <- si_nigh2009(age = grid$bha, si = grid$si, model = 1)$height
  expect_equal(got, ref, tolerance = 1e-10)
})

# --- Tier 1: Model 1 SI direction matches the SAS SI_Nigh_2009 fixed point ---
test_that("si_nigh2009 model 1 SI direction matches the SAS SI_Nigh_2009 reference", {
  sas_si <- function(bha, height) {
    b1 <- 8.842
    b2 <- -1.124
    b3 <- -1.561
    tr <- 50
    si0 <- 20
    si1 <- Inf
    repeat {
      x1 <- 1 + exp(b1 + b2 * log(tr - 0.5) + b3 * log(si0 - 1.3))
      x2 <- 1 + exp(b1 + b2 * log(bha - 0.5) + b3 * log(si0 - 1.3))
      si1 <- 1.3 + (height - 1.3) * (x2 / x1)
      if (abs(si0 - si1) < 1e-8) break
      si0 <- (si0 + si1) / 2
    }
    si0
  }

  grid <- expand.grid(
    bha = c(15, 25, 80, 120),
    si = c(10, 16, 22, 28)
  )
  height <- mapply(
    function(a, s) si_nigh2009(age = a, si = s, model = 1)$height,
    grid$bha, grid$si
  )
  ref <- mapply(sas_si, grid$bha, height)
  got <- si_nigh2009(age = grid$bha, height = height, model = 1)$si
  expect_equal(got, ref, tolerance = 1e-6)
})

# --- Fidelity guard: model is exact at the base age (height == si at BHA 50) ---
test_that("si_nigh2009 returns height == si at breast-height age 50", {
  expect_equal(si_nigh2009(age = 50, si = 20, model = 1)$height, 20)
  expect_equal(si_nigh2009(age = 50, si = 20, model = 2)$height, 20)
  for (z in c("ICH", "IDF", "SBS")) {
    expect_equal(
      si_nigh2009(age = 50, si = 20, model = 3, bec_zone = z)$height,
      20,
      info = z
    )
  }
})

# --- Fidelity guard: predict -> invert round-trip recovers site index ---
test_that("si_nigh2009 predict/invert round-trip recovers si", {
  ages <- c(15, 30, 80, 120)
  sis <- c(10, 16, 22, 28)
  for (m in 1:2) {
    h <- si_nigh2009(age = ages, si = sis, model = m)$height
    expect_equal(
      si_nigh2009(age = ages, height = h, model = m)$si,
      sis,
      tolerance = 1e-6,
      info = paste("model", m)
    )
  }
  # zonal model round-trip
  h3 <- si_nigh2009(age = ages, si = sis, model = 3, bec_zone = "SBS")$height
  expect_equal(
    si_nigh2009(age = ages, height = h3, model = 3, bec_zone = "SBS")$si,
    sis,
    tolerance = 1e-6
  )
})

# --- Plausibility: SBS a1 differs from ICH/IDF; ICH and IDF are identical ---
test_that("si_nigh2009 model 3 zones behave as published (SBS != ICH == IDF)", {
  ich <- si_nigh2009(age = 30, si = 20, model = 3, bec_zone = "ICH")$height
  idf <- si_nigh2009(age = 30, si = 20, model = 3, bec_zone = "IDF")$height
  sbs <- si_nigh2009(age = 30, si = 20, model = 3, bec_zone = "SBS")$height
  expect_equal(ich, idf)
  expect_false(isTRUE(all.equal(ich, sbs)))
})

# --- Plausibility (sanity): height increases with age and with si ---
test_that("si_nigh2009 height is monotonic in age and in si (sanity check)", {
  by_age <- si_nigh2009(age = seq(10, 140, by = 10), si = 18)$height
  expect_true(all(diff(by_age) > 0))

  by_si <- si_nigh2009(age = 60, si = seq(8, 28, by = 2))$height
  expect_true(all(diff(by_si) > 0))
})
