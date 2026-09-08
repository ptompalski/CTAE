# tests/testthat/test-si_nigh2002.R
# testthat 3e. Nigh, Krestov & Klinka (2002) trembling aspen height-age model.
#
# NOTE: the source prints no worked example or reference table, so there is no
# Tier-1 source-fidelity test. The strongest fidelity guards available from the
# published equation are the exact-at-base-age identity (height == si at BHA 50)
# and the predict->invert round-trip; both are checked below. The comparison-CSV
# test is a regression guard on this implementation, not a source benchmark.

test_that("si_nigh2002 returns a well-formed tibble (predict height)", {
  out <- si_nigh2002(age = c(25, 50, 80), si = c(12, 18, 24))
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
  expect_equal(nrow(out), 3)
})

test_that("si_nigh2002 returns a well-formed tibble (predict si)", {
  out <- si_nigh2002(age = c(25, 50, 80), height = c(8, 18, 26))
  expect_s3_class(out, "tbl_df")
  expect_named(out, "si")
  expect_equal(nrow(out), 3)
})

test_that("si_nigh2002 recycles inputs to a common length", {
  out <- si_nigh2002(age = 50, si = c(12, 18, 24))
  expect_equal(nrow(out), 3)
})

test_that("si_nigh2002 errors on incompatible input lengths", {
  expect_error(si_nigh2002(age = c(25, 50), si = c(12, 18, 24)))
})

test_that("si_nigh2002 requires exactly one of height / si", {
  expect_error(si_nigh2002(age = 50))
  expect_error(si_nigh2002(age = 50, height = 18, si = 18))
})

test_that("si_nigh2002 validates numeric inputs", {
  expect_error(si_nigh2002(age = -5, si = 18))
  expect_error(si_nigh2002(age = 50, si = -2))
  expect_error(si_nigh2002(age = 50, si = NA_real_))
})

test_that("si_nigh2002 rejects unknown bec_zone", {
  expect_error(si_nigh2002(age = 50, si = 18, bec_zone = "XYZ"))
})

test_that("si_nigh2002 aborts on non-finite predictions from degenerate inputs", {
  # si in (0, 1.3] makes log(si - 1.3) non-finite -> height prediction aborts.
  expect_error(
    suppressWarnings(si_nigh2002(age = 30, si = 0.5)),
    "Non-finite height"
  )
  # height at/below breast height (1.3 m) has no invertible site index ->
  # site-index prediction aborts.
  expect_error(
    si_nigh2002(age = 30, height = 1.0),
    "Non-finite site index"
  )
})

# --- Fidelity guard 1: model is exact at the base age (height == si at BHA 50) ---
test_that("si_nigh2002 returns height == si at breast-height age 50", {
  # base model
  expect_equal(si_nigh2002(age = 50, si = 20)$height, 20)
  # every extended zone
  for (z in c("BWBS", "ICH", "IDF", "MS", "SBPS", "SBS")) {
    expect_equal(
      si_nigh2002(age = 50, si = 20, bec_zone = z)$height,
      20,
      info = z
    )
  }
})

# --- Fidelity guard 2: predict -> invert round-trip recovers site index ---
test_that("si_nigh2002 predict/invert round-trip recovers si", {
  ages <- c(15, 30, 80, 120)
  sis <- c(10, 16, 22, 28)
  h <- si_nigh2002(age = ages, si = sis)$height
  expect_equal(si_nigh2002(age = ages, height = h)$si, sis, tolerance = 1e-6)

  # extended model round-trip
  h2 <- si_nigh2002(age = ages, si = sis, bec_zone = "BWBS")$height
  expect_equal(
    si_nigh2002(age = ages, height = h2, bec_zone = "BWBS")$si,
    sis,
    tolerance = 1e-6
  )
})

# --- Plausibility (sanity) check: height increases with age and with si ---
test_that("si_nigh2002 height is monotonic in age and in si (sanity check)", {
  by_age <- si_nigh2002(age = seq(10, 140, by = 10), si = 18)$height
  expect_true(all(diff(by_age) > 0))

  by_si <- si_nigh2002(age = 60, si = seq(8, 28, by = 2))$height
  expect_true(all(diff(by_si) > 0))
})

# --- Regression / self-consistency guard over an input grid ---
# The committed comparison-value generator
# (tmp/generate_si_nigh2002_comparison_values.R) writes a CSV under tmp/, but
# tmp/ is .Rbuildignore'd and absent from installed-package coverage/check runs.
# The reference grid is therefore inlined here so the test is portable: for every
# (zone, age, si) it confirms predict -> invert recovers the original si.
test_that("si_nigh2002 round-trips si -> height -> si over a grid", {
  ref <- expand.grid(
    bec_zone = c(NA, "BWBS", "ICH", "IDF", "MS", "SBPS", "SBS"),
    age = c(10, 25, 50, 80, 120),
    si = c(8, 14, 20, 26),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  height <- mapply(
    function(z, a, s) {
      args <- list(age = a, si = s)
      if (!is.na(z)) {
        args$bec_zone <- z
      }
      do.call(si_nigh2002, args)$height
    },
    ref$bec_zone,
    ref$age,
    ref$si
  )
  expect_true(all(is.finite(height)))

  si_rec <- mapply(
    function(z, a, h) {
      args <- list(age = a, height = h)
      if (!is.na(z)) {
        args$bec_zone <- z
      }
      do.call(si_nigh2002, args)$si
    },
    ref$bec_zone,
    ref$age,
    height,
    USE.NAMES = FALSE
  )
  expect_equal(si_rec, ref$si, tolerance = 1e-4)
})
