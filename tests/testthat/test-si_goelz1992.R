# tests/testthat/test-si_goelz1992.R
# testthat 3e. Goelz & Burk (1992) base-age invariant jack pine SI model.
# Target 100% coverage of R/si_goelz1992.R.

test_that("si_goelz1992 returns a well-formed tibble (predict height)", {
  out <- si_goelz1992(age = c(20, 50, 80), si = c(16, 16, 16))
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "height")
  expect_equal(nrow(out), 3L)
})

test_that("si_goelz1992 returns a well-formed tibble (predict si)", {
  out <- si_goelz1992(age = c(20, 50, 80), height = c(8, 16, 20))
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "si")
  expect_equal(nrow(out), 3L)
})

test_that("si_goelz1992 recycles inputs to a common length", {
  out <- si_goelz1992(age = c(20, 50, 80), si = 16)
  expect_equal(nrow(out), 3L)
})

test_that("si_goelz1992 errors on incompatible input lengths", {
  expect_error(si_goelz1992(age = c(20, 50), si = c(16, 16, 16)))
})

test_that("si_goelz1992 requires exactly one of height / si", {
  expect_error(si_goelz1992(age = 50), "exactly one")
  expect_error(si_goelz1992(age = 50, height = 16, si = 16), "exactly one")
})

test_that("si_goelz1992 validates numeric inputs", {
  expect_error(si_goelz1992(age = -1, si = 16)) # age > 0
  expect_error(si_goelz1992(age = NA_real_, si = 16)) # finite
  expect_error(si_goelz1992(age = 50, si = 1.0)) # si > 1.3
  expect_error(si_goelz1992(age = 50, height = 1.0)) # height > 1.3
  expect_error(si_goelz1992(age = 50, si = Inf)) # finite
})

test_that("height equals site index at base age 50", {
  expect_equal(si_goelz1992(age = 50, si = 16)$height, 16)
  expect_equal(si_goelz1992(age = 50, height = 16)$si, 16)
})

# --- TIER 1 (fidelity): published reference values ---------------------------
# Fig. 1 caption (p. 5): a curve from SI = 16 at breast-height age 50 gives
# 8.83 m at age 20 and 19.82 m at age 80.
test_that("si_goelz1992 matches published Fig. 1 reference values (Goelz & Burk 1992)", {
  out <- si_goelz1992(age = c(20, 80), si = c(16, 16))
  # Published values are rounded to 2 dp; allow 0.02 m.
  expect_equal(out$height[[1]], 8.83, tolerance = 0.02)
  expect_equal(out$height[[2]], 19.82, tolerance = 0.02)
})

test_that("si_goelz1992 reproduces reference grid values (eq. 16)", {
  # A few points from tmp/si_goelz1992_comparison_values.csv, recomputed here
  # from eq. 16 with the Table 2 coefficients so the test needs no external file.
  ref <- expand.grid(si = c(11, 16, 20), age = c(20, 50, 80))
  b1 <- 0.0185
  b2 <- 1.3382
  b3 <- 0.4257
  b4 <- 1.0464
  expected <- with(ref, {
    base <- b1 * (si / 50)^b2 * 50^b3
    1.3 + (si - 1.3) * (1 - exp(-base * age))^b4 / (1 - exp(-base * 50))^b4
  })
  got <- si_goelz1992(age = ref$age, si = ref$si)$height
  expect_equal(got, expected, tolerance = 1e-10)
})

# --- Cross-check vs existing same-family model (always, even under Tier 1) ----
test_that("si_goelz1992 is consistent with si_carmean2001 (jack pine, ON)", {
  grid <- expand.grid(si = c(11, 14, 16, 20), age = c(20, 30, 50, 70, 80))
  new <- si_goelz1992(age = grid$age, si = grid$si)$height
  ref <- si_carmean2001(age = grid$age, si = grid$si)$height
  # Same species/region, different model forms: expect agreement within ~10%.
  expect_true(all(abs(new - ref) / ref < 0.10))
  # Height increases monotonically with age at fixed SI.
  ages <- sort(c(10, 20, 30, 50, 70, 80, 100))
  h <- si_goelz1992(age = ages, si = 16)$height
  expect_true(all(diff(h) > 0))
})
