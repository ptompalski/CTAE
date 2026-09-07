# testthat 3e tests for si_chenklinka2000()
# Fidelity tier: published site-index tables (Chen & Klinka 2000, pp. 3-4).

test_that("si_chenklinka2000 returns a well-formed tibble (predict height)", {
  out <- si_chenklinka2000(
    age = c(25, 50, 80),
    si = c(12, 16, 20),
    species = c("ABIE.LAS", "PICE.ENG", "PINU.CON")
  )
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
  expect_equal(nrow(out), 3)
  expect_true(all(is.finite(out$height)))
})

test_that("si_chenklinka2000 returns a well-formed tibble (predict si)", {
  out <- si_chenklinka2000(
    age = c(25, 50, 80),
    height = c(8, 16, 24),
    species = c("ABIE.LAS", "PICE.ENG", "PINU.CON")
  )
  expect_s3_class(out, "tbl_df")
  expect_named(out, "si")
  expect_equal(nrow(out), 3)
  expect_true(all(is.finite(out$si)))
})

test_that("si_chenklinka2000 recycles inputs to a common length", {
  out <- si_chenklinka2000(age = c(20, 40, 60), si = 16, species = "PICE.ENG")
  expect_equal(nrow(out), 3)
})

test_that("si_chenklinka2000 errors on incompatible input lengths", {
  expect_error(
    si_chenklinka2000(
      age = c(20, 40),
      si = c(12, 16, 20),
      species = "ABIE.LAS"
    )
  )
})

test_that("si_chenklinka2000 requires exactly one of height / si", {
  expect_error(
    si_chenklinka2000(age = 50, species = "ABIE.LAS"),
    "exactly one"
  )
  expect_error(
    si_chenklinka2000(age = 50, height = 16, si = 16, species = "ABIE.LAS"),
    "exactly one"
  )
})

test_that("si_chenklinka2000 validates numeric inputs", {
  expect_error(si_chenklinka2000(age = -5, si = 16, species = "ABIE.LAS"))
  expect_error(si_chenklinka2000(age = 50, si = -1, species = "ABIE.LAS"))
  expect_error(si_chenklinka2000(age = 50, height = 0, species = "ABIE.LAS"))
  expect_error(si_chenklinka2000(age = NA_real_, si = 16, species = "ABIE.LAS"))
})

test_that("si_chenklinka2000 errors on unsupported species", {
  expect_error(
    si_chenklinka2000(age = 50, si = 16, species = "PICE.MAR"),
    "No ChenKlinka2000 parameters"
  )
})

test_that("si_chenklinka2000 errors on empty input", {
  expect_error(
    si_chenklinka2000(
      age = numeric(0),
      si = numeric(0),
      species = character(0)
    ),
    "length"
  )
})

test_that("height equals site index at breast-height age 50 (conditioning)", {
  si_vals <- c(8, 12, 16, 20, 24)
  for (sp in c("ABIE.LAS", "PICE.ENG", "PINU.CON")) {
    h <- si_chenklinka2000(
      age = rep(50, length(si_vals)),
      si = si_vals,
      species = sp
    )$height
    expect_equal(h, si_vals, tolerance = 1e-6)
  }
})

test_that("predict_si inverts predict_height (round trip)", {
  ages <- c(20, 35, 70, 110)
  si_in <- c(10, 14, 18, 22)
  for (sp in c("ABIE.LAS", "PICE.ENG", "PINU.CON")) {
    h <- si_chenklinka2000(age = ages, si = si_in, species = sp)$height
    si_out <- si_chenklinka2000(age = ages, height = h, species = sp)$si
    expect_equal(si_out, si_in, tolerance = 1e-4)
  }
})

test_that("height increases monotonically with age (fixed si)", {
  ages <- seq(5, 150, by = 5)
  for (sp in c("ABIE.LAS", "PICE.ENG", "PINU.CON")) {
    h <- si_chenklinka2000(
      age = ages,
      si = rep(16, length(ages)),
      species = sp
    )$height
    expect_true(all(diff(h) > 0))
  }
})

# --- TIER 1 fidelity: published site-index tables --------------------------
test_that("si_chenklinka2000 matches published table values (Chen & Klinka 2000)", {
  ref <- readr::read_csv(
    testthat::test_path(
      "..",
      "..",
      "tmp",
      "si_chenklinka2000_published_table_values.csv"
    ),
    show_col_types = FALSE
  )
  pred <- si_chenklinka2000(
    age = ref$age_years,
    si = ref$si_m,
    species = ref$species
  )$height
  # Published table is rounded to 0.1 m; allow half a rounding unit.
  expect_true(max(abs(pred - ref$height_m)) <= 0.05)
})

# --- Cross-check vs. existing same-family model (sanity, not fidelity) ------
test_that("si_chenklinka2000 is consistent with si_thrower1994 for shared species", {
  ages <- c(30, 50, 80)
  for (sp in c("ABIE.LAS", "PINU.CON")) {
    ck <- si_chenklinka2000(age = ages, si = rep(18, 3), species = sp)$height
    th <- si_thrower1994(age = ages, si = rep(18, 3), species = sp)$height
    # Both are SI50 models: exact agreement at base age, close elsewhere.
    expect_equal(ck[2], 18, tolerance = 1e-6)
    expect_true(all(abs(ck - th) / th < 0.1))
  }
})

test_that("predict_height aborts for site index below the model floor", {
  # si < 1.3 makes (si - 1.3) <= 0, yielding a non-finite height.
  expect_error(
    si_chenklinka2000(age = 50, si = 1.0, species = "PICE.ENG"),
    "Non-finite height"
  )
})

test_that("solve_si aborts when a solution cannot be bracketed", {
  # Heights below the model floor (H >= 1.3 m) have no valid site index.
  expect_error(
    si_chenklinka2000(age = 50, height = 1.0, species = "ABIE.LAS"),
    "site-index solution"
  )
})
