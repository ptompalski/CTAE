# tests/testthat/test-si_goudie1984.R
# testthat 3e. Goudie (1984) lodgepole pine / white spruce height-age model.
#
# The implementation follows the NRCan SAS reference macros
# (%HT_Goudie_1984 / %SI_Goudie_1984), which are the designated authority. The
# Tier-1 fidelity guard below ports the exact SAS arithmetic in R (independent
# reference) and checks si_goudie1984() against it over a grid. The at-base-age
# identity (height == si at BHA 50) and the predict -> invert round-trip are
# also checked.

# --- SAS reference ports (independent of the package implementation) ---
.sas_goudie_ht <- function(b1, b2, b3, bha, si) {
  Tr <- 50
  x1 <- 1 + exp(b1 + b2 * log(Tr - 0.5) - b3 * log(si - 1.3))
  x2 <- 1 + exp(b1 + b2 * log(bha - 0.5) - b3 * log(si - 1.3))
  1.3 + (si - 1.3) * (x1 / x2)
}

.goudie_coef <- function(species) {
  switch(
    species,
    "PICE.GLA" = c(b1 = 9.794, b2 = -1.466, b3 = 1.287),
    "PINU.CON" = c(b1 = 7.815, b2 = -1.285, b3 = 1.007)
  )
}

test_that("si_goudie1984 returns a well-formed tibble (predict height)", {
  out <- si_goudie1984(
    age = c(25, 50, 80),
    si = c(12, 18, 24),
    species = "PINU.CON"
  )
  expect_s3_class(out, "tbl_df")
  expect_named(out, "height")
  expect_equal(nrow(out), 3)
})

test_that("si_goudie1984 returns a well-formed tibble (predict si)", {
  out <- si_goudie1984(
    age = c(25, 50, 80),
    height = c(8, 18, 26),
    species = "PICE.GLA"
  )
  expect_s3_class(out, "tbl_df")
  expect_named(out, "si")
  expect_equal(nrow(out), 3)
})

test_that("si_goudie1984 recycles inputs to a common length", {
  out <- si_goudie1984(age = 50, si = c(12, 18, 24), species = "PINU.CON")
  expect_equal(nrow(out), 3)
})

test_that("si_goudie1984 errors on incompatible input lengths", {
  expect_error(si_goudie1984(
    age = c(25, 50),
    si = c(12, 18, 24),
    species = "PINU.CON"
  ))
})

test_that("si_goudie1984 requires exactly one of height / si", {
  expect_error(si_goudie1984(age = 50, species = "PINU.CON"))
  expect_error(si_goudie1984(
    age = 50,
    height = 18,
    si = 18,
    species = "PINU.CON"
  ))
})

test_that("si_goudie1984 validates numeric inputs", {
  expect_error(si_goudie1984(age = -5, si = 18, species = "PINU.CON"))
  expect_error(si_goudie1984(age = 50, si = -2, species = "PINU.CON"))
  expect_error(si_goudie1984(age = 50, si = NA_real_, species = "PINU.CON"))
})

test_that("si_goudie1984 rejects unsupported species", {
  expect_error(si_goudie1984(age = 50, si = 18, species = "PSEU.MEN"))
})

test_that("si_goudie1984 aborts on non-finite predictions from degenerate inputs", {
  # si in (1.0, 1.3] makes log(si - 1.3) non-finite -> height prediction aborts.
  expect_error(
    suppressWarnings(si_goudie1984(age = 30, si = 1.2, species = "PINU.CON")),
    "Non-finite height"
  )
  # height at/below breast height (1.3 m) has no invertible site index ->
  # site-index prediction aborts.
  expect_error(
    si_goudie1984(age = 30, height = 1.1, species = "PICE.GLA"),
    "Non-finite site index"
  )
})

test_that(".goudie1984_prepare errors on zero-length input", {
  expect_error(
    CanadaForestAllometry:::.goudie1984_prepare(
      age = numeric(0),
      x = numeric(0),
      species = character(0),
      x_name = "si"
    ),
    "length > 0"
  )
})

# --- Fidelity guard 1: model is exact at the base age (height == si at BHA 50) ---
test_that("si_goudie1984 returns height == si at breast-height age 50", {
  for (sp in c("PINU.CON", "PICE.GLA")) {
    expect_equal(
      si_goudie1984(age = 50, si = 20, species = sp)$height,
      20,
      info = sp
    )
  }
})

# --- Fidelity guard 2 (Tier 1): matches SAS-reference height over a grid ---
test_that("si_goudie1984 matches the SAS reference height (Tier-1 fidelity)", {
  ref <- expand.grid(
    species = c("PINU.CON", "PICE.GLA"),
    age = c(10, 25, 50, 80, 120),
    si = c(6, 12, 18, 24),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  ref$height_sas <- mapply(
    function(sp, a, s) {
      b <- .goudie_coef(sp)
      .sas_goudie_ht(b["b1"], b["b2"], b["b3"], a, s)
    },
    ref$species,
    ref$age,
    ref$si
  )

  height_pkg <- mapply(
    function(sp, a, s) si_goudie1984(age = a, si = s, species = sp)$height,
    ref$species,
    ref$age,
    ref$si,
    USE.NAMES = FALSE
  )

  expect_equal(height_pkg, unname(ref$height_sas), tolerance = 1e-8)
})

# --- Fidelity guard 3: predict -> invert round-trip recovers site index ---
test_that("si_goudie1984 predict/invert round-trip recovers si", {
  for (sp in c("PINU.CON", "PICE.GLA")) {
    ages <- c(15, 30, 80, 120)
    sis <- c(8, 14, 20, 26)
    h <- si_goudie1984(age = ages, si = sis, species = sp)$height
    expect_equal(
      si_goudie1984(age = ages, height = h, species = sp)$si,
      sis,
      tolerance = 1e-6,
      info = sp
    )
  }
})

# --- Plausibility (sanity): height increases with age and with si ---
test_that("si_goudie1984 height is monotonic in age and in si (sanity check)", {
  by_age <- si_goudie1984(
    age = seq(10, 140, by = 10),
    si = 18,
    species = "PINU.CON"
  )$height
  expect_true(all(diff(by_age) > 0))

  by_si <- si_goudie1984(
    age = 60,
    si = seq(8, 28, by = 2),
    species = "PICE.GLA"
  )$height
  expect_true(all(diff(by_si) > 0))
})
