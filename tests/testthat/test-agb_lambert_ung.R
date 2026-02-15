# tests/testthat/test-agb_lambert_ung.R

lu_ns <- function() {
  asNamespace("CanadaForestAllometry")
}

lu_params <- function() {
  lu_ns()$.get_internal_data("parameters_LambertUng") |>
    tibble::as_tibble()
}

lu_coefs <- function(species, model) {
  p <- lu_params() |>
    dplyr::filter(.data$species == .env$species, .data$model == .env$model)

  testthat::skip_if(
    nrow(p) == 0,
    paste0("No Lambert/Ung coefficients found for ", species, " ", model)
  )

  stats::setNames(as.numeric(p$estimate), p$parameter)
}


test_that("agb_lambert_ung returns expected columns and row counts", {
  ns <- lu_ns()

  x <- ns$agb_lambert_ung(DBH = 20, species = "PICE.MAR", equation_set = "dbh")

  expect_s3_class(x, "tbl_df")
  expect_equal(nrow(x), 1L)
  expect_named(
    x,
    c("Bwood", "Bbark", "Bstem", "Bfoliage", "Bbranches", "Bcrown", "Btotal")
  )
})


test_that("keep_model_id adds model_id as last column and does not include species", {
  ns <- lu_ns()

  x <- ns$agb_lambert_ung(DBH = 20, species = "PICE.MAR", keep_model_id = TRUE)

  expect_s3_class(x, "tbl_df")
  expect_equal(nrow(x), 1L)
  expect_true("model_id" %in% names(x))
  expect_identical(names(x)[ncol(x)], "model_id")
  expect_false("species" %in% names(x))
})

test_that("agb_lambert_ung coerces non-character species input", {
  ns <- lu_ns()

  x <- ns$agb_lambert_ung(
    DBH = c(15, 20),
    height = c(12, 16),
    species = factor(c("PICE.MAR", "PICE.MAR")),
    equation_set = "dbh_height"
  )

  expect_s3_class(x, "tbl_df")
  expect_equal(nrow(x), 2L)
  expect_true(all(c("Bwood", "Bbark", "Btotal") %in% names(x)))
})


test_that("equation_set='auto' chooses DBH vs DBHHT per row based on height availability", {
  ns <- lu_ns()

  DBH <- c(22, 30, 18, 35)
  height <- c(18, 24, NA_real_, 27)

  out <- ns$agb_lambert_ung(
    DBH = DBH,
    height = height,
    species = rep("PICE.MAR", 4),
    keep_model_id = TRUE,
    equation_set = "auto"
  )

  expect_equal(out$model_id, c("DBHHT", "DBHHT", "DBH", "DBHHT"))
  expect_identical(names(out)[ncol(out)], "model_id")
})


test_that("output row order matches input order (no reordering across groups)", {
  ns <- lu_ns()

  trees <- tibble::tibble(
    DBH = c(10, 25, 12, 30, 18, 35),
    height = c(NA_real_, 18, 22, NA_real_, 15, 27),
    species = c("PICE.GLA", "PICE.GLA", "PINU.BAN", "PINU.BAN", "ABIE.BAL", "PICE.MAR")
  )

  out <- ns$agb_lambert_ung(
    DBH = trees$DBH,
    height = trees$height,
    species = trees$species,
    keep_model_id = TRUE,
    equation_set = "auto"
  )

  expect_equal(out$model_id, ifelse(is.na(trees$height), "DBH", "DBHHT"))
  expect_identical(names(out)[ncol(out)], "model_id")
})


test_that("can be attached via mutate() without rowwise/list-column explosion", {
  trees <- tibble::tibble(
    tree_id = 1:4,
    DBH = c(22, 30, 18, 35),
    height = c(18, 24, NA_real_, 27),
    species = c("PICE.GLA", "PICE.GLA", "ABIE.BAL", "PINU.BAN")
  )

  out <- trees |>
    dplyr::mutate(
      agb_lambert_ung(DBH, height, species, keep_model_id = TRUE)
    )

  expect_equal(nrow(out), nrow(trees))
  expect_true(all(c("Bwood", "Bbark", "Btotal", "model_id") %in% names(out)))
  expect_equal(out$model_id, ifelse(is.na(trees$height), "DBH", "DBHHT"))
})


test_that("agb_lambert_ung matches manual DBH-only calculations (real coefficients)", {
  ns <- lu_ns()

  DBH <- c(10, 20, 30)
  spp <- "PICE.MAR"
  coefs <- lu_coefs(species = spp, model = "DBH")

  Ywood <- coefs["bwood1"] * DBH^(coefs["bwood2"])
  Ybark <- coefs["bbark1"] * DBH^(coefs["bbark2"])
  Yfoliage <- coefs["bfoliage1"] * DBH^(coefs["bfoliage2"])
  Ybranches <- coefs["bbranches1"] * DBH^(coefs["bbranches2"])

  expected <- tibble::tibble(
    Bwood = as.numeric(Ywood),
    Bbark = as.numeric(Ybark),
    Bstem = as.numeric(Ywood + Ybark),
    Bfoliage = as.numeric(Yfoliage),
    Bbranches = as.numeric(Ybranches),
    Bcrown = as.numeric(Yfoliage + Ybranches),
    Btotal = as.numeric(Ywood + Ybark + Yfoliage + Ybranches)
  )

  got <- ns$agb_lambert_ung(
    DBH = DBH,
    height = NA_real_,
    species = spp,
    equation_set = "dbh"
  )

  expect_equal(got, expected, tolerance = 1e-10)
})


test_that("agb_lambert_ung matches manual DBH+height calculations (real coefficients)", {
  ns <- lu_ns()

  DBH <- c(10, 20, 30)
  H <- c(12, 18, 25)
  spp <- "PICE.MAR"
  coefs <- lu_coefs(species = spp, model = "DBHHT")

  Ywood <- coefs["bwood1"] * DBH^(coefs["bwood2"]) * H^(coefs["bwood3"])
  Ybark <- coefs["bbark1"] * DBH^(coefs["bbark2"]) * H^(coefs["bbark3"])
  Yfoliage <- coefs["bfoliage1"] * DBH^(coefs["bfoliage2"]) * H^(coefs["bfoliage3"])
  Ybranches <- coefs["bbranches1"] * DBH^(coefs["bbranches2"]) * H^(coefs["bbranches3"])

  expected <- tibble::tibble(
    Bwood = as.numeric(Ywood),
    Bbark = as.numeric(Ybark),
    Bstem = as.numeric(Ywood + Ybark),
    Bfoliage = as.numeric(Yfoliage),
    Bbranches = as.numeric(Ybranches),
    Bcrown = as.numeric(Yfoliage + Ybranches),
    Btotal = as.numeric(Ywood + Ybark + Yfoliage + Ybranches)
  )

  got <- ns$agb_lambert_ung(
    DBH = DBH,
    height = H,
    species = spp,
    equation_set = "dbh_height"
  )

  expect_equal(got, expected, tolerance = 1e-10)
})


test_that(".lu_validate_inputs catches DBH and height problems", {
  ns <- lu_ns()

  expect_error(ns$.lu_validate_inputs(DBH = "x", height = 10), "`DBH` must be numeric")
  expect_error(ns$.lu_validate_inputs(DBH = c(10, NA_real_), height = 10), "`DBH` contains NA")
  expect_error(ns$.lu_validate_inputs(DBH = c(10, 0), height = 10), "`DBH` must be > 0")
  expect_error(ns$.lu_validate_inputs(DBH = 10, height = "nope"), "`height` must be numeric")
  expect_error(ns$.lu_validate_inputs(DBH = 10, height = c(10, -1)), "`height` must be > 0 and finite")
  expect_error(ns$.lu_validate_inputs(DBH = 10, height = c(10, Inf)), "`height` must be > 0 and finite")
})


test_that(".lu_validate_inputs allows height NA but enforces recycling rule", {
  ns <- lu_ns()

  expect_silent(ns$.lu_validate_inputs(DBH = c(10, 20), height = c(NA_real_, 15)))

  expect_error(
    ns$.lu_validate_inputs(DBH = c(10, 20), height = c(10, 20, 30)),
    "must have the same length, or one must be length 1"
  )
})


test_that(".lu_standardize_species maps NA to UNKN.SPP", {
  ns <- lu_ns()

  out <- ns$.lu_standardize_species(c(NA_character_, "PICE.MAR"))
  expect_identical(unname(out), c("UNKN.SPP", "PICE.MAR"))
})

test_that(".lu_standardize_species coerces non-character input before mapping", {
  ns <- lu_ns()

  out <- ns$.lu_standardize_species(factor(c(NA, "PICE.MAR")))
  expect_identical(unname(out), c("UNKN.SPP", "PICE.MAR"))
})


test_that(".lu_get_coefs_cached errors on unknown model_id and unknown species", {
  ns <- lu_ns()

  expect_error(
    ns$.lu_get_coefs_cached(model_id = "NOPE", species = "PICE.MAR"),
    "No coefficients found for model_id"
  )

  expect_error(
    ns$.lu_get_coefs_cached(model_id = "DBH", species = "ZZZZ.ZZZ"),
    "Unknown species"
  )

  co <- ns$.lu_get_coefs_cached(model_id = "DBH", species = "PICE.MAR")
  expect_true(is.numeric(co))
  expect_true(all(c("bwood1", "bwood2") %in% names(co)))
})


test_that("agb_lambert_ung errors on length mismatch across DBH/height/species", {
  ns <- lu_ns()

  expect_error(
    ns$agb_lambert_ung(DBH = 1:2, height = 1:3, species = "PICE.MAR"),
    "`DBH` and `height` must have the same length, or one must be length 1",
    fixed = TRUE
  )
})

test_that("agb_lambert_ung checks species length against recycled common length", {
  ns <- lu_ns()

  expect_error(
    ns$agb_lambert_ung(DBH = c(10, 20), height = c(15, 16), species = c("PICE.MAR", "ABIE.BAL", "PINU.BAN")),
    "`DBH`, `height`, and `species` must have the same length, or be length 1.",
    fixed = TRUE
  )
})


test_that("agb_lambert_ung: equation_set dbh_height requires valid height", {
  ns <- lu_ns()

  expect_error(
    ns$agb_lambert_ung(
      DBH = c(10, 20),
      height = c(15, NA_real_),
      species = "PICE.MAR",
      equation_set = "dbh_height"
    ),
    "height.*must be provided"
  )
})

