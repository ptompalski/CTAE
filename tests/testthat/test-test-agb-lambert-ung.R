# tests/testthat/test-agb-lambert-ung.R

test_that("AGB_LambertUngDBH returns a tibble with expected columns and rows", {
  skip_if_not(exists("parameters_LambertUng", inherits = TRUE))
  skip_if_not(requireNamespace("tibble", quietly = TRUE))
  skip_if_not(requireNamespace("dplyr", quietly = TRUE))

  res <- AGB_LambertUngDBH(DBH = 20, species = "PICE.MAR")

  expect_s3_class(res, "tbl_df")
  expect_named(
    res,
    c("Bwood", "Bbark", "Bstem", "Bfoliage", "Bbranches", "Bcrown", "Btotal")
  )
  expect_equal(nrow(res), 1L)
  expect_true(all(vapply(res, is.numeric, logical(1))))
  expect_true(all(res$Bwood > 0))
  expect_true(all(res$Bbark > 0))
  expect_true(all(res$Btotal > 0))

  # internal consistency
  expect_equal(res$Bstem, res$Bwood + res$Bbark, tolerance = 1e-10)
  expect_equal(res$Bcrown, res$Bfoliage + res$Bbranches, tolerance = 1e-10)
  expect_equal(
    res$Btotal,
    res$Bwood + res$Bbark + res$Bfoliage + res$Bbranches,
    tolerance = 1e-10
  )
})

test_that("AGB_LambertUngDBHHT returns a tibble with expected columns and rows", {
  skip_if_not(exists("parameters_LambertUng", inherits = TRUE))
  skip_if_not(requireNamespace("tibble", quietly = TRUE))
  skip_if_not(requireNamespace("dplyr", quietly = TRUE))

  res <- AGB_LambertUngDBHHT(DBH = 20, height = 20, species = "PICE.MAR")

  expect_s3_class(res, "tbl_df")
  expect_named(
    res,
    c("Bwood", "Bbark", "Bstem", "Bfoliage", "Bbranches", "Bcrown", "Btotal")
  )
  expect_equal(nrow(res), 1L)
  expect_true(all(vapply(res, is.numeric, logical(1))))
  expect_true(all(res$Btotal > 0))

  # internal consistency
  expect_equal(res$Bstem, res$Bwood + res$Bbark, tolerance = 1e-10)
  expect_equal(res$Bcrown, res$Bfoliage + res$Bbranches, tolerance = 1e-10)
  expect_equal(
    res$Btotal,
    res$Bwood + res$Bbark + res$Bfoliage + res$Bbranches,
    tolerance = 1e-10
  )
})

test_that("Vectorization works (multiple DBH, single species)", {
  skip_if_not(exists("parameters_LambertUng", inherits = TRUE))

  dbh <- c(10, 15, 20)

  r1 <- AGB_LambertUngDBH(DBH = dbh, species = "PICE.MAR")
  expect_equal(nrow(r1), length(dbh))
  expect_true(all(is.finite(as.matrix(r1))))

  r2 <- AGB_LambertUngDBHHT(DBH = dbh, height = 20, species = "PICE.MAR")
  expect_equal(nrow(r2), length(dbh))
  expect_true(all(is.finite(as.matrix(r2))))
})

test_that("Species affects outputs when coefficients differ in table", {
  skip_if_not(exists("parameters_LambertUng", inherits = TRUE))

  # pick two species that exist for DBH model
  sp <- parameters_LambertUng |>
    dplyr::filter(.data$model == "DBH") |>
    dplyr::distinct(.data$species) |>
    dplyr::pull(.data$species) |>
    unique()

  skip_if(
    length(sp) < 2L,
    "Need at least 2 species in parameters_LambertUng for model == 'DBH'"
  )

  # find a pair that actually has different coefficients (robust to tables with shared params)
  find_pair <- function(species_vec) {
    # compare the full parameter vectors (estimate by parameter) between species
    for (i in seq_along(species_vec)) {
      for (j in seq_along(species_vec)) {
        if (j <= i) {
          next
        }
        a <- parameters_LambertUng |>
          dplyr::filter(
            .data$model == "DBH",
            .data$species == species_vec[i]
          ) |>
          dplyr::arrange(.data$parameter) |>
          dplyr::pull(.data$estimate)
        b <- parameters_LambertUng |>
          dplyr::filter(
            .data$model == "DBH",
            .data$species == species_vec[j]
          ) |>
          dplyr::arrange(.data$parameter) |>
          dplyr::pull(.data$estimate)

        if (length(a) == length(b) && !isTRUE(all.equal(a, b, tolerance = 0))) {
          return(c(species_vec[i], species_vec[j]))
        }
      }
    }
    NULL
  }

  pair <- find_pair(sp)
  skip_if(
    is.null(pair),
    "No species pair with different DBH coefficients found in table"
  )

  dbh <- 20
  rA <- AGB_LambertUngDBH(DBH = dbh, species = pair[1])
  rB <- AGB_LambertUngDBH(DBH = dbh, species = pair[2])

  # at least one component should differ
  expect_true(any(abs(unlist(rA) - unlist(rB)) > 0))
})

test_that("DBH and DBHHT are broadly consistent in scale for typical trees", {
  skip_if_not(exists("parameters_LambertUng", inherits = TRUE))

  # Use a realistic combination; if this fails, it flags coefficient/model mismatches.
  dbh <- 20
  h <- 20
  sp <- "PICE.MAR"

  r_dbh <- AGB_LambertUngDBH(DBH = dbh, species = sp)
  r_dbhht <- AGB_LambertUngDBHHT(DBH = dbh, height = h, species = sp)

  # "Somewhat similar" is subjective; this just catches extreme blow-ups (like 20x to 1000x).
  ratio <- as.numeric(r_dbhht$Btotal / r_dbh$Btotal)

  expect_true(is.finite(ratio))
  expect_lt(ratio, 10) # adjust if you expect bigger spread
  expect_gt(ratio, 0.1) # and not orders of magnitude smaller
})

test_that("Input validation errors are thrown", {
  skip_if_not(exists("parameters_LambertUng", inherits = TRUE))

  expect_error(AGB_LambertUngDBH(DBH = "20", species = "PICE.MAR"), "DBH")
  expect_error(AGB_LambertUngDBH(DBH = -1, species = "PICE.MAR"), "> 0")
  expect_error(
    AGB_LambertUngDBHHT(DBH = 20, height = -1, species = "PICE.MAR"),
    "> 0"
  )
  expect_error(
    AGB_LambertUngDBHHT(
      DBH = c(10, 20),
      height = c(10, 20, 30),
      species = "PICE.MAR"
    ),
    "same length|length 1"
  )
})

test_that("Default species is UNKN.SPP when species is NA", {
  skip_if_not(exists("parameters_LambertUng", inherits = TRUE))

  expect_warning(
    res <- AGB_LambertUngDBH(DBH = 20, species = NA),
    "UNKN\\.SPP"
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)

  expect_warning(
    res2 <- AGB_LambertUngDBHHT(DBH = 20, height = 20, species = NA),
    "UNKN\\.SPP"
  )
  expect_s3_class(res2, "tbl_df")
  expect_equal(nrow(res2), 1L)
})
