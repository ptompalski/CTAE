testthat::test_that("volume_model_registry has expected structure and key models", {
  reg <- CanadaForestAllometry::volume_model_registry()

  testthat::expect_s3_class(reg, "tbl_df")
  testthat::expect_true(all(
    c("model_id", "engine", "params_key", "rank") %in% names(reg)
  ))
  testthat::expect_true(all(c(
    "national_ung_dbh",
    "regional_kozak94",
    "regional_nigh2016"
  ) %in% reg$model_id))
})

testthat::test_that("get_params_tbl returns data frame for known params key", {
  p <- CanadaForestAllometry:::get_params_tbl("parameters_NationalTaperModelsDBH")
  testthat::expect_true(is.data.frame(p))
  testthat::expect_true("Species" %in% names(p))
})

testthat::test_that("get_params_tbl errors for non-data.frame internal object", {
  testthat::expect_error(
    CanadaForestAllometry:::get_params_tbl("parameters_v2b"),
    "is not a data.frame",
    fixed = FALSE
  )
})

testthat::test_that("get_params_tbl executes fallback path for non-parameters key and errors if missing", {
  testthat::expect_error(
    suppressWarnings(
      CanadaForestAllometry:::get_params_tbl("this_object_does_not_exist_anywhere")
    ),
    "Internal params object not found",
    fixed = FALSE
  )
})

testthat::test_that("extract_species_from_params handles common shapes", {
  x1 <- tibble::tibble(Species = c("PICE.GLA", "PICE.GLA", NA_character_, "ABIE.BAL"))
  x2 <- tibble::tibble(species = c("PINU.BAN", NA_character_))
  x3 <- tibble::tibble(foo = 1:3)

  s1 <- CanadaForestAllometry:::extract_species_from_params(x1)
  s2 <- CanadaForestAllometry:::extract_species_from_params(x2)
  s3 <- CanadaForestAllometry:::extract_species_from_params(x3)

  testthat::expect_identical(s1, c("ABIE.BAL", "PICE.GLA"))
  testthat::expect_identical(s2, c("PINU.BAN"))
  testthat::expect_identical(s3, character(0))
})

testthat::test_that("volume_model_registry_species returns species metadata", {
  out <- CanadaForestAllometry:::volume_model_registry_species()
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(all(c("species", "n_species", "species_text") %in% names(out)))
  testthat::expect_true(all(out$n_species >= 0))
})

testthat::test_that("volume_model_registry_species applies backward-compatible defaults", {
  minimal_reg <- tibble::tibble(
    model_id = "m1",
    params_key = "dummy_params"
  )

  out <- testthat::with_mocked_bindings(
    {
      CanadaForestAllometry:::volume_model_registry_species()
    },
    volume_model_registry = function() minimal_reg,
    get_params_tbl = function(params_key) tibble::tibble(Species = c("ABIE.BAL", "PICE.GLA")),
    .package = "CanadaForestAllometry"
  )

  testthat::expect_true("subregion_required" %in% names(out))
  testthat::expect_true("subregion_arg" %in% names(out))
  testthat::expect_true("subregion_type" %in% names(out))
  testthat::expect_identical(out$subregion_required[[1]], FALSE)
  testthat::expect_true(is.na(out$subregion_arg[[1]]))
  testthat::expect_identical(out$subregion_type[[1]], "none")
})

testthat::test_that("volume_model_registry_species wraps get_params_tbl errors with model context", {
  minimal_reg <- tibble::tibble(
    model_id = "model_x",
    params_key = "missing_x",
    subregion_required = FALSE,
    subregion_arg = NA_character_,
    subregion_type = "none"
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      {
        CanadaForestAllometry:::volume_model_registry_species()
      },
      volume_model_registry = function() minimal_reg,
      get_params_tbl = function(params_key) rlang::abort("boom"),
      .package = "CanadaForestAllometry"
    ),
    "Failed to build species list for model_id=model_x",
    fixed = FALSE
  )
})
