gp_registry <- function() {
  CanadaForestAllometry::volume_model_registry()
}

gp_params_for_model <- function(model_id) {
  reg <- gp_registry()
  key <- reg$params_key[reg$model_id == model_id][[1]]
  CanadaForestAllometry:::get_params_tbl(key) |>
    tibble::as_tibble()
}

testthat::test_that("get_volume_params errors on unknown model_id", {
  testthat::expect_error(
    CanadaForestAllometry::get_volume_params(
      model_id = "not_a_real_model",
      species = "PICE.GLA"
    ),
    "Unknown `model_id`",
    fixed = TRUE
  )
})

testthat::test_that("get_volume_params applies Province filter when column exists", {
  reg <- gp_registry()

  model_with_prov <- purrr::detect(reg$model_id, function(mid) {
    p <- gp_params_for_model(mid)
    if (!all(c("Species", "Province") %in% names(p))) {
      return(FALSE)
    }
    any(!is.na(p$Province))
  })

  testthat::skip_if(
    is.null(model_with_prov),
    "No model with a Province column found."
  )

  p <- gp_params_for_model(model_with_prov)
  row <- p |>
    dplyr::filter(!is.na(.data$Province), !is.na(.data$Species)) |>
    dplyr::slice(1)

  testthat::skip_if(nrow(row) == 0, "No species/province rows available.")

  spp <- row$Species[[1]]
  prov <- row$Province[[1]]

  out <- CanadaForestAllometry::get_volume_params(
    model_id = model_with_prov,
    species = spp,
    province = prov,
    strict = FALSE
  )

  testthat::expect_true(nrow(out) > 0)
  testthat::expect_true(all(out$Species == spp))
  testthat::expect_true(all(out$Province == prov))
})

testthat::test_that("get_volume_params applies Subregion filter when column exists", {
  reg <- gp_registry()

  model_with_subr <- purrr::detect(reg$model_id, function(mid) {
    p <- gp_params_for_model(mid)
    if (!all(c("Species", "Subregion") %in% names(p))) {
      return(FALSE)
    }
    any(!is.na(p$Subregion))
  })

  testthat::skip_if(
    is.null(model_with_subr),
    "No model with a Subregion column found."
  )

  p <- gp_params_for_model(model_with_subr)
  row <- p |>
    dplyr::filter(!is.na(.data$Subregion), !is.na(.data$Species)) |>
    dplyr::slice(1)

  testthat::skip_if(nrow(row) == 0, "No species/subregion rows available.")

  spp <- row$Species[[1]]
  subr <- row$Subregion[[1]]

  out <- CanadaForestAllometry::get_volume_params(
    model_id = model_with_subr,
    species = spp,
    subregion = subr,
    strict = FALSE
  )

  testthat::expect_true(nrow(out) > 0)
  testthat::expect_true(all(out$Species == spp))
  testthat::expect_true(all(out$Subregion == subr))
})

testthat::test_that("strict = FALSE returns empty tibble with same columns when no rows match", {
  model_id <- "regional_nigh2016"
  p <- gp_params_for_model(model_id)

  testthat::skip_if(
    !all(c("Species", "Subregion") %in% names(p)),
    "regional_nigh2016 parameters unavailable."
  )

  out <- CanadaForestAllometry::get_volume_params(
    model_id = model_id,
    species = "PICE.SPP",
    subregion = "NOT_A_REAL_SUBREGION",
    strict = FALSE
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), 0L)
  testthat::expect_identical(names(out), names(tibble::as_tibble(p)))
})

testthat::test_that("strict = TRUE includes province/subregion context in error message", {
  testthat::expect_error(
    CanadaForestAllometry::get_volume_params(
      model_id = "national_ung_dbh",
      species = "PICE.SPP",
      province = "BC",
      subregion = "CWH",
      strict = TRUE
    ),
    "No parameters found for model_id='national_ung_dbh'.*province='BC'.*subregion='CWH'",
    perl = TRUE
  )
})

testthat::test_that("genus fallback works when species not found but GENUS.SPP exists", {
  reg <- gp_registry()

  match <- purrr::detect(reg$model_id, function(mid) {
    p <- gp_params_for_model(mid)
    if (!"Species" %in% names(p)) {
      return(FALSE)
    }
    any(grepl("\\.SPP$", p$Species))
  })

  testthat::skip_if(
    is.null(match),
    "No model with genus-group Species (*.SPP) found."
  )

  p <- gp_params_for_model(match)
  spp <- p$Species[grepl("\\.SPP$", p$Species)][[1]]
  probe_species <- sub("\\.SPP$", ".ZZZ", spp)

  out <- CanadaForestAllometry::get_volume_params(
    model_id = match,
    species = probe_species,
    strict = FALSE
  )

  testthat::expect_true(nrow(out) > 0)
  testthat::expect_true(all(out$Species == spp))
})

testthat::test_that("variety code does not trigger GENUS.SPP fallback", {
  reg <- gp_registry()

  match <- purrr::detect(reg$model_id, function(mid) {
    p <- gp_params_for_model(mid)
    if (!"Species" %in% names(p)) {
      return(FALSE)
    }
    any(grepl("\\.SPP$", p$Species))
  })

  testthat::skip_if(
    is.null(match),
    "No model with genus-group Species (*.SPP) found."
  )

  p <- gp_params_for_model(match)
  spp <- p$Species[grepl("\\.SPP$", p$Species)][[1]]
  probe_species_var <- sub("\\.SPP$", ".ZZZ.AAA", spp)

  out <- CanadaForestAllometry::get_volume_params(
    model_id = match,
    species = probe_species_var,
    strict = FALSE
  )

  testthat::expect_equal(nrow(out), 0L)
})

testthat::test_that("get_volume_params applies province filter when provided and Province column exists", {
  fake_reg <- tibble::tibble(
    model_id = "m_prov",
    params_key = "k_prov"
  )
  fake_params <- tibble::tibble(
    Species = c("PICE.GLA", "PICE.GLA"),
    Province = c("AB", "BC"),
    b0 = c(1, 2)
  )

  out <- testthat::with_mocked_bindings(
    {
      CanadaForestAllometry::get_volume_params(
        model_id = "m_prov",
        species = "PICE.GLA",
        province = "AB",
        strict = FALSE
      )
    },
    volume_model_registry = function() fake_reg,
    get_params_tbl = function(key) fake_params,
    standardize_species_code = function(x, ...) x,
    .package = "CanadaForestAllometry"
  )

  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_identical(out$Province[[1]], "AB")
})

testthat::test_that("get_volume_params errors when params object is not a data.frame", {
  fake_reg <- tibble::tibble(
    model_id = "m_bad",
    params_key = "k_bad"
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      {
        CanadaForestAllometry::get_volume_params(
          model_id = "m_bad",
          species = "PICE.GLA"
        )
      },
      volume_model_registry = function() fake_reg,
      get_params_tbl = function(key) list(not = "a data frame"),
      standardize_species_code = function(x, ...) x,
      .package = "CanadaForestAllometry"
    ),
    "is not a data.frame/tibble",
    fixed = FALSE
  )
})

testthat::test_that("get_volume_params errors when Species column is missing", {
  fake_reg <- tibble::tibble(
    model_id = "m_nospp",
    params_key = "k_nospp"
  )
  fake_params <- tibble::tibble(
    Province = "AB",
    b0 = 1
  )

  testthat::expect_error(
    testthat::with_mocked_bindings(
      {
        CanadaForestAllometry::get_volume_params(
          model_id = "m_nospp",
          species = "PICE.GLA"
        )
      },
      volume_model_registry = function() fake_reg,
      get_params_tbl = function(key) fake_params,
      standardize_species_code = function(x, ...) x,
      .package = "CanadaForestAllometry"
    ),
    "must contain a `Species` column",
    fixed = FALSE
  )
})
