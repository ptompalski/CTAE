test_that("vol() pick_best=TRUE returns one row per tree and expected columns", {
  out <- vol(
    DBH = c(20, 22),
    height = c(20, 18),
    species = c("PICE.MAR", "BETU.PAP"),
    jurisdiction = c("AB", "ON"),
    pick_best = TRUE
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2)
  expect_true(all(c("vol_total", "vol_merchantable") %in% names(out)))

  # Depending on implementation, vol_model may or may not be present when keep_model_id=FALSE.
  # If present, it should be character (often NA) and length nrow(out).
  if ("vol_model" %in% names(out)) {
    expect_true(is.character(out$vol_model))
    expect_equal(length(out$vol_model), nrow(out))
  }
})

test_that("vol() keep_model_id=TRUE includes vol_model when pick_best=TRUE", {
  out <- vol(
    DBH = 20,
    height = 20,
    species = "PICE.MAR",
    jurisdiction = "AB",
    pick_best = TRUE,
    keep_model_id = TRUE
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_true(all(
    c("vol_total", "vol_merchantable", "vol_model") %in% names(out)
  ))
  expect_true(is.character(out$vol_model))
  expect_equal(length(out$vol_model), 1)
  expect_true(nzchar(out$vol_model))
})

test_that("vol() pick_best=FALSE returns list suitable for list-column use", {
  # This mode may trigger merchantability warnings in some engines; silence in tests
  res <- suppressWarnings(
    vol(
      DBH = c(18, 22, 30, 26),
      height = c(15, 18, 22, 20),
      species = c("PICE.MAR", "BETU.PAP", "POPU.TRE", "PSEU.MEN"),
      jurisdiction = c("AB", "ON", "QC", "BC"),
      subregion = c(NA, NA, NA, "CWH"),
      pick_best = FALSE,
      keep_model_id = TRUE
    )
  )

  expect_true(is.list(res))
  expect_equal(length(res), 4)

  purrr::walk(res, function(x) {
    expect_s3_class(x, "tbl_df")
    expect_true(all(
      c("vol_total", "vol_merchantable", "vol_model") %in% names(x)
    ))
  })
})

test_that("vol() pick_best=FALSE can be unnested after mutate()", {
  trees <- tibble::tibble(
    DBH = c(18, 22, 30, 26),
    height = c(15, 18, 22, 20),
    species = c("PICE.MAR", "BETU.PAP", "POPU.TRE", "PSEU.MEN"),
    jurisdiction = c("AB", "ON", "QC", "BC"),
    subregion = c(NA, NA, NA, "CWH")
  )

  out <- trees |>
    dplyr::mutate(
      v = suppressWarnings(
        vol(
          DBH,
          height,
          species,
          jurisdiction,
          subregion,
          pick_best = FALSE,
          keep_model_id = TRUE
        )
      )
    ) |>
    tidyr::unnest(v)

  expect_s3_class(out, "tbl_df")
  expect_true(all(
    c("vol_total", "vol_merchantable", "vol_model") %in% names(out)
  ))
  expect_true(nrow(out) >= nrow(trees))
})

test_that("subregion NA does not override engine defaults (AB Huang appears without subregion)", {
  res <- suppressWarnings(
    vol(
      DBH = 20,
      height = 20,
      species = "PICE.MAR",
      jurisdiction = "AB",
      subregion = NA_character_,
      pick_best = FALSE,
      keep_model_id = TRUE
    )
  )

  expect_true(is.list(res))
  expect_equal(length(res), 1)
  cand <- res[[1]]

  # Huang should be among candidates when eligible
  expect_true(any(cand$vol_model == "vol_huang94"))
})

test_that("pick_best=TRUE prefers highest-ranked successful model (AB Huang over Ung)", {
  out <- vol(
    DBH = 20,
    height = 20,
    species = "PICE.MAR",
    jurisdiction = "AB",
    pick_best = TRUE,
    keep_model_id = TRUE
  )

  expect_equal(out$vol_model, "vol_huang94")
})

test_that("Ung variants are collapsed to a single row per tree when pick_best=FALSE", {
  res <- suppressWarnings(
    vol(
      DBH = 22,
      height = 18,
      species = "BETU.PAP",
      jurisdiction = "ON",
      pick_best = FALSE,
      keep_model_id = TRUE
    )
  )

  cand <- res[[1]]
  expect_lte(sum(cand$vol_model == "vol_ung2013"), 1)
})

test_that("invalid species codes error early with informative message", {
  expect_error(
    vol(
      DBH = 20,
      height = 20,
      species = "NOT.A.SPP",
      jurisdiction = "ON",
      pick_best = TRUE
    ),
    "Unrecognized species codes",
    fixed = TRUE
  )
})

test_that("vol() validates vector lengths and non-empty DBH", {
  expect_error(
    vol(
      DBH = c(20, 21),
      height = 20,
      species = "PICE.MAR",
      jurisdiction = c("AB", "AB", "AB")
    ),
    "`jurisdiction` must have length 1 or 2",
    fixed = FALSE
  )

  expect_error(
    vol(
      DBH = numeric(0),
      height = numeric(0),
      species = character(0),
      jurisdiction = character(0)
    ),
    "`DBH` must have length > 0",
    fixed = FALSE
  )
})

test_that("vol() handles missing backward-compat registry columns", {
  reg_min <- tibble::tibble(
    model_id = "m1",
    engine = "vol_huang94",
    rank = 1,
    requires_ht = FALSE,
    province_scope = list("ALL"),
    species = list(c("PICE.MAR")),
    subregion_type = NA_character_
  )

  out <- testthat::with_mocked_bindings(
    vol(
      DBH = 20,
      height = NA_real_,
      species = "PICE.MAR",
      jurisdiction = "AB",
      keep_model_id = TRUE
    ),
    volume_model_registry_species = function() reg_min,
    vol_huang94 = function(DBH, height, species, jurisdiction, subregion = NULL) {
      tibble::tibble(vol_total = 1, vol_merchantable = 0.5)
    },
    .package = "CanadaForestAllometry"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_true(all(c("vol_total", "vol_merchantable", "vol_model") %in% names(out)))
})

test_that("vol() errors when no eligible model exists", {
  reg_none <- tibble::tibble(
    model_id = "m1",
    engine = "vol_huang94",
    rank = 1,
    requires_ht = FALSE,
    province_scope = list(character(0)),
    species = list(character(0)),
    subregion_required = FALSE,
    subregion_arg = NA_character_
  )

  expect_error(
    testthat::with_mocked_bindings(
      vol(DBH = 20, height = 20, species = "PICE.MAR", jurisdiction = "AB"),
      volume_model_registry_species = function() reg_none,
      .package = "CanadaForestAllometry"
    ),
    "No eligible volume model found",
    fixed = FALSE
  )
})

test_that("vol() handles engine failures differently for pick_best TRUE vs FALSE", {
  reg_bad <- tibble::tibble(
    model_id = "m1",
    engine = "vol_huang94",
    rank = 1,
    requires_ht = FALSE,
    province_scope = list("ALL"),
    species = list(c("PICE.MAR")),
    subregion_required = FALSE,
    subregion_arg = NA_character_
  )

  expect_error(
    testthat::with_mocked_bindings(
      vol(
        DBH = 20,
        height = 20,
        species = "PICE.MAR",
        jurisdiction = "AB",
        pick_best = TRUE
      ),
      volume_model_registry_species = function() reg_bad,
      vol_huang94 = function(DBH, height, species, jurisdiction, subregion = NULL) {
        tibble::tibble(not_expected = 1)
      },
      .package = "CanadaForestAllometry"
    ),
    "No volume model succeeded for at least one input row",
    fixed = FALSE
  )

  out_list <- testthat::with_mocked_bindings(
    vol(
      DBH = 20,
      height = 20,
      species = "PICE.MAR",
      jurisdiction = "AB",
      pick_best = FALSE
    ),
    volume_model_registry_species = function() reg_bad,
    vol_huang94 = function(DBH, height, species, jurisdiction, subregion = NULL) {
      tibble::tibble(not_expected = 1)
    },
    .package = "CanadaForestAllometry"
  )

  expect_true(is.list(out_list))
  expect_equal(length(out_list), 1)
  expect_s3_class(out_list[[1]], "tbl_df")
  expect_equal(nrow(out_list[[1]]), 0L)
  expect_true(all(c("vol_total", "vol_merchantable", "vol_model") %in% names(out_list[[1]])))
})
