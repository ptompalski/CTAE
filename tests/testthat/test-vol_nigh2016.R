# tests/testthat/test-vol_nigh2016.R

testthat::test_that("vol_nigh2016: returns tibble and matches Nigh2016 formula", {
  # get parameters via API (works for internal tables)
  reg <- CanadaForestAllometry::get_volume_params(
    model_id = "regional_nigh2016",
    species = "PICE.SPP", # genus-group guaranteed to exist if the model supports PICE
    subregion = "ALL",
    strict = FALSE
  )

  if (nrow(reg) == 0) {
    testthat::skip("No Nigh2016 parameters available (regional_nigh2016).")
  }

  params <- dplyr::as_tibble(reg)

  testthat::expect_true(all(
    c("Species", "Subregion", "volume_type", "b0", "b1", "b2") %in%
      names(params)
  ))

  keys <- params |>
    dplyr::group_by(.data$Species, .data$Subregion) |>
    dplyr::summarise(
      has_total = any(.data$volume_type == "total"),
      has_merch = any(.data$volume_type %in% c("merch", "merchantable")),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$has_total, .data$has_merch) |>
    dplyr::arrange(.data$Species, .data$Subregion)

  testthat::expect_true(nrow(keys) > 0)

  sp <- keys$Species[[1]]
  subr <- keys$Subregion[[1]]

  # pull exact rows using API so we’re consistent with model selection
  p_tot <- CanadaForestAllometry::get_volume_params(
    "regional_nigh2016",
    species = sp,
    subregion = subr,
    strict = FALSE
  ) |>
    dplyr::filter(.data$volume_type == "total") |>
    dplyr::slice(1)

  p_mer <- CanadaForestAllometry::get_volume_params(
    "regional_nigh2016",
    species = sp,
    subregion = subr,
    strict = FALSE
  ) |>
    dplyr::filter(.data$volume_type %in% c("merch", "merchantable")) |>
    dplyr::slice(1)

  DBH <- 30
  ht <- 20

  out <- CanadaForestAllometry::vol_nigh2016(
    DBH = DBH,
    height = ht,
    species = sp,
    subregion = subr
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, c("vol_total", "vol_merchantable"))
  testthat::expect_equal(nrow(out), 1L)

  exp_fun <- function(b0, b1, b2) exp(b0) * (DBH^b1) * (ht^b2)
  vt_exp <- exp_fun(p_tot$b0[[1]], p_tot$b1[[1]], p_tot$b2[[1]])
  vm_exp <- exp_fun(p_mer$b0[[1]], p_mer$b1[[1]], p_mer$b2[[1]])

  testthat::expect_equal(out$vol_total[[1]], vt_exp, tolerance = 1e-8)
  testthat::expect_equal(out$vol_merchantable[[1]], vm_exp, tolerance = 1e-8)
})

testthat::test_that("vol_nigh2016: vectorization and subregion recycling", {
  params <- CanadaForestAllometry::get_volume_params(
    model_id = "regional_nigh2016",
    species = "PICE.SPP",
    subregion = "ALL",
    strict = FALSE
  )
  if (nrow(params) == 0) {
    testthat::skip("No Nigh2016 parameters available.")
  }

  keys <- params |>
    dplyr::group_by(.data$Species, .data$Subregion) |>
    dplyr::summarise(
      has_total = any(.data$volume_type == "total"),
      has_merch = any(.data$volume_type %in% c("merch", "merchantable")),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$has_total, .data$has_merch) |>
    dplyr::arrange(.data$Species, .data$Subregion)

  sp <- keys$Species[[1]]
  subr <- keys$Subregion[[1]]

  out <- CanadaForestAllometry::vol_nigh2016(
    DBH = c(15, 25, 35),
    height = c(12, 18, 24),
    species = rep(sp, 3),
    subregion = subr
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(is.finite(out$vol_total)))
  testthat::expect_true(all(is.finite(out$vol_merchantable)))
  testthat::expect_true(all(out$vol_merchantable <= out$vol_total))
})

testthat::test_that("vol_nigh2016: input validation errors", {
  testthat::expect_error(
    CanadaForestAllometry::vol_nigh2016(
      DBH = 20,
      height = 20,
      species = "PICE.GLA"
    ),
    "subregion is required",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::vol_nigh2016(
      DBH = c(20, 30),
      height = c(20, 22),
      species = c("PICE.GLA", "ABIE.BAL"),
      subregion = c("Coast", "Interior", "CWH")
    ),
    "subregion must have length 1 or the same length as DBH",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::vol_nigh2016(
      DBH = c(20, 30),
      height = 20,
      species = "PICE.GLA",
      subregion = "Coast"
    ),
    "DBH, height, and species must have the same length",
    ignore.case = TRUE
  )
})

testthat::test_that("vol_nigh2016: rejects invalid geometry values", {
  params <- CanadaForestAllometry::get_volume_params(
    model_id = "regional_nigh2016",
    species = "PICE.SPP",
    subregion = "ALL",
    strict = FALSE
  )
  if (nrow(params) == 0) {
    testthat::skip("No Nigh2016 parameters available.")
  }

  keys <- params |>
    dplyr::group_by(.data$Species, .data$Subregion) |>
    dplyr::summarise(
      has_total = any(.data$volume_type == "total"),
      has_merch = any(.data$volume_type %in% c("merch", "merchantable")),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$has_total, .data$has_merch) |>
    dplyr::arrange(.data$Species, .data$Subregion)

  sp <- keys$Species[[1]]
  subr <- keys$Subregion[[1]]

  testthat::expect_error(
    CanadaForestAllometry::vol_nigh2016(
      DBH = 0,
      height = 20,
      species = sp,
      subregion = subr
    ),
    "DBH must be > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::vol_nigh2016(
      DBH = 20,
      height = 0,
      species = sp,
      subregion = subr
    ),
    "height must be > 0",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::vol_nigh2016(
      DBH = 20,
      height = 1.2,
      species = sp,
      subregion = subr
    ),
    "height must be >= 1.3",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::vol_nigh2016(
      DBH = NA_real_,
      height = 20,
      species = sp,
      subregion = subr
    ),
    "finite numeric values",
    ignore.case = TRUE
  )
})

testthat::test_that("vol_nigh2016: unknown subregion produces informative error", {
  params <- CanadaForestAllometry::get_volume_params(
    model_id = "regional_nigh2016",
    species = "PICE.SPP",
    subregion = "ALL",
    strict = FALSE
  )
  if (nrow(params) == 0) {
    testthat::skip("No Nigh2016 parameters available.")
  }

  sp <- params$Species[[1]]

  testthat::expect_error(
    CanadaForestAllometry::vol_nigh2016(
      DBH = 20,
      height = 18,
      species = sp,
      subregion = "NOT_A_REAL_SUBREGION"
    ),
    "No parameters returned|No Nigh2016 parameters found",
    ignore.case = TRUE
  )
})

testthat::test_that("vol_nigh2016: works across all species/subregion pairs", {
  params <- CanadaForestAllometry::get_volume_params(
    model_id = "regional_nigh2016",
    species = "PICE.SPP",
    subregion = "ALL",
    strict = FALSE
  )
  if (nrow(params) == 0) {
    testthat::skip("No Nigh2016 parameters available.")
  }

  keys <- params |>
    dplyr::group_by(.data$Species, .data$Subregion) |>
    dplyr::summarise(
      has_total = any(.data$volume_type == "total"),
      has_merch = any(.data$volume_type %in% c("merch", "merchantable")),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$has_total, .data$has_merch)

  testthat::expect_true(nrow(keys) > 0)

  DBH <- 30
  ht <- 20

  out <- CanadaForestAllometry::vol_nigh2016(
    DBH = rep(DBH, nrow(keys)),
    height = rep(ht, nrow(keys)),
    species = keys$Species,
    subregion = keys$Subregion
  )

  testthat::expect_equal(nrow(out), nrow(keys))
  testthat::expect_true(all(is.finite(out$vol_total)))
  testthat::expect_true(all(is.finite(out$vol_merchantable)))
  testthat::expect_true(all(out$vol_total >= 0))
  testthat::expect_true(all(out$vol_merchantable >= 0))
  testthat::expect_true(all(out$vol_merchantable <= out$vol_total))
})

testthat::test_that("get_volume_params supports genus-group fallback for Nigh (PICE.GLA -> PICE.SPP)", {
  p <- CanadaForestAllometry::get_volume_params(
    "regional_nigh2016",
    species = "PICE.GLA",
    subregion = "Interior"
  )
  testthat::expect_true(nrow(p) > 0)
})

testthat::test_that("vol_nigh2016 matches manual calculations for 4 reference rows (region + BEC; total + merchantable)", {
  # Helper: manual Nigh (2016) prediction
  nigh_manual <- function(DBH, H, b0, b1, b2) exp(b0) * (DBH^b1) * (H^b2)

  DBH <- 20
  H <- 20

  # 1) REGION total (PSEU.MEN, Coast)
  out <- CanadaForestAllometry::vol_nigh2016(DBH, H, "PSEU.MEN", "Coast")
  v_manual <- nigh_manual(DBH, H, -9.988, 1.709, 1.159)
  testthat::expect_equal(out$vol_total[[1]], v_manual, tolerance = 1e-10)

  # 2) REGION merch (TSUG.HET, Interior)
  out <- CanadaForestAllometry::vol_nigh2016(DBH, H, "TSUG.HET", "Interior")
  v_manual <- nigh_manual(DBH, H, -10.387, 1.885, 1.114)
  testthat::expect_equal(out$vol_merchantable[[1]], v_manual, tolerance = 1e-10)

  # 3) BEC total (ABIE.LAS, ICH)
  out <- CanadaForestAllometry::vol_nigh2016(DBH, H, "ABIE.LAS", "ICH")
  v_manual <- nigh_manual(DBH, H, -9.931, 1.916, 0.960)
  testthat::expect_equal(out$vol_total[[1]], v_manual, tolerance = 1e-10)

  # 4) BEC merch (LARI.LAX, BWBS)
  out <- CanadaForestAllometry::vol_nigh2016(DBH, H, "LARI.LAX", "BWBS")
  v_manual <- nigh_manual(DBH, H, -10.315, 1.717, 1.266)
  testthat::expect_equal(out$vol_merchantable[[1]], v_manual, tolerance = 1e-10)
})
