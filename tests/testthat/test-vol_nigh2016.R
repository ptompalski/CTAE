# tests/testthat/test-vol_nigh2016.R

.local_data <- function(obj_name) {
  e <- new.env(parent = emptyenv())
  utils::data(list = obj_name, package = "CanadaForestAllometry", envir = e)
  if (!exists(obj_name, envir = e, inherits = FALSE)) {
    testthat::skip(paste0(
      "Dataset `",
      obj_name,
      "` not available via utils::data()."
    ))
  }
  get(obj_name, envir = e, inherits = FALSE)
}

testthat::test_that("vol_nigh2016: returns tibble and matches Nigh2016 formula", {
  params <- dplyr::as_tibble(.local_data("parameters_Nigh2016"))

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

  p_tot <- params |>
    dplyr::filter(
      .data$Species == sp,
      .data$Subregion == subr,
      .data$volume_type == "total"
    ) |>
    dplyr::slice(1)

  p_mer <- params |>
    dplyr::filter(
      .data$Species == sp,
      .data$Subregion == subr,
      .data$volume_type %in% c("merch", "merchantable")
    ) |>
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

  exp_fun <- function(b0, b1, b2) {
    exp(b0) * (DBH^b1) * (ht^b2)
  }
  vt_exp <- exp_fun(p_tot$b0[[1]], p_tot$b1[[1]], p_tot$b2[[1]])
  vm_exp <- exp_fun(p_mer$b0[[1]], p_mer$b1[[1]], p_mer$b2[[1]])

  testthat::expect_equal(out$vol_total[[1]], vt_exp, tolerance = 1e-8)
  testthat::expect_equal(out$vol_merchantable[[1]], vm_exp, tolerance = 1e-8)
})

testthat::test_that("vol_nigh2016: vectorization and subregion recycling", {
  params <- dplyr::as_tibble(.local_data("parameters_Nigh2016"))

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
  params <- dplyr::as_tibble(.local_data("parameters_Nigh2016"))
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
  params <- dplyr::as_tibble(.local_data("parameters_Nigh2016"))
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
  params <- dplyr::as_tibble(.local_data("parameters_Nigh2016"))

  keys <- params |>
    dplyr::group_by(.data$Species, .data$Subregion) |>
    dplyr::summarise(
      has_total = any(.data$volume_type == "total"),
      has_merch = any(.data$volume_type %in% c("merch", "merchantable")),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$has_total, .data$has_merch)

  testthat::expect_true(nrow(keys) > 0)

  # Use a common, valid geometry for all
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
test_that("get_volume_params supports genus-group fallback for Nigh (PICE.GLA -> PICE.SPP)", {
  skip_if_not(exists("get_volume_params", mode = "function"))

  p <- get_volume_params(
    "regional_nigh2016",
    species = "PICE.GLA",
    subregion = "Interior"
  )
  expect_true(nrow(p) > 0)
})

test_that("vol_nigh2016 matches manual calculations for 4 reference rows (region + BEC; total + merchantable)", {
  skip_if_not(exists("vol_nigh2016", mode = "function"))
  skip_if_not(exists("get_volume_params", mode = "function"))

  # Helper: manual Nigh (2016) prediction
  nigh_manual <- function(DBH, H, b0, b1, b2) {
    exp(b0) * (DBH^b1) * (H^b2)
  }

  # Choosing test inputs
  DBH <- 20
  H <- 20

  # ---- 1) REGION: total volume (hard-coded from PDF Table 4, Species Fd (PSEU.MEN), Coastal) ----
  #  –9.988 1.709 1.159
  b0 <- -9.988
  b1 <- 1.709
  b2 <- 1.159

  out <- vol_nigh2016(DBH, H, "PSEU.MEN", "Coast")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("vol_total", "vol_merchantable"))

  v_manual <- nigh_manual(DBH, H, b0, b1, b2)
  expect_equal(out$vol_total[[1]], v_manual, tolerance = 1e-10)

  # ---- 2) REGION: merchantable volume (hard-coded from PDF Table 6, Species=Hw (TSUG.HET), Interior) ----
  # –10.387 1.885 1.114
  b0 <- -10.387
  b1 <- 1.885
  b2 <- 1.114

  out <- vol_nigh2016(DBH, H, "TSUG.HET", "Interior")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("vol_total", "vol_merchantable"))

  v_manual <- nigh_manual(DBH, H, b0, b1, b2)
  expect_equal(out$vol_merchantable[[1]], v_manual, tolerance = 1e-10)

  # ---- 3) BEC: total volume (hard-coded from PDF Table 9, Species Bl (ABIE.LAS), ICH) ----
  #  –9.931 1.916 0.960
  b0 <- -9.931
  b1 <- 1.916
  b2 <- 0.960

  out <- vol_nigh2016(DBH, H, "ABIE.LAS", "ICH")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("vol_total", "vol_merchantable"))

  v_manual <- nigh_manual(DBH, H, b0, b1, b2)
  expect_equal(out$vol_total[[1]], v_manual, tolerance = 1e-10)

  # ---- 4) BEC: merchantable volume (hard-coded from PDF Table 11, Species L (LARI.LAX), BWBS) ----
  #  –10.315 1.717 1.266
  b0 <- -10.315
  b1 <- 1.717
  b2 <- 1.266

  out <- vol_nigh2016(DBH, H, "LARI.LAX", "BWBS")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("vol_total", "vol_merchantable"))

  v_manual <- nigh_manual(DBH, H, b0, b1, b2)
  expect_equal(out$vol_merchantable[[1]], v_manual, tolerance = 1e-10)
})
