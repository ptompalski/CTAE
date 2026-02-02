testthat::test_that("vol_fortin2007: basic behavior and output shape", {
  skip_if_not(exists("parameters_fortin2007", inherits = TRUE))

  spp <- parameters_fortin2007$Species
  testthat::expect_true(length(spp) > 0)

  out <- vol_fortin2007(
    DBH = rep(20, length(spp)),
    height = rep(18, length(spp)),
    species = spp
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, c("vol_total", "vol_merchantable"))
  testthat::expect_equal(nrow(out), length(spp))

  # Total is placeholder for now
  testthat::expect_true(all(is.na(out$vol_total)))

  # Merchantable should be finite and >= 0
  testthat::expect_true(all(is.finite(out$vol_merchantable)))
  testthat::expect_true(all(out$vol_merchantable >= 0))
})

testthat::test_that("vol_fortin2007: DBH < 9.1 cm returns 0 merchantable volume", {
  skip_if_not(exists("parameters_fortin2007", inherits = TRUE))

  spp <- parameters_fortin2007$Species

  out <- vol_fortin2007(
    DBH = rep(9.0, length(spp)),
    height = rep(10, length(spp)),
    species = spp
  )

  testthat::expect_true(all(out$vol_merchantable == 0))
})

testthat::test_that("vol_fortin2007: input validation (lengths + finite + min height)", {
  skip_if_not(exists("parameters_fortin2007", inherits = TRUE))

  spp1 <- parameters_fortin2007$Species[[1]]

  testthat::expect_error(
    vol_fortin2007(DBH = c(20, 25), height = 18, species = c(spp1, spp1)),
    "must have the same length"
  )

  testthat::expect_error(
    vol_fortin2007(DBH = NA_real_, height = 18, species = spp1),
    "finite"
  )

  testthat::expect_error(
    vol_fortin2007(DBH = 20, height = Inf, species = spp1),
    "finite"
  )

  testthat::expect_error(
    vol_fortin2007(DBH = 20, height = 1.2, species = spp1),
    ">= 1\\.3"
  )

  testthat::expect_error(
    vol_fortin2007(DBH = -1, height = 18, species = spp1),
    "DBH must be > 0"
  )
})

testthat::test_that("vol_fortin2007: all species give reasonable volumes for typical trees", {
  skip_if_not(exists("parameters_fortin2007", inherits = TRUE))

  spp <- parameters_fortin2007$Species

  # A "typical" tree case used to sanity check all species
  DBH <- rep(25, length(spp))
  H <- rep(20, length(spp))

  out <- vol_fortin2007(DBH = DBH, height = H, species = spp)
  v <- out$vol_merchantable

  testthat::expect_true(all(is.finite(v)))
  testthat::expect_true(all(v >= 0))

  # Plausibility bound (merchantable m^3): this is generous but catches explosions
  # For DBH=25 cm, H=20 m, merchantable volume should not be anywhere near 10+ m^3
  testthat::expect_true(all(v < 10))
})

testthat::test_that("vol_fortin2007: volumes increase with DBH for realistic ranges", {
  skip_if_not(exists("parameters_fortin2007", inherits = TRUE))

  spp <- parameters_fortin2007$Species[[1]]

  dbh_seq <- seq(10, 60, by = 5)
  ht_fixed <- rep(20, length(dbh_seq))

  out <- vol_fortin2007(
    DBH = dbh_seq,
    height = ht_fixed,
    species = rep(spp, length(dbh_seq))
  )
  v <- out$vol_merchantable

  # Non-decreasing (allow tiny numerical jitter)
  dv <- diff(v)
  testthat::expect_true(all(dv >= -1e-12))
})

testthat::test_that("vol_fortin2007: volumes increase with height for realistic ranges", {
  skip_if_not(exists("parameters_fortin2007", inherits = TRUE))

  spp <- parameters_fortin2007$Species[[1]]

  ht_seq <- seq(5, 35, by = 2.5)
  dbh_fixed <- rep(30, length(ht_seq))

  out <- vol_fortin2007(
    DBH = dbh_fixed,
    height = ht_seq,
    species = rep(spp, length(ht_seq))
  )
  v <- out$vol_merchantable

  dv <- diff(v)
  testthat::expect_true(all(dv >= -1e-12))
})

testthat::test_that("vol_fortin2007: does not produce extreme values across species grid", {
  skip_if_not(exists("parameters_fortin2007", inherits = TRUE))

  spp <- parameters_fortin2007$Species

  # A small grid over realistic DBH/height combinations, replicated over all species
  dbh_vals <- c(10, 20, 40, 60, 80)
  ht_vals <- c(10, 20, 30, 40)

  grid <- tidyr::crossing(DBH = dbh_vals, height = ht_vals, Species = spp)

  out <- vol_fortin2007(
    DBH = grid$DBH,
    height = grid$height,
    species = grid$Species
  )
  v <- out$vol_merchantable

  testthat::expect_true(all(is.finite(v)))
  testthat::expect_true(all(v >= 0))

  # Very generous upper bound for merchantable m^3 at DBH<=80 cm, H<=40 m
  testthat::expect_true(all(v < 50))
})

testthat::test_that("vol_fortin2007 matches manual calculations: hardwood vs conifer", {
  # model paramters entered manually from the java implementation. They match the parameters in the publication, although have more decimals
  cases <- tibble::tribble(
    ~species   , ~DBH , ~H , ~b1              , ~b2              , ~b3              ,
    "BETU.PAP" ,   20 , 20 , -16.000108490313 , 0.43866084427706 ,  0               ,
    "PICE.MAR" ,   20 , 20 , -16.000108490313 , 0.51037282935719 , -0.0034365415188
  )

  for (k in seq_len(nrow(cases))) {
    DBH <- cases$DBH[k]
    H <- cases$H[k]
    spp <- cases$species[k]

    cyl_dm3 <- pi * DBH^2 * H / 40
    pred_dm3 <- cases$b1[k] *
      (H / DBH) +
      cases$b2[k] * cyl_dm3 +
      cases$b3[k] * (cyl_dm3 * DBH)

    expected_m3 <- pred_dm3 / 1000
    out <- vol_fortin2007(DBH = DBH, height = H, species = spp)

    testthat::expect_equal(
      out$vol_merchantable[[1]],
      expected_m3,
      tolerance = 1e-7
    )
  }
})
