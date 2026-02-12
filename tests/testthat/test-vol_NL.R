# tests/testthat/test-vol_NL.R

testthat::test_that("vol_NL validates lengths and subregion inputs", {
  ns <- getNamespace("CanadaForestAllometry")

  # ---- length mismatch ----
  testthat::expect_error(
    CanadaForestAllometry::vol_NL(
      DBH = c(10, 20),
      height = 20,
      species = "PICE.MAR"
    ),
    "must have the same length",
    fixed = FALSE
  )

  # ---- invalid subregion values (must be Province/ALL or 1-24) ----
  testthat::expect_error(
    CanadaForestAllometry::vol_NL(20, 20, "PICE.MAR", subregion = 25),
    "Invalid NL subregion",
    fixed = FALSE
  )

  testthat::expect_error(
    CanadaForestAllometry::vol_NL(20, 20, "PICE.MAR", subregion = "foo"),
    "Invalid NL subregion",
    fixed = FALSE
  )

  # NA subregion should fail validation too
  testthat::expect_error(
    CanadaForestAllometry::vol_NL(20, 20, "PICE.MAR", subregion = NA),
    "Invalid NL subregion",
    fixed = FALSE
  )
})

testthat::test_that("vol_NL subregion normalization and C# district-fallback behavior", {
  ns <- getNamespace("CanadaForestAllometry")

  called_subregions <- character(0)

  # Minimal merch criteria (NL-wide)
  mock_get_merch_criteria <- function(jurisdiction, verbose = FALSE, ...) {
    tibble::tibble(
      jurisdiction = "NL",
      species = "ALL",
      stumpht_m = 0.15,
      topdbh_cm = 7.6,
      mindbh_cm = 9
    )
  }

  # Return a simple, valid NX-122/NX-67 row regardless of subregion.
  # Also record which subregion vol_NL asked for.
  mock_get_volume_params <- function(model_id, species, subregion, ...) {
    called_subregions <<- c(called_subregions, as.character(subregion))

    tibble::tibble(
      param_set = "NX122_NX67",
      Species = species,
      Subregion = as.character(subregion),
      t = 0.176,
      a = 2.184,
      b = 287.181,
      c = 0.9604,
      d = -0.1660,
      e = -0.7868,
      nv_a = -999,
      nv_b = -999,
      nv_c = -999
    )
  }

  # Keep standardize_species_code deterministic in tests
  mock_standardize_species_code <- function(x, ...) x

  testthat::local_mocked_bindings(
    get_merch_criteria = mock_get_merch_criteria,
    get_volume_params = mock_get_volume_params,
    standardize_species_code = mock_standardize_species_code,
    .env = ns
  )

  # subregion default Province -> ALL
  out1 <- CanadaForestAllometry::vol_NL(20, 20, "PICE.MAR")
  testthat::expect_equal(ncol(out1), 2)
  testthat::expect_true(all(
    c("vol_total", "vol_merchantable") %in% names(out1)
  ))

  # District 1 is accepted, but should FALL BACK to ALL (C# behavior)
  out2 <- CanadaForestAllometry::vol_NL(20, 20, "PICE.MAR", subregion = 1)
  testthat::expect_equal(out2$vol_total, out1$vol_total)
  testthat::expect_equal(out2$vol_merchantable, out1$vol_merchantable)

  # District 19 is accepted, but should FALL BACK to ALL (C# behavior)
  out3 <- CanadaForestAllometry::vol_NL(20, 20, "PICE.MAR", subregion = 19)
  testthat::expect_equal(out3$vol_total, out1$vol_total)
  testthat::expect_equal(out3$vol_merchantable, out1$vol_merchantable)

  # District 2 should be used (not forced to ALL)
  out4 <- CanadaForestAllometry::vol_NL(20, 20, "PICE.MAR", subregion = 2)

  # We didn't change the params by subregion in this mock,
  # so numerically it matches, but we can still check which subregion was requested.
  testthat::expect_true(any(called_subregions == "2"))
  testthat::expect_true(any(called_subregions == "ALL"))

  # Subregion synonyms should map to ALL and not warn
  testthat::expect_silent(CanadaForestAllometry::vol_NL(
    20,
    20,
    "PICE.MAR",
    subregion = "Province"
  ))
  testthat::expect_silent(CanadaForestAllometry::vol_NL(
    20,
    20,
    "PICE.MAR",
    subregion = "ALL"
  ))
  testthat::expect_silent(CanadaForestAllometry::vol_NL(
    20,
    20,
    "PICE.MAR",
    subregion = "nl"
  ))
  testthat::expect_silent(CanadaForestAllometry::vol_NL(
    20,
    20,
    "PICE.MAR",
    subregion = "prov"
  ))
})

testthat::test_that("vol_NL NX-242 branch + net handling + constraints", {
  ns <- getNamespace("CanadaForestAllometry")

  mock_get_merch_criteria <- function(jurisdiction, verbose = FALSE, ...) {
    tibble::tibble(
      jurisdiction = "NL",
      species = "ALL",
      stumpht_m = 0.15,
      topdbh_cm = 7.6,
      mindbh_cm = 9
    )
  }

  # Return NX-242 district params only for District 2, otherwise NX-122.
  # Use nv_* to force (a) net > gross cap and (b) net < 0.95*gross constraint.
  mock_get_volume_params <- function(model_id, species, subregion, ...) {
    subregion <- as.character(subregion)

    if (subregion == "2") {
      # pick coefficients yielding valid tv/gross
      # dbh=20 ht=20 => tv = 400 / (a + b/ht)
      # choose a=200, b=20000 => denom = 200 + 1000 = 1200 => tv ~ 0.3333
      # choose gross polynomial giving something like ~0.8 * tv
      a <- 200
      b <- 20000
      c <- 0.90
      d <- 0
      e <- 0

      # net coefficients: vary by species to hit both branches
      if (identical(species, "PICE.MAR")) {
        # net way too small => should be constrained to 0.95*gross
        nv_a <- 0.10
        nv_b <- 0
        nv_c <- 0
      } else {
        # net too big => should be capped to gross
        nv_a <- 2.00
        nv_b <- 0
        nv_c <- 0
      }

      return(tibble::tibble(
        param_set = "NX242_BS_DISTRICT",
        Species = species,
        Subregion = subregion,
        t = -999,
        a = a,
        b = b,
        c = c,
        d = d,
        e = e,
        nv_a = nv_a,
        nv_b = nv_b,
        nv_c = nv_c
      ))
    }

    tibble::tibble(
      param_set = "NX122_NX67",
      Species = species,
      Subregion = subregion,
      t = 0.176,
      a = 2.184,
      b = 287.181,
      c = 0.9604,
      d = -0.1660,
      e = -0.7868,
      nv_a = -999,
      nv_b = -999,
      nv_c = -999
    )
  }

  mock_standardize_species_code <- function(x, ...) x

  testthat::local_mocked_bindings(
    get_merch_criteria = mock_get_merch_criteria,
    get_volume_params = mock_get_volume_params,
    standardize_species_code = mock_standardize_species_code,
    .env = ns
  )

  # ---- net too small => constrained to 95% of gross ----
  x <- CanadaForestAllometry::vol_NL(
    20,
    20,
    "PICE.MAR",
    subregion = 2,
    keep_net = TRUE
  )

  testthat::expect_true(all(
    c(
      "vol_total",
      "vol_merchantable",
      "vol_merchantable_gross",
      "vol_merchantable_net"
    ) %in%
      names(x)
  ))

  testthat::expect_equal(x$vol_merchantable, x$vol_merchantable_gross)

  testthat::expect_lte(x$vol_merchantable_net, x$vol_merchantable_gross)
  testthat::expect_gte(x$vol_merchantable_net, 0.95 * x$vol_merchantable_gross)

  # ---- net too large => capped to gross ----
  y <- CanadaForestAllometry::vol_NL(
    20,
    20,
    "ABIE.BAL",
    subregion = 2,
    keep_net = TRUE
  )
  testthat::expect_equal(y$vol_merchantable_net, y$vol_merchantable_gross)
})

testthat::test_that("vol_NL NX-122/NX-67 branch clamps negative merch, caps merch > total, and applies minDBH rule", {
  ns <- getNamespace("CanadaForestAllometry")

  mock_get_merch_criteria <- function(jurisdiction, verbose = FALSE, ...) {
    tibble::tibble(
      jurisdiction = "NL",
      species = "ALL",
      stumpht_m = 0.15,
      topdbh_cm = 7.6,
      mindbh_cm = 9
    )
  }

  # Drive NX-122 branch always; vary coeffs by species to hit each clamp/cap.
  mock_get_volume_params <- function(model_id, species, subregion, ...) {
    # Base parameters for total volume (must be valid)
    t <- 0.176
    a <- 2.184
    b <- 287.181

    if (identical(species, "NEG.MERCH")) {
      # Force negative merchantable: c < 0 and d=e=0
      c <- -1
      d <- 0
      e <- 0
    } else if (identical(species, "OVER.MERCH")) {
      # Force merch > total: c > 1 and d=e=0
      c <- 2
      d <- 0
      e <- 0
    } else {
      # Normal-ish
      c <- 0.9604
      d <- -0.1660
      e <- -0.7868
    }

    tibble::tibble(
      param_set = "NX122_NX67",
      Species = species,
      Subregion = as.character(subregion),
      t = t,
      a = a,
      b = b,
      c = c,
      d = d,
      e = e,
      nv_a = -999,
      nv_b = -999,
      nv_c = -999
    )
  }

  # Bypass any internal standardization to keep our fake codes intact
  mock_standardize_species_code <- function(x, ...) x

  testthat::local_mocked_bindings(
    get_merch_criteria = mock_get_merch_criteria,
    get_volume_params = mock_get_volume_params,
    standardize_species_code = mock_standardize_species_code,
    .env = ns
  )

  # ---- negative merch -> clamp to 0 ----
  a <- CanadaForestAllometry::vol_NL(
    20,
    20,
    "NEG.MERCH",
    subregion = "Province"
  )
  testthat::expect_true(a$vol_total > 0)
  testthat::expect_equal(a$vol_merchantable, 0)

  # ---- merch > total -> cap to total ----
  b <- CanadaForestAllometry::vol_NL(
    20,
    20,
    "OVER.MERCH",
    subregion = "Province"
  )
  testthat::expect_true(b$vol_total > 0)
  testthat::expect_equal(b$vol_merchantable, b$vol_total)

  # ---- minDBH rule: below threshold => merch 0 but total still computed ----
  c <- CanadaForestAllometry::vol_NL(
    8,
    20,
    "OVER.MERCH",
    subregion = "Province",
    keep_net = TRUE
  )
  testthat::expect_true(c$vol_total > 0)
  testthat::expect_equal(c$vol_merchantable, 0)
  testthat::expect_equal(c$vol_merchantable_gross, 0)
  testthat::expect_equal(c$vol_merchantable_net, 0)
})

testthat::test_that("vol_NL returns expected columns based on keep_net", {
  ns <- getNamespace("CanadaForestAllometry")

  mock_get_merch_criteria <- function(jurisdiction, verbose = FALSE, ...) {
    tibble::tibble(
      jurisdiction = "NL",
      species = "ALL",
      stumpht_m = 0.15,
      topdbh_cm = 7.6,
      mindbh_cm = 9
    )
  }

  mock_get_volume_params <- function(model_id, species, subregion, ...) {
    tibble::tibble(
      param_set = "NX122_NX67",
      Species = species,
      Subregion = as.character(subregion),
      t = 0.176,
      a = 2.184,
      b = 287.181,
      c = 0.9604,
      d = -0.1660,
      e = -0.7868,
      nv_a = -999,
      nv_b = -999,
      nv_c = -999
    )
  }

  mock_standardize_species_code <- function(x, ...) x

  testthat::local_mocked_bindings(
    get_merch_criteria = mock_get_merch_criteria,
    get_volume_params = mock_get_volume_params,
    standardize_species_code = mock_standardize_species_code,
    .env = ns
  )

  out1 <- CanadaForestAllometry::vol_NL(20, 20, "PICE.MAR", keep_net = FALSE)
  testthat::expect_identical(names(out1), c("vol_total", "vol_merchantable"))

  out2 <- CanadaForestAllometry::vol_NL(20, 20, "PICE.MAR", keep_net = TRUE)
  testthat::expect_true(all(
    c(
      "vol_total",
      "vol_merchantable",
      "vol_merchantable_gross",
      "vol_merchantable_net"
    ) %in%
      names(out2)
  ))
})

testthat::test_that("vol_NL integration smoke test over parameters_volNL (if available)", {
  has_data <- "parameters_volNL" %in%
    data(package = "CanadaForestAllometry")$results[, "Item"]
  testthat::skip_if_not(has_data)

  # load dataset from package data
  data(
    "parameters_volNL",
    package = "CanadaForestAllometry",
    envir = environment()
  )
  testthat::expect_true(exists("parameters_volNL", inherits = FALSE))

  params <- parameters_volNL

  testthat::skip_if_not(is.data.frame(params))
  testthat::skip_if_not(all(c("Species", "Subregion") %in% names(params)))

  cases <- params |>
    dplyr::transmute(
      Species = .data$Species,
      subregion = dplyr::coalesce(as.character(.data$Subregion), "ALL")
    ) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(.data$Species), .data$Species != "") |>
    dplyr::filter(!is.na(.data$subregion), .data$subregion != "")

  cases <- dplyr::slice_head(cases, n = min(nrow(cases), 50))

  out <- purrr::pmap_dfr(
    list(cases$Species, cases$subregion),
    function(sp, sr) {
      CanadaForestAllometry::vol_NL(
        DBH = 20,
        height = 20,
        species = sp,
        subregion = sr,
        keep_net = TRUE
      ) |>
        dplyr::mutate(Species = sp, subregion = sr)
    }
  )

  testthat::expect_true(all(is.finite(out$vol_total)))
  testthat::expect_true(all(out$vol_total >= 0))
  testthat::expect_true(all(is.finite(out$vol_merchantable)))
  testthat::expect_true(all(out$vol_merchantable >= 0))
})
