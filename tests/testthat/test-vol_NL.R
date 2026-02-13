# tests/testthat/test-vol_NL.R

# ---- helpers ---------------------------------------------------------------

.local_internal <- function(obj_name) {
  ns <- asNamespace("CanadaForestAllometry")
  if (!exists(obj_name, envir = ns, inherits = FALSE)) {
    testthat::skip(paste0(
      "Internal object `",
      obj_name,
      "` not found in namespace. ",
      "Is it saved to R/sysdata.rda?"
    ))
  }
  get(obj_name, envir = ns, inherits = FALSE)
}

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

.pick_merchcrit_nl <- function() {
  mc <- dplyr::as_tibble(.local_data("merchcrit"))

  # support either naming convention
  if (all(c("Province", "Species", "MinDBH") %in% names(mc))) {
    out <- mc |>
      dplyr::filter(
        .data$Province == "NL",
        .data$Species %in% c("ALL", "UNKN.SPP")
      ) |>
      dplyr::slice(1)

    if (nrow(out) == 0) {
      testthat::skip("No NL rows found in merchcrit.")
    }
    return(list(mindbh_cm = out$MinDBH[[1]]))
  }

  if (all(c("province", "species", "mindbh_cm") %in% names(mc))) {
    out <- mc |>
      dplyr::filter(
        .data$province == "NL",
        .data$species %in% c("ALL", "UNKN.SPP")
      ) |>
      dplyr::slice(1)

    if (nrow(out) == 0) {
      testthat::skip("No NL rows found in merchcrit.")
    }
    return(list(mindbh_cm = out$mindbh_cm[[1]]))
  }

  testthat::skip(
    "merchcrit does not have expected columns (Province/Species/MinDBH or province/species/mindbh_cm)."
  )
}

# ---- tests -----------------------------------------------------------------

testthat::test_that("vol_NL validates lengths and subregion inputs", {
  testthat::expect_error(
    CanadaForestAllometry::vol_NL(
      DBH = c(10, 20),
      height = 20,
      species = "PICE.MAR"
    ),
    "must have the same length",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::vol_NL(20, 20, "PICE.MAR", subregion = 25),
    "Invalid NL subregion",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::vol_NL(20, 20, "PICE.MAR", subregion = "foo"),
    "Invalid NL subregion",
    ignore.case = TRUE
  )

  testthat::expect_error(
    CanadaForestAllometry::vol_NL(20, 20, "PICE.MAR", subregion = NA),
    "Invalid NL subregion",
    ignore.case = TRUE
  )
})

testthat::test_that("vol_NL returns tibble with expected columns and vectorizes", {
  params <- dplyr::as_tibble(.local_internal("parameters_volNL"))
  testthat::expect_true(all(c("Species", "Subregion") %in% names(params)))

  # Choose a species that appears in province-wide (ALL) rows if possible
  sp_all <- params |>
    dplyr::filter(.data$Subregion == "ALL") |>
    dplyr::distinct(.data$Species) |>
    dplyr::slice(1) |>
    dplyr::pull(.data$Species)

  if (length(sp_all) == 0 || is.na(sp_all)) {
    # fallback: any species at all
    sp_all <- params |>
      dplyr::distinct(.data$Species) |>
      dplyr::slice(1) |>
      dplyr::pull(.data$Species)
  }

  testthat::skip_if_not(
    length(sp_all) == 1 && is.character(sp_all) && nzchar(sp_all)
  )

  out <- CanadaForestAllometry::vol_NL(
    DBH = c(10, 20, 30),
    height = c(12, 18, 24),
    species = rep(sp_all, 3),
    subregion = "ALL"
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, c("vol_total", "vol_merchantable"))
  testthat::expect_equal(nrow(out), 3L)

  testthat::expect_true(all(is.finite(out$vol_total)))
  testthat::expect_true(all(is.finite(out$vol_merchantable)))
  testthat::expect_true(all(out$vol_total >= 0))
  testthat::expect_true(all(out$vol_merchantable >= 0))
  testthat::expect_true(all(out$vol_merchantable <= out$vol_total))
})

testthat::test_that("vol_NL keep_net=TRUE adds net columns and respects constraints", {
  params <- dplyr::as_tibble(.local_internal("parameters_volNL"))

  # Find a NX242 district row (these have district-specific param_set)
  # and with usable net coefficients (nv_* not all -999)
  nx242 <- params |>
    dplyr::filter(grepl("^NX242", .data$param_set)) |>
    dplyr::filter(.data$Subregion != "ALL") |>
    dplyr::filter(!is.na(.data$nv_a), !is.na(.data$nv_b), !is.na(.data$nv_c)) |>
    dplyr::filter(
      !(.data$nv_a == -999 & .data$nv_b == -999 & .data$nv_c == -999)
    ) |>
    dplyr::slice(1)

  testthat::skip_if_not(nrow(nx242) == 1)

  sp <- nx242$Species[[1]]
  sr <- nx242$Subregion[[1]]

  out <- CanadaForestAllometry::vol_NL(
    DBH = 20,
    height = 20,
    species = sp,
    subregion = sr,
    keep_net = TRUE
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(all(
    c(
      "vol_total",
      "vol_merchantable",
      "vol_merchantable_gross",
      "vol_merchantable_net"
    ) %in%
      names(out)
  ))

  testthat::expect_true(is.finite(out$vol_total[[1]]))
  testthat::expect_true(is.finite(out$vol_merchantable[[1]]))
  testthat::expect_true(is.finite(out$vol_merchantable_gross[[1]]))
  testthat::expect_true(is.finite(out$vol_merchantable_net[[1]]))

  testthat::expect_gte(out$vol_total[[1]], 0)
  testthat::expect_gte(out$vol_merchantable[[1]], 0)
  testthat::expect_gte(out$vol_merchantable_gross[[1]], 0)
  testthat::expect_gte(out$vol_merchantable_net[[1]], 0)

  # gross is the merchantable volume returned (your API choice)
  testthat::expect_equal(
    out$vol_merchantable[[1]],
    out$vol_merchantable_gross[[1]]
  )

  # net constraints implemented in vol_NL:
  #   net <= gross
  #   net >= 0.95 * gross
  testthat::expect_lte(
    out$vol_merchantable_net[[1]],
    out$vol_merchantable_gross[[1]]
  )
  testthat::expect_gte(
    out$vol_merchantable_net[[1]],
    0.95 * out$vol_merchantable_gross[[1]]
  )
})

testthat::test_that("vol_NL applies NL minDBH rule: below -> merch 0, total still computed", {
  params <- dplyr::as_tibble(.local_internal("parameters_volNL"))
  mc_nl <- .pick_merchcrit_nl()
  mindbh_cm <- mc_nl$mindbh_cm

  testthat::skip_if_not(is.numeric(mindbh_cm) && is.finite(mindbh_cm))

  # Pick a species that exists in NL params (province-wide preferred)
  sp <- params |>
    dplyr::filter(.data$Subregion == "ALL") |>
    dplyr::distinct(.data$Species) |>
    dplyr::slice(1) |>
    dplyr::pull(.data$Species)

  if (length(sp) == 0 || is.na(sp)) {
    sp <- params |>
      dplyr::distinct(.data$Species) |>
      dplyr::slice(1) |>
      dplyr::pull(.data$Species)
  }

  testthat::skip_if_not(length(sp) == 1 && is.character(sp) && nzchar(sp))

  # DBH just below min => merchantable forced 0
  DBH_small <- max(0.1, mindbh_cm - 0.1)

  out <- CanadaForestAllometry::vol_NL(
    DBH = DBH_small,
    height = 20,
    species = sp,
    subregion = "ALL",
    keep_net = TRUE
  )

  testthat::expect_true(is.finite(out$vol_total[[1]]))
  testthat::expect_true(out$vol_total[[1]] >= 0)

  testthat::expect_equal(out$vol_merchantable[[1]], 0)
  testthat::expect_equal(out$vol_merchantable_gross[[1]], 0)
  testthat::expect_equal(out$vol_merchantable_net[[1]], 0)
})

testthat::test_that("vol_NL works across many Species/Subregion combinations (smoke test)", {
  params <- dplyr::as_tibble(.local_internal("parameters_volNL"))
  testthat::expect_true(all(c("Species", "Subregion") %in% names(params)))

  cases <- params |>
    dplyr::transmute(
      Species = .data$Species,
      subregion = dplyr::coalesce(as.character(.data$Subregion), "ALL")
    ) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(.data$Species), .data$Species != "") |>
    dplyr::filter(!is.na(.data$subregion), .data$subregion != "")

  testthat::skip_if_not(nrow(cases) > 0)

  # limit runtime
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

  testthat::expect_equal(nrow(out), nrow(cases))
  testthat::expect_true(all(is.finite(out$vol_total)))
  testthat::expect_true(all(out$vol_total >= 0))
  testthat::expect_true(all(is.finite(out$vol_merchantable)))
  testthat::expect_true(all(out$vol_merchantable >= 0))
  testthat::expect_true(all(out$vol_merchantable <= out$vol_total))

  # When keep_net=TRUE, these should exist and be finite/non-negative
  testthat::expect_true(all(is.finite(out$vol_merchantable_gross)))
  testthat::expect_true(all(out$vol_merchantable_gross >= 0))
  testthat::expect_true(all(is.finite(out$vol_merchantable_net)))
  testthat::expect_true(all(out$vol_merchantable_net >= 0))

  testthat::expect_true(all(
    out$vol_merchantable_net <= out$vol_merchantable_gross
  ))
})
