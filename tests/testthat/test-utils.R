testthat::test_that("supported_provinces returns sorted unique province codes", {
  out <- CanadaForestAllometry:::supported_provinces()
  testthat::expect_true(is.character(out))
  testthat::expect_identical(out, sort(unique(out)))
  testthat::expect_true(all(c("AB", "BC", "ON", "QC") %in% out))
})

testthat::test_that("standardize_jurisdiction_code maps aliases and errors on unknown codes", {
  mapped <- CanadaForestAllometry:::standardize_jurisdiction_code(c(" pei ", "nwt", "pq"))
  testthat::expect_identical(mapped, c("PE", "NT", "QC"))

  testthat::expect_error(
    CanadaForestAllometry:::standardize_jurisdiction_code(c("ON", "NOTREAL")),
    "Unknown province/territory codes found",
    fixed = FALSE
  )
})

testthat::test_that("normalize_province_scope handles ALL and explicit vectors", {
  all_scope <- CanadaForestAllometry:::normalize_province_scope(list("ALL"))
  testthat::expect_identical(
    all_scope,
    CanadaForestAllometry:::supported_provinces()
  )

  explicit <- CanadaForestAllometry:::normalize_province_scope(list(c("AB", "AB", "BC")))
  testthat::expect_identical(explicit, c("AB", "BC"))
})

testthat::test_that("select_volume_models filters registry by jurisdiction and sorts by rank", {
  out <- CanadaForestAllometry:::select_volume_models("AB")
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(nrow(out) > 0)
  testthat::expect_true(all(diff(out$rank) >= 0))
  testthat::expect_true(all(vapply(out$supported, isTRUE, logical(1))))
})

testthat::test_that("standardize_ecozone returns NULL for NULL input", {
  testthat::expect_null(CanadaForestAllometry:::standardize_ecozone(NULL))
})

testthat::test_that(".get_internal_data errors clearly for missing internal object", {
  testthat::expect_error(
    CanadaForestAllometry:::.get_internal_data("definitely_not_an_internal_dataset"),
    "Internal data object `definitely_not_an_internal_dataset` not found in namespace",
    fixed = TRUE
  )
})
