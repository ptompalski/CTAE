# tests/testthat/test-<fn_name>.R
# testthat 3e. Adapt to the model's inputs/outputs.
# Target 100% coverage: exercise every branch below — each error path, each
# species/variant, and both prediction directions. Check with
# covr::file_coverage("R/<fn_name>.R", "tests/testthat/test-<fn_name>.R").

test_that("<fn_name> returns a well-formed tibble", {
  out <- <fn_name>(<minimal valid inputs>)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c(<expected cols>) %in% names(out)))
  expect_equal(nrow(out), <expected n>)
})

test_that("<fn_name> recycles inputs to a common length", {
  out <- <fn_name>(<scalar + vector inputs>)
  expect_equal(nrow(out), <length of longest input>)
})

test_that("<fn_name> errors on incompatible input lengths", {
  expect_error(<fn_name>(<length-2 and length-3 inputs>))
})

test_that("<fn_name> validates inputs", {
  # e.g. exactly one of height/si; non-finite / out-of-domain inputs
  expect_error(<fn_name>(<invalid inputs>))
})

# --- Validation tier: choose ONE and label it clearly ---

# TIER 1 (fidelity) -- use when the source gives reference values.
test_that("<fn_name> matches published reference values (Author Year, Table X)", {
  ref <- readr::read_csv(
    testthat::test_path("..", "..", "tmp", "<fn_name>_comparison_values.csv"),
    show_col_types = FALSE
  )
  # recompute and compare to the source's published outputs
  expect_equal(<computed>, <ref$expected>, tolerance = 1e-3)
})

# TIER 2 (plausibility) -- ONLY when no source reference values exist.
# This is a SANITY CHECK, not proof of fidelity. Flag the model "no source
# benchmark" in the model spec.
test_that("<fn_name> is plausible vs. existing same-family models (sanity check)", {
  new <- <fn_name>(<inputs>)
  ref <- <existing_same_family_fn>(<comparable inputs>)
  # similar magnitude, not identical
  expect_true(all(abs(new$<col> - ref$<col>) / ref$<col> < 0.25))
  # monotonic behavior expected by the model
  # expect_true(all(diff(<fn_name>(age = sort(ages), ...)$<col>) >= 0))
})

# --- Cross-check vs. existing similar functions: ALWAYS include, even under Tier 1.
# This complements the fidelity test; report any discrepancies to the user.
test_that("<fn_name> is consistent with existing same-family models", {
  new <- <fn_name>(<comparable inputs>)
  ref <- <existing_same_family_fn>(<comparable inputs>)
  expect_true(all(abs(new$<col> - ref$<col>) / ref$<col> < 0.25))
})
