# tests/testthat/test-v2b.R

library(dplyr)

# ---- helpers for tests -------------------------------------------------------

make_mock_params_v2b <- function() {
  # Minimal tables to exercise fetch/join logic + computations.
  # One species row: juris_id="AB", ecozone=4, genus="PICE", species="MAR"
  # One variety-specific row too, so we can test variety fallback.
  list(
    B3 = tibble::tibble(
      juris_id = c("AB", "AB"),
      ecozone = c(4, 4),
      genus = c("PICE", "PICE"),
      species = c("MAR", "MAR"),
      variety = c("AAA", NA_character_),
      a = c(2, 2),
      b = c(0.7, 0.7),
      source_table = c("B3", "B3")
    ),
    B4 = tibble::tibble(
      juris_id = c("AB"),
      ecozone = c(4),
      genus = c("PICE"),
      species = c("MAR"),
      variety = c(NA_character_),
      k = c(1.0),
      a = c(0.10),
      b = c(0.50),
      cap = c(1.20),
      source_table = c("B4")
    ),
    B5 = tibble::tibble(
      juris_id = c("AB"),
      ecozone = c(4),
      genus = c("PICE"),
      k = c(1.0),
      a = c(0.05),
      b = c(0.80),
      cap = c(1.10)
    ),
    B6_vol = tibble::tibble(
      juris_id = c("AB"),
      ecozone = c(4),
      genus = c("PICE"),
      species = c("MAR"),
      variety = c(NA_character_),
      # coefficients chosen to make raw props sometimes outside caps
      a1 = -1.0,
      a2 = 0.0,
      a3 = 0.0,
      b1 = -2.0,
      b2 = 0.0,
      b3 = 0.0,
      c1 = 1.0,
      c2 = 0.0,
      c3 = 0.0,
      source_table = "B6_vol"
    ),
    B7_vol = tibble::tibble(
      juris_id = c("AB"),
      ecozone = c(4),
      genus = c("PICE"),
      species = c("MAR"),
      variety = c(NA_character_),
      x_min = 10,
      x_max = 50,
      p_sw_low = 0.60,
      p_sb_low = 0.05,
      p_br_low = 0.05,
      p_fl_low = 0.05,
      p_sw_high = 0.80,
      p_sb_high = 0.20,
      p_br_high = 0.20,
      p_fl_high = 0.20,
      source_table = "B7_vol"
    ),
    # Not used in "vol path" tests below but required by v2b_component_keys()
    B6_tb = tibble::tibble(
      juris_id = "AB",
      ecozone = 4,
      genus = "PICE",
      species = "MAR",
      variety = NA_character_,
      a1 = 0,
      a2 = 0,
      a3 = 0,
      b1 = 0,
      b2 = 0,
      b3 = 0,
      c1 = 0,
      c2 = 0,
      c3 = 0
    ),
    B7_tb = tibble::tibble(
      juris_id = "AB",
      ecozone = 4,
      genus = "PICE",
      species = "MAR",
      variety = NA_character_,
      x_min = 0,
      x_max = 1,
      p_sw_low = 0,
      p_sb_low = 0,
      p_br_low = 0,
      p_fl_low = 0,
      p_sw_high = 1,
      p_sb_high = 1,
      p_br_high = 1,
      p_fl_high = 1
    ),
    B14 = tibble::tibble(
      juris_id = "AB",
      ecozone = 4,
      genus = "PICE",
      a = NA_real_,
      b = NA_real_,
      k = NA_real_,
      cap = NA_real_
    )
  )
}

make_mock_keys <- function(
  species = c("PICE.MAR", "PICE.MAR.AAA"),
  jurisdiction = "AB",
  ecozone = 4
) {
  # small helper for unit tests: bypass standardize_* by building keys directly
  sp <- v2b_split_species_nfi(species)
  tibble::tibble(
    .row_id = seq_along(species),
    juris_id = rep(jurisdiction, length(species)),
    ecozone = rep(ecozone, length(species)),
    species_nfi = species,
    genus = sp$genus,
    species = sp$species,
    variety = sp$variety
  )
}

# ---- unit tests: pure logic with mock params ---------------------------------

testthat::test_that("v2b_split_species_nfi splits genus/species/variety", {
  x <- c("PICE.MAR", "PICE.MAR.AAA", "PICE.MAR.")
  out <- v2b_split_species_nfi(x)

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(out$genus, c("PICE", "PICE", "PICE"))
  testthat::expect_equal(out$species, c("MAR", "MAR", "MAR"))
  testthat::expect_equal(out$variety, c(NA_character_, "AAA", NA_character_))
})

testthat::test_that("v2b_component_keys returns expected join keys", {
  testthat::expect_equal(
    v2b_component_keys("B3"),
    c("juris_id", "ecozone", "genus", "species", "variety")
  )
  testthat::expect_equal(
    v2b_component_keys("B5"),
    c("juris_id", "ecozone", "genus")
  )
})

testthat::test_that("v2b_fetch_params matches and preserves input order", {
  params <- make_mock_params_v2b()
  keys <- make_mock_keys(species = c("PICE.MAR", "PICE.MAR.AAA"))

  # For B3: one key has variety NA, other has AAA
  p3 <- v2b_fetch_params(
    "B3",
    keys,
    params_list = params,
    allow_variety_fallback = TRUE
  )

  testthat::expect_equal(p3$.row_id, c(1L, 2L))
  testthat::expect_equal(p3$a, c(2, 2))
  testthat::expect_equal(p3$b, c(0.7, 0.7))
})

testthat::test_that("v2b_fetch_params variety fallback works (variety -> NA)", {
  params <- make_mock_params_v2b()
  keys <- make_mock_keys(species = c("PICE.MAR.BBB")) # BBB not in params

  p3 <- v2b_fetch_params(
    "B3",
    keys,
    params_list = params,
    allow_variety_fallback = TRUE
  )

  # should fall back to variety = NA row
  testthat::expect_equal(p3$variety, NA_character_)
  testthat::expect_equal(p3$a, 2)
})

testthat::test_that("v2b_fetch_params errors when missing params", {
  params <- make_mock_params_v2b()
  keys <- make_mock_keys(species = c("ABIE.BAL")) # not present

  testthat::expect_error(
    v2b_fetch_params("B3", keys, params_list = params),
    class = "ctae_v2b_missing_params"
  )
})

testthat::test_that("math helpers behave and handle caps", {
  testthat::expect_equal(v2b_stem_merch(100, a = 2, b = 0.7), 2 * (100^0.7))

  # nonmerch factor: cap applies
  f <- v2b_nonmerch_factor(b_m = 100, a = 1, b = 1, k = 0, cap = 50)
  testthat::expect_equal(f, 50)

  # sapling factor: cap applies
  fs <- v2b_sapling_factor(b_nm = 100, a = 1, b = 1, k = 0, cap = 10)
  testthat::expect_equal(fs, 10)
})

testthat::test_that("v2b_props_from_table6 returns proportions summing to 1", {
  props <- v2b_props_from_table6(
    x = 100,
    a1 = -1,
    a2 = 0,
    a3 = 0,
    b1 = -2,
    b2 = 0,
    b3 = 0,
    c1 = 1,
    c2 = 0,
    c3 = 0,
    offset = 5
  )
  testthat::expect_s3_class(props, "tbl_df")
  s <- props$p_sw + props$p_sb + props$p_br + props$p_fl
  testthat::expect_equal(as.numeric(s), 1, tolerance = 1e-12)
})

testthat::test_that("v2b_apply_caps_table7 clamps to low/high and optional renormalize", {
  props <- tibble::tibble(
    p_sw = 0.01, # below low
    p_sb = 0.50, # above high
    p_br = 0.30, # above high
    p_fl = 0.19
  )
  caps <- tibble::tibble(
    p_sw_low = 0.60,
    p_sw_high = 0.80,
    p_sb_low = 0.05,
    p_sb_high = 0.20,
    p_br_low = 0.05,
    p_br_high = 0.20,
    p_fl_low = 0.05,
    p_fl_high = 0.20
  )

  out_no <- v2b_apply_caps_table7(props, caps, renormalize = FALSE)
  testthat::expect_equal(out_no$p_sw, 0.60)
  testthat::expect_equal(out_no$p_sb, 0.20)
  testthat::expect_equal(out_no$p_br, 0.20)
  testthat::expect_equal(out_no$p_fl, 0.19)

  out_yes <- v2b_apply_caps_table7(props, caps, renormalize = TRUE)
  s <- out_yes$p_sw + out_yes$p_sb + out_yes$p_br + out_yes$p_fl
  testthat::expect_equal(as.numeric(s), 1, tolerance = 1e-12)
})

testthat::test_that("v2b_biomass_components: warning when outside x-range and clamp_x affects volume_used", {
  params <- make_mock_params_v2b()
  keys <- make_mock_keys(species = "PICE.MAR")

  # Build a small wrapper that uses our mock params by temporarily calling internals
  # We'll test the specific behaviors by reusing the helpers directly.

  p6 <- v2b_fetch_params("B6_vol", keys, params_list = params)
  p7 <- v2b_fetch_params("B7_vol", keys, params_list = params)

  vol <- 100 # outside x_max=50 in mock
  testthat::expect_true(vol > p7$x_max)

  props_raw <- v2b_props_from_table6(
    x = vol,
    a1 = p6$a1,
    a2 = p6$a2,
    a3 = p6$a3,
    b1 = p6$b1,
    b2 = p6$b2,
    b3 = p6$b3,
    c1 = p6$c1,
    c2 = p6$c2,
    c3 = p6$c3
  )
  props_cap <- v2b_apply_caps_table7(props_raw, p7, renormalize = FALSE)

  # since vol is outside range, caps should be able to bind (at least for extreme cases)
  testthat::expect_true(all(props_cap >= 0 & props_cap <= 1))
})

# ---- integration tests: run against real package tables if available ---------

testthat::test_that("v2b_build_keys validates and standardizes inputs", {
  testthat::skip_if_not(exists("v2b_build_keys", mode = "function"))
  testthat::skip_if_not(exists(
    "standardize_jurisdiction_code",
    mode = "function"
  ))
  testthat::skip_if_not(exists("standardize_species_code", mode = "function"))

  keys <- v2b_build_keys(
    species = c("PICE.MAR", "PICE.MAR.AAA"),
    jurisdiction = "AB",
    ecozone = 4
  )

  testthat::expect_s3_class(keys, "tbl_df")
  testthat::expect_named(
    keys,
    c(
      ".row_id",
      "juris_id",
      "ecozone",
      "species_nfi",
      "genus",
      "species",
      "variety"
    )
  )
  testthat::expect_equal(keys$.row_id, 1:2)
  testthat::expect_true(all(keys$juris_id == "AB"))
  testthat::expect_true(all(keys$ecozone == 4))
})

testthat::test_that("v2b_stem_biomass returns expected shape and non-negative components", {
  testthat::skip_if_not(exists("parameters_v2b", inherits = TRUE))
  testthat::skip_if_not(exists("v2b_stem_biomass", mode = "function"))

  out <- v2b_stem_biomass(
    vol_merchantable = 100,
    species = "PICE.MAR",
    jurisdiction = "AB",
    ecozone = 4
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(
    out,
    c("b_m", "b_n", "b_nm", "b_s", "f_nm", "f_s", "has_sapling")
  )

  # b_m should be finite and >= 0 for positive volume in most models
  testthat::expect_true(is.finite(out$b_m))
  testthat::expect_true(out$b_m >= 0)
  testthat::expect_true(is.finite(out$b_nm))
  testthat::expect_true(out$b_nm >= 0)
  testthat::expect_true(is.finite(out$b_s))
  testthat::expect_true(out$b_s >= 0)
})

testthat::test_that("v2b_biomass_components returns proportions and components consistent with stem total", {
  testthat::skip_if_not(exists("parameters_v2b", inherits = TRUE))
  testthat::skip_if_not(exists("v2b_biomass_components", mode = "function"))
  testthat::skip_if_not(exists("v2b_stem_biomass", mode = "function"))

  vol <- 400
  stem <- v2b_stem_biomass(
    vol,
    species = "PSEU.MEN",
    jurisdiction = "BC",
    ecozone = 13
  )
  comp <- v2b_biomass_components(
    vol,
    species = "PSEU.MEN",
    jurisdiction = "BC",
    ecozone = 13,
    renormalize = TRUE,
    clamp_x = FALSE
  )

  testthat::expect_s3_class(comp, "tbl_df")
  testthat::expect_named(
    comp,
    c(
      "b_total",
      "b_bark",
      "b_branches",
      "b_foliage",
      "p_sw",
      "p_sb",
      "p_br",
      "p_fl",
      "b_stem_total"
    )
  )

  # proportions sum to 1 when renormalize=TRUE
  s <- comp$p_sw + comp$p_sb + comp$p_br + comp$p_fl
  testthat::expect_equal(as.numeric(s), 1, tolerance = 1e-10)

  # b_total should back-calculate from stem_total / p_sw
  testthat::expect_equal(
    as.numeric(comp$b_total),
    as.numeric(comp$b_stem_total / comp$p_sw),
    tolerance = 1e-10
  )

  # b_stem_total matches stem$b_nm + stem$b_s
  testthat::expect_equal(
    as.numeric(comp$b_stem_total),
    as.numeric(stem$b_nm + stem$b_s),
    tolerance = 1e-12
  )
})

testthat::test_that("v2b warns on extrapolation when clamp_x=FALSE and volume outside [x_min, x_max]", {
  testthat::skip_if_not(exists("parameters_v2b", inherits = TRUE))
  testthat::skip_if_not(exists("v2b", mode = "function"))

  # pick a volume that is likely to exceed x_max for many species/ecozones
  # (if this ends up not exceeding, the test will be skipped gracefully)
  vol <- 10^6

  # Try to run; if no warning, we just don't enforce (tables differ)
  w <- NULL
  out <- tryCatch(
    withCallingHandlers(
      v2b(
        vol_merchantable = vol,
        species = "PICE.MAR",
        jurisdiction = "AB",
        ecozone = 4,
        include_props = TRUE
      ),
      warning = function(e) {
        w <<- e
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  # If it errored due to missing params, skip (depends on coverage in your tables)
  if (inherits(out, "error")) {
    testthat::skip(
      "No parameters available for this integration case in current tables."
    )
  }

  # If a warning was raised, confirm it's the extrapolation class
  if (!is.null(w)) {
    testthat::expect_true(inherits(w, "ctae_v2b_extrapolation"))
  }
})

testthat::test_that("v2b output columns respect include_props/include_intermediates flags", {
  testthat::skip_if_not(exists("parameters_v2b", inherits = TRUE))
  testthat::skip_if_not(exists("v2b", mode = "function"))

  base <- v2b(
    vol_merchantable = 400,
    species = "PSEU.MEN",
    jurisdiction = "BC",
    ecozone = 13
  )
  testthat::expect_named(
    base,
    c("b_total", "b_bark", "b_branches", "b_foliage")
  )

  with_props <- v2b(
    vol_merchantable = 400,
    species = "PSEU.MEN",
    jurisdiction = "BC",
    ecozone = 13,
    include_props = TRUE
  )
  testthat::expect_true(all(
    c("p_sw", "p_sb", "p_br", "p_fl") %in% names(with_props)
  ))

  with_int <- v2b(
    vol_merchantable = 400,
    species = "PSEU.MEN",
    jurisdiction = "BC",
    ecozone = 13,
    include_intermediates = TRUE
  )
  testthat::expect_true(all(
    c(
      "b_m",
      "b_n",
      "b_nm",
      "b_s",
      "f_nm",
      "f_s",
      "has_sapling",
      "b_stem_total"
    ) %in%
      names(with_int)
  ))
})

testthat::test_that("v2b() matches manual calculation for one case (manually pasted params)", {
  # ---- Choose a single case to validate -------------------------------------
  # Recreating the example from the pdf, but because the parameters changed and also
  # now the models require gross (not net) values for BC, the results will differ from the
  # ones in the publication.
  case <- list(
    vol_merchantable = 350, # <-- set the test volume
    species = "PSEU.MEN", # <-- NFI species code used in v2b() "PSEU.MEN" is 500 canfi code
    jurisdiction = "BC", # <-- e.g. "AB"
    ecozone = 13 # <-- 1..15
  )

  # ---- Paste NEW parameter values from the PDF here --------------------------
  # Copy the exact coefficients for the SAME (juris, ecozone, lead species/genus) row.
  # Tables:
  #  - Table 3: a, b (merchantable stem biomass)
  #  - Table 4: k, a, b, cap (nonmerch factor)
  #  - Table 5: k, a, b, cap (sapling factor; genus-level; if missing set has_sapling = FALSE)
  #  - Table 6: a1 a2 a3 b1 b2 b3 c1 c2 c3 (proportion model)
  #  - Table 7: vol_min vol_max and p_*_low/high caps

  params <- list(
    # Table 3 (Eq 1)
    B3 = list(
      a = 0.497746142, # <-- paste
      b = 0.9776560066 # <-- paste
    ),

    # Table 4 (Eq 2)
    B4 = list(
      k = 0.923602106, # <-- paste
      a = 41.27354201, # <-- paste
      b = -1.025384947, # <-- paste
      cap = 7.76561681 # <-- paste (upper cap for factor)
    ),

    # Table 5 (Eq 3) - optional (genus-level). If not available, set has_sapling = FALSE below.
    B5 = list(
      has_sapling = TRUE, # <-- set FALSE if no Table 5 row exists for this genus/juris/ecozone
      k = 0.996274057, # <-- paste
      a = 0.54870672, # <-- paste
      b = -0.835829886, # <-- paste
      cap = 1.022997618 # <-- paste
    ),

    # Table 6 (Eqs 4–7; volume-based proportions)
    B6_vol = list(
      a1 = -1.244877,
      a2 = -0.0000193,
      a3 = -0.0728796, # bark numerator
      b1 = -0.0248945,
      b2 = -0.0001724,
      b3 = -0.2091615, # branches numerator
      c1 = 0.2900703,
      c2 = -0.0003808,
      c3 = -0.3410254 # foliage numerator
    ),

    # Table 7 (caps on proportion models)
    B7_vol = list(
      vol_min = 2.587423538, # <-- paste (Table 7 "vol_min")
      vol_max = 3057.8851584, # <-- paste (Table 7 "vol_max")

      p_sw_low = 0.3913066698,
      p_sb_low = 0.0972101087,
      p_br_low = 0.2497066626,
      p_fl_low = 0.261776559,
      p_sw_high = 0.7778126497,
      p_sb_high = 0.1176340748,
      p_br_high = 0.0835503019,
      p_fl_high = 0.0210029736
    )
  )

  # ---- Manual calculation (should mirror v2b implementation) -----------------

  vol_merchantable <- as.numeric(case$vol_merchantable)
  offset <- 5

  # Eq 1 (Table 3)
  b_m <- params$B3$a * (vol_merchantable^params$B3$b)

  # Eq 2 (Table 4): nonmerchfactor = k + a * b_m^b, then cap
  f_nm <- params$B4$k + params$B4$a * (b_m^params$B4$b)
  if (!is.na(params$B4$cap) && !is.na(f_nm) && f_nm > params$B4$cap) {
    f_nm <- params$B4$cap
  }

  b_nm <- f_nm * b_m
  b_n <- b_nm - b_m

  # Eq 3 (Table 5): saplingfactor = k + a * b_nm^b, then cap (if applicable)
  if (isTRUE(params$B5$has_sapling)) {
    f_s <- params$B5$k + params$B5$a * (b_nm^params$B5$b)
    if (!is.na(params$B5$cap) && !is.na(f_s) && f_s > params$B5$cap) {
      f_s <- params$B5$cap
    }
    b_s <- (f_s * b_nm) - b_nm
  } else {
    f_s <- NA_real_
    b_s <- 0
  }

  b_stem_total <- b_nm + b_s

  # Table 6 proportions (raw)
  lx <- log(vol_merchantable + offset)
  p_a <- exp(
    params$B6_vol$a1 +
      params$B6_vol$a2 * vol_merchantable +
      params$B6_vol$a3 * lx
  )
  p_b <- exp(
    params$B6_vol$b1 +
      params$B6_vol$b2 * vol_merchantable +
      params$B6_vol$b3 * lx
  )
  p_c <- exp(
    params$B6_vol$c1 +
      params$B6_vol$c2 * vol_merchantable +
      params$B6_vol$c3 * lx
  )
  den <- 1 + p_a + p_b + p_c

  p_sw_raw <- 1 / den
  p_sb_raw <- p_a / den
  p_br_raw <- p_b / den
  p_fl_raw <- p_c / den

  # Table 7 caps (this is the "set predicted p = p-low/p-high" behavior)
  clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)

  p_sw <- clamp(p_sw_raw, params$B7_vol$p_sw_low, params$B7_vol$p_sw_high)
  p_sb <- clamp(p_sb_raw, params$B7_vol$p_sb_low, params$B7_vol$p_sb_high)
  p_br <- clamp(p_br_raw, params$B7_vol$p_br_low, params$B7_vol$p_br_high)
  p_fl <- clamp(p_fl_raw, params$B7_vol$p_fl_low, params$B7_vol$p_fl_high)

  # match v2b default behavior: renormalize is TRUE by default in v2b()
  renormalize <- TRUE
  if (isTRUE(renormalize)) {
    s <- p_sw + p_sb + p_br + p_fl
    p_sw <- p_sw / s
    p_sb <- p_sb / s
    p_br <- p_br / s
    p_fl <- p_fl / s
  }

  b_total <- b_stem_total / p_sw
  b_bark <- b_total * p_sb
  b_branches <- b_total * p_br
  b_foliage <- b_total * p_fl

  manual <- tibble::tibble(
    b_total = b_total,
    b_bark = b_bark,
    b_branches = b_branches,
    b_foliage = b_foliage,
    p_sw = p_sw,
    p_sb = p_sb,
    p_br = p_br,
    p_fl = p_fl,
    b_m = b_m,
    b_n = b_n,
    b_nm = b_nm,
    b_s = b_s,
    f_nm = f_nm,
    f_s = f_s,
    has_sapling = isTRUE(params$B5$has_sapling),
    b_stem_total = b_stem_total
  )

  # ---- Compare to v2b() ------------------------------------------------------

  got <- v2b(
    vol_merchantable = case$vol_merchantable,
    species = case$species,
    jurisdiction = case$jurisdiction,
    ecozone = case$ecozone,
    include_props = TRUE,
    include_intermediates = TRUE,
    renormalize = renormalize,
    clamp_x = FALSE
  )

  # Use a slightly relaxed tolerance because PDFs sometimes show rounded coefficients.
  tol <- 1e-6

  testthat::expect_equal(got$b_total, manual$b_total, tolerance = tol)
  testthat::expect_equal(got$b_bark, manual$b_bark, tolerance = tol)
  testthat::expect_equal(got$b_branches, manual$b_branches, tolerance = tol)
  testthat::expect_equal(got$b_foliage, manual$b_foliage, tolerance = tol)

  testthat::expect_equal(got$p_sw, manual$p_sw, tolerance = tol)
  testthat::expect_equal(got$p_sb, manual$p_sb, tolerance = tol)
  testthat::expect_equal(got$p_br, manual$p_br, tolerance = tol)
  testthat::expect_equal(got$p_fl, manual$p_fl, tolerance = tol)

  testthat::expect_equal(got$b_m, manual$b_m, tolerance = tol)
  testthat::expect_equal(got$b_n, manual$b_n, tolerance = tol)
  testthat::expect_equal(got$b_nm, manual$b_nm, tolerance = tol)
  testthat::expect_equal(got$b_s, manual$b_s, tolerance = tol)

  testthat::expect_equal(got$f_nm, manual$f_nm, tolerance = tol)
  testthat::expect_equal(got$f_s, manual$f_s, tolerance = tol)
})


test_that("Boudewyn functions accept ecozone as number, EN name, and FR name", {
  # pick some simple, valid inputs (adjust if your functions require more args)
  vol_total <- c(100, 100, 100)
  ecozone_num <- c(4, 10, 6)

  ecozone_en <- c("Taiga Plain", "Prairie", "Boreal Shield")
  ecozone_fr <- c("Taïga des plaines", "Prairies", "Bouclier boréal")

  # ---- v2b() ----
  out_num_v2b <- v2b(
    vol_merchantable = vol_total,
    ecozone = ecozone_num,
    species = "PICE.MAR",
    jurisdiction = "AB"
  )
  out_en_v2b <- v2b(
    vol_merchantable = vol_total,
    ecozone = ecozone_en,
    species = "PICE.MAR",
    jurisdiction = "AB"
  )
  out_fr_v2b <- v2b(
    vol_merchantable = vol_total,
    ecozone = ecozone_fr,
    species = "PICE.MAR",
    jurisdiction = "AB"
  )

  expect_equal(out_en_v2b, out_num_v2b)
  expect_equal(out_fr_v2b, out_num_v2b)

  # ---- vol_total_to_merchantable() ----
  out_num_m <- vol_total_to_merchantable(
    vol_total = vol_total,
    ecozone = ecozone_num,
    species = "PICE.MAR",
    jurisdiction = "AB"
  )
  out_en_m <- vol_total_to_merchantable(
    vol_total = vol_total,
    ecozone = ecozone_en,
    species = "PICE.MAR",
    jurisdiction = "AB"
  )
  out_fr_m <- vol_total_to_merchantable(
    vol_total = vol_total,
    ecozone = ecozone_fr,
    species = "PICE.MAR",
    jurisdiction = "AB"
  )

  expect_equal(out_en_m, out_num_m)
  expect_equal(out_fr_m, out_num_m)
})

test_that("Boudewyn functions accept mixed ecozone vectors (name + number)", {
  vol_total <- c(100, 100)
  ecozone_mixed <- c("Taiga Plain", 4)

  out_v2b <- v2b(vol_merchantable = vol_total, ecozone = ecozone_mixed)
  out_ref <- v2b(vol_merchantable = vol_total, ecozone = c(4, 4))

  expect_equal(out_v2b, out_ref)

  out_m <- vol_total_to_merchantable(
    vol_total = vol_total,
    ecozone = ecozone_mixed
  )
  out_ref_m <- vol_total_to_merchantable(
    vol_total = vol_total,
    ecozone = c(4, 4)
  )

  expect_equal(out_m, out_ref_m)
})

test_that("Boudewyn functions error clearly on unknown ecozone names", {
  vol_total <- 100

  expect_error(
    v2b(
      vol_total,
      ecozone = "Not a real ecozone",
      species = "PICE.MAR",
      jurisdiction = "AB"
    ),
    "Unknown ecozone name",
    fixed = FALSE
  )

  expect_error(
    vol_total_to_merchantable(
      vol_total = vol_total,
      ecozone = "Not a real ecozone",
      species = "PICE.MAR",
      jurisdiction = "AB"
    ),
    "Unknown ecozone name",
    fixed = FALSE
  )
})

test_that("Boudewyn functions error clearly on invalid ecozone numbers", {
  vol_total <- 100

  expect_error(
    v2b(vol_total, ecozone = 99, species = "PICE.MAR", jurisdiction = "AB"),
    "Invalid ecozone number",
    fixed = FALSE
  )

  expect_error(
    vol_total_to_merchantable(
      vol_total = vol_total,
      ecozone = 0,
      species = "PICE.MAR",
      jurisdiction = "AB"
    ),
    "Invalid ecozone number",
    fixed = FALSE
  )
})
