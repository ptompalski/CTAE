# CanadaForestAllometry 0.8.6

## Site index updates

- Added `si_huang2009()` for aspen (`POPU.TRE`), black spruce (`PICE.MAR`), lodgepole pine (`PINU.CON`), and white spruce (`PICE.GLA`) in Alberta, implementing the Huang, Meng & Yang (2009) GYPSY top-height / site-index models (total-age based, with breast-height-age site index via `index_age`).
- Added `si_chenklinka2000()` for subalpine fir (`ABIE.LAS`), Engelmann spruce (`PICE.ENG`), and lodgepole pine (`PINU.CON`) in the ESSF zone of British Columbia, implementing the conditioned Chapman--Richards height-age (site index) model of Chen and Klinka (2000).
- Added `si_brisco2002()` for western larch (`LARI.OCC`) in British Columbia, implementing the recommended Chapman--Richards height-age (site index) model of Brisco, Klinka and Nigh (2002) (no source benchmark).
- Added `si_alemdag1991()` for white spruce (`PICE.GLA`), implementing the Alemdag (1991) national (Canada-wide) modified Chapman--Richards site-index and height-growth model for natural stands (no source benchmark).
- Added `si_nigh1998()` and `si_nigh1998_gi()` for western hemlock (`TSUG.HET`) in the interior of British Columbia, implementing the Nigh (1998) log-logistic height-age and growth-intercept site-index models.

- Added `si_batho2014()` for lodgepole pine (`PINU.CON`) in the Sub-Boreal Spruce zone of British Columbia, implementing the Batho and García (2014) polymorphic Bertalanffy--Richards height-age / site-index model (no source benchmark).
- Added `si_nigh2017()` for lodgepole pine (`PINU.CON`) in British Columbia, implementing the Nigh (2017) grounded-GADA (Chapman-Richards) height-age / site-index model.
- Added `si_carmean2001()` for jack pine (`PINU.BAN`) in northern Ontario, implementing the Carmean, Niznowski & Hazenberg (2001) constrained polymorphic (Newnham) height-age / site-index model.
- Added `si_goelz1992()` for jack pine (`PINU.BAN`) in north central Ontario, implementing the Goelz & Burk (1992) base-age invariant Chapman--Richards height-age / site-index model.
- Added `si_carmean2006()` for black spruce (`PICE.MAR`) and trembling aspen (`POPU.TRE`) in northwest Ontario, implementing the Carmean, Hazenberg & Deschamps (2006) constrained polymorphic height-age / site-index model.
- Added `si_nigh2004()` for lodgepole pine (`PINU.CON`) and interior spruce (`PICE.GLA`) in British Columbia, implementing the Nigh (2004) juvenile height-age / site-index model, with province-wide and biogeoclimatic-zone parameter sets selectable via `bec_zone`.
- Added `si_nigh2009()` for paper birch (`BETU.PAP`) in British Columbia, implementing the Nigh, Thomas, Yearsley & Wang (2009) log-logistic height-age / site-index model, with base, operational, and zonal variants selectable via `model`.
- Added `si_goudie1984()` for lodgepole pine (`PINU.CON`) and white spruce (`PICE.GLA`) in British Columbia, implementing the Goudie (1984) logistic height-age / site-index curves.
- Added `si_cieszewski1993()` for eleven Saskatchewan timber species, implementing the preliminary variable-age height-growth / site-index model of Cieszewski, Bella and Yeung (1993).
- Added `si_hugarcia2009()` for interior spruce (`PICE.GLA`, `PICE.ENG`) in the Sub-Boreal Spruce zone of British Columbia, implementing the polymorphic Bertalanffy--Richards model of Hu and García (2009).
- Added `si_nigh1997()` for Sitka spruce (`PICE.SIT`) in coastal British Columbia, implementing the Nigh (1997) logistic height-age / site-index model (no source benchmark).
- Added `si_nigh2002()` for trembling aspen (`POPU.TRE`) in British Columbia, implementing the logistic height-age / site-index model of Nigh, Krestov and Klinka (2002), with base and biogeoclimatic-zone-calibrated variants via `bec_zone`.

## Years-to-breast-height updates

- Added `ytbh_nigh1998()` for western hemlock (`TSUG.HET`) in the interior of British Columbia, implementing the Nigh (1998) years-to-breast-height model (invertible for site index).
- Added `ytbh_nigh2015()` for Engelmann spruce (`PICE.ENG`) in the ESSF biogeoclimatic zone of British Columbia, implementing the Nigh (2015) years-to-breast-height model (no source benchmark).

## Internal changes

- Moved the `si_carmeanhahn1981()`, `si_augerward2021()`, `si_sharma2022()`, `si_nigh1998()`, `si_buckman2006()`, and `si_batho2014()` model coefficients from inline tibbles/constant lists into the compiled internal parameter data, for consistency with the rest of the site-index family.
- Consolidated the shared Carmean constrained-polymorphic (Newnham) height and site-index solver used by `si_carmean2001()` and `si_carmean2006()` into common internal helpers; model outputs are unchanged.



# CanadaForestAllometry 0.8.5

## Bug fixes

- Corrected sugar maple species-code mappings from `ACER.SAC` to `ACER.SAH` in Fortin et al. (2007), Scott and Voorhis (1986), and Carmean et al. (1989) parameter data.
- Corrected Sharma (2021) sugar maple volume parameter mappings from `ACER.SAC` to `ACER.SAH`.
- Added regression tests to ensure sugar maple (`ACER.SAH`) and silver maple (`ACER.SAC`) remain distinct in affected model parameter tables.


# CanadaForestAllometry 0.8.4

## Site index updates

- Added Quebec plantation site-index implementations:
  - `si_pregent2010()` for white spruce plantations
  - `si_pregent2016()` for Norway spruce plantations
  - `si_augerward2021()` for jack pine and black spruce plantations

## Height-diameter updates

- Added `hd_huang2013()` for fixed/population-average height-diameter predictions for major Alberta tree species.
- Added `hd_sharmaparton2007()` for fixed height-diameter predictions for eight Ontario boreal tree species using DBH and stand covariates.
- Added `hd_rijal2012()` for fixed height-diameter predictions for 15 Acadian Region tree species using DBH, site, and competition covariates.


# CanadaForestAllometry 0.8.3

## Site index updates

- Added site index model implementations used in Ontario for:
  - `si_parresolvissage1998()` for Parresol & Vissage (1998)
  - `si_sharmaparton2019()` for Sharma & Parton (2019)
  - `si_buckman2006()` for Buckman et al. (2006)
  - `si_sharmaparton2018a()` for Sharma & Parton (2018a)
  - `si_sharmareid2018()` for Sharma and Reid (2018)
  - `si_sharma2015()` for Sharma et al. (2015)
  - `si_sharmaparton2018b()` for Sharma & Parton (2018b)
  - `si_sharma2022()` for Sharma (2022)
  - `si_carmean1996()` for Carmean (1996)
  - `si_carmean1989()` for Carmean et al. (1989)


# CanadaForestAllometry 0.8.2

## New features

- Added `translate_species_code()`, a species translator for converting among NFI, CANFI, and jurisdiction-specific species codes, as well as English and French common names and scientific names.
- Added `si_cieszewskibella1991()`, implementing the Cieszewski & Bella (1991) site index model.

# CanadaForestAllometry 0.8.1

## Site index updates

- Added `si_huang1994()` (Huang et al. 1994) for Alberta species with support for:
  - predicting `height` from `si`, and
  - predicting `si` from `height`.
- Added species/subregion parameter handling for Huang 1994 SI coefficients in internal data.
- Improved `subregion` matching in `si_huang1994()`:
  - accepts `"All"` (and aliases `"provincial"` / `"province"`),
  - accepts exact grouped subregion strings, and
  - accepts single Alberta subregion codes (e.g., `"ALP"`) by mapping to grouped parameter sets.
- Improved error messages to report missing species/subregion combinations and allowed subregion groups.
- Added tests for Huang 1994 SI behavior, table validation, input checks, and subregion matching edge cases.


# CanadaForestAllometry 0.8.0

## New: Site index models

- Added a suite of site index models under a unified interface:
  - `si_carmeanhahn1981()`
  - `si_kerbowling1991()`
  - `si_lundgrendolid1970()`
  - `si_nigh2000()` and `si_nigh2000_gi()`
  - `si_nighcourtin1998()`
  - `si_payandeh1974()`
  - `si_scottvoorhis1986()`
  - `si_thrower1994()`
- Added support helpers used by selected SI models (`ytbh_*`).

## Documentation and website

- Added site index content to package documentation.
- Updated pkgdown reference sections to group SI and related functions.
- Updated README model overview to include site index functionality.


# CanadaForestAllometry 0.7.1

## New: Newfoundland and Labrador volume model (`vol_nl()`)

- Added a new regional volume model for Newfoundland and Labrador based on:
  - Ker (1974) — *N-X-122* (province-wide total volume)
  - Warren & Meades (1986) — *N-X-242* (district-level total, gross and net volume)
  - Honer (1967) — *N-X-67* (merchantable conversion factors)

- The implementation is based on the original OSM (C#) code (thank you Chris Hennigar):
  - District-specific equations are used only for districts 2 and 4–18
  - District 19 is intentionally excluded (unstable behaviour noted in original implementation)
  - All other districts (including 1, 3, 20–24) fall back to province-wide equations
  - Net merchantable volume (when available) is internally constrained to ≥95% of gross volume

- Default behavior:
  - `subregion = "Province"` uses province-wide parameters
  - Output includes `vol_total` and `vol_merchantable`, consistent with other volume models
  - Optional `keep_net = TRUE` exposes gross and net merchantable components


## Parameter storage refactor

- Model parameters are now stored as internal datasets



# CanadaForestAllometry 0.7.0

- **Renamed package**: *CTAE* is now **CanadaForestAllometry**.
- This is a naming change only
- All existing functions are available under the new package name.


# CanadaForestAllometry 0.6.0

## Boudewyn et al. (2007) volume-to-biomass refactor (`v2b()`)

The Boudewyn et al. (2007) volume-to-biomass implementation has been fully refactored and standardized under a new high-level wrapper, `v2b()`.

This refactor preserves the original model structure (Tables 3–7) while improving correctness, transparency, and vectorization:

* Model components are now modularized and vectorized, making the functions safe to use inside `dplyr::mutate()` workflows.
* Parameter selection is stricter and more explicit, with clear errors when parameters are missing or non-unique.
* Volume-based biomass proportion models (Table 6) now explicitly apply the proportion bounds (“caps”) reported in Table 7, as described in Boudewyn et al. (2007), to prevent unrealistic proportions when extrapolating.
* Optional renormalization ensures capped proportions sum to 1.
* Warnings are issued when volumes fall outside the calibration range (`x_min` / `x_max`) reported in Table 7.


## Boudewyn et al. (2007) total volume to merchantable volume (`vol_total_to_merchantable()`)  

New implementation of the Boudewyn et al. (2007) *total volume → merchantable volume* conversion based on Appendix 6 (Table 14).
  
- Fully vectorized and safe for use inside `dplyr::mutate()` + `tidyr::unnest()`.
- Supports genus-level species matching using NFI species codes.
- Optional controls for:
  - returning the predicted merchantable proportion (`include_prop = TRUE`);
   - clamping predicted proportions to [0, 1];
   - warning on extrapolation below the calibration minimum (`volmin`);
   - clamping input volume to the calibration domain (`clamp_x = TRUE`).

This function is intended for workflows where total volume is available but merchantable volume is required, e.g. prior to applying Boudewyn volume-to-biomass conversions.

Results may represent extrapolations when total volume falls below the calibration range reported in Boudewyn et al. (2007); warnings are issued by default in such cases.




#### Important note on numerical differences
Results from `v2b()` may differ from earlier implementations of the Boudewyn models, particularly for high-volume stands.  
These differences arise because earlier code paths applied the Table 6 proportion equations directly, without enforcing the Table 7 bounds. The new implementation follows the documented model behavior more closely by applying these caps.


# CanadaForestAllometry 0.5.2

## Changes

- Refactored the Lambert & Ung aboveground biomass implementation into a single unified function, `agb_lambert_ung()`, replacing the previous DBH-only and DBH+height variants.
- The refactored function exposes the two Lambert & Ung equation sets (DBH-only and DBH + height) via the new `equation_set` argument
  (`"dbh"`, `"dbh_height"`, or `"auto"`).
- When `equation_set = "auto"` (default), the function automatically selects the DBH + height equations when valid height is provided, and falls back to the DBH-only equations otherwise.
- The implementation is now fully vectorized across trees and species and uses cached coefficient lookup for substantially improved performance.

## Breaking changes

- The former DBH-only and DBH+height Lambert & Ung functions have been superseded by `agb_lambert_ung()` and are no longer exported.
- The refactored implementation is vectorized and must be called on full vectors of inputs; row-wise usage (e.g., via `dplyr::rowwise()`) is no longer supported and may lead to incorrect results or poor performance.
- The equation set used can be included explicitly via `keep_model_id = TRUE`, in which case `model_id` is  returned as the last column.

# CanadaForestAllometry 0.5.1

- Changed package license from GPL to LGPL-3.
- Added Nigh 2016 model for BC `vol_nigh2016`

# CanadaForestAllometry 0.5.0

## Major features

* Added a suite of tree volume models with:
  - consistent handling of species codes, jurisdictions, and subregions,
  - standardized outputs for total and merchantable volume,
  - shared internal helpers for parameter lookup and validation.
* Added a user-friendly model-selection wrapper that can evaluate multiple candidate
  volume models and optionally select the best available model, with explicit
  warnings when fallbacks are used.
* Added jurisdiction-specific merchantability criteria
* Added extensive internal parameter datasets for volume and taper models.

## Testing 

* Added test coverage for volume-related functions.
* Improved error and warning messages .
