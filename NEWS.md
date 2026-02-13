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


# CTAE 0.6.0

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


# CTAE 0.5.2

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

# CTAE 0.5.1

- Changed package license from GPL to LGPL-3.
- Added Nigh 2016 model for BC `vol_nigh2016`

# CTAE 0.5.0

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


