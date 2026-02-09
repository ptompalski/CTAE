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


