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


