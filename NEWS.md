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


