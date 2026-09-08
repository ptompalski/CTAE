# Get volume-model parameters from the CanadaForestAllometry registry

Retrieves the parameter table for a given volume model using
[`volume_model_registry`](https://ptompalski.github.io/CanadaForestAllometry/reference/volume_model_registry.md)
and the parameter objects bundled with the CanadaForestAllometry package
(e.g., `parameters_NationalTaperModelsDBH`).

## Usage

``` r
get_volume_params(
  model_id,
  species,
  province = NA_character_,
  subregion = "ALL",
  strict = FALSE
)
```

## Arguments

- model_id:

  A single model id from `volume_model_registry()$model_id`, e.g.
  `"national_ung_dbh"` or `"regional_kozak88"`.

- species:

  Species code (e.g., `"POPU.TRE"`). Required for all current models.

- province:

  Province/jurisdiction code (e.g., `"BC"`, `"AB"`). Used for regional
  models when the parameter table includes a `Province` column.

- subregion:

  Subregion identifier (BEC zone in BC). Used for regional models when
  the parameter table includes a `Subregion` column.

- strict:

  Logical. If `TRUE`, throw an error when no matching parameters are
  found. If `FALSE` (default), returns an empty tibble.

## Value

A tibble containing the filtered parameter rows for the requested model.
If no rows match and `strict = FALSE`, returns an empty tibble with the
same columns as the underlying parameter table.

## Details

The function then applies **context filters** (when relevant / available
in the parameter table), typically by `species`, and for regional models
also by `province` and `subregion`.

This function does **not** refactor or run any volume engines; it only
returns the parameter rows needed for a given model/context.

## Examples

``` r
# National DBH-only (Ung et al.)
# get_volume_params("national_ung_dbh", species = "POPU.TRE")

# Regional Kozak 88 (requires province/subregion in most parameter tables)
# get_volume_params("regional_kozak88", species = "POPU.TRE", province = "AB", subregion = "ALL")

# BC Kozak 94 (BEC zone as subregion)
# get_volume_params("regional_kozak94", species = "PSEU.MEN", province = "BC", subregion = "SBS")
```
