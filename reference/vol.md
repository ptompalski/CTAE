# Calculate tree volume: choose best available model or run all candidate models

`vol()` is the main convenience wrapper for estimating total and
merchantable stem volume from diameter and height. It automatically
selects and runs one (or more) of the volume models implemented in
CanadaForestAllometry, based on the model registry returned by
[`volume_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/volume_model_registry.md).

## Usage

``` r
vol(
  DBH,
  height = NA_real_,
  species,
  jurisdiction,
  subregion = NA_character_,
  pick_best = TRUE,
  keep_model_id = TRUE
)
```

## Arguments

- DBH:

  Numeric vector. Diameter at breast height (cm).

- height:

  Numeric vector. Total height (m). Can be `NA` to use the DBH-only
  models (currently only
  [`vol_ung2013()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_ung2013.md)).

- species:

  Character vector. Species codes (e.g., "PICE.GLA").

- jurisdiction:

  Character scalar or vector. Province/territory code (e.g., "BC",
  "QC").

- subregion:

  Optional character vector. Subregion identifier (e.g., BEC zone for
  [`vol_kozak94()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_kozak94.md)).

- pick_best:

  Logical. If `TRUE` (default), pick the single best model per tree
  using `rank`. If `FALSE`, compute all candidate model outputs per tree
  and return a list-column (one tibble per tree) that can be unnested
  for comparison.

- keep_model_id:

  Logical. If `TRUE`, include a `vol_model` column with the engine
  function name used.

## Value

If `pick_best = TRUE`, a tibble with one row per tree and columns:
`vol_total`, `vol_merchantable`, and optionally `vol_model`.

If `pick_best = FALSE`, a list (suitable for use as a list-column in
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html))
with one element per tree. Each element is a tibble of candidate model
outputs (`vol_total`, `vol_merchantable`, and optionally `vol_model`).

## Model selection logic

Model selection is driven by the model registry and occurs per input
tree:

1.  Geographic applicability: models are filtered to those whose
    `province_scope` includes the provided `jurisdiction` (or `"ALL"`).

2.  Species coverage: models are filtered to those that include the
    provided `species` in their parameter tables. Some models may
    include genus/group codes (e.g., `"PICE.SPP"`); these are treated as
    covering all matching species within that genus/group.

3.  Required inputs: models that require total height are excluded when
    `height` is missing. Models that require a subregion (e.g., BEC zone
    for certain provincial models) are excluded when `subregion` is
    missing.

4.  Model ranking: if multiple eligible models remain,
    `pick_best = TRUE` selects the highest-ranked model (`rank`) for
    each tree.

## Fallback behavior

Availability is evaluated at the species level, not only by
jurisdiction. This means a regional/provincial model may exist for a
jurisdiction but may not include coefficients for the provided species.
In such cases, `vol()` automatically falls back to another eligible
model (e.g., a multi-province regional model or a national model) that
*does* cover the species and required inputs.

## Important limitations and recommended use

- This is an automatic wrapper. Because selection is registry-driven,
  `vol()` may use a model that is not the one you intended (e.g., a
  national model instead of a regional model, or a different regional
  model than you expected), especially when the preferred model is not
  available for the input species or missing required inputs.

- Model outputs differ. Different models will generally return slightly
  different volume estimates for the same DBH/height/species due to
  differences in model form, fitted data, geographic scope, and
  merchantability assumptions. These differences are expected.

- If you require a specific model, do not use `vol()`. Instead, call the
  relevant model function directly (e.g.,
  [`vol_huang94()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_huang94.md),
  [`vol_kozak94()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_kozak94.md),
  [`vol_ung2013()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_ung2013.md),
  etc.) to ensure full control and reproducibility.

- Investigate before operational use. It is strongly recommended to run
  `vol()` with `pick_best = FALSE` during exploratory analyses to
  inspect which models are eligible and how sensitive results are to
  model choice. When `pick_best = FALSE`, `vol()` returns a list-column
  (one element per tree) containing a tibble of candidate model outputs;
  this can be unnested to compare models.

## Examples

``` r
## --- Basic usage: pick best available model (default) ---
vol(
  DBH = 20,
  height = 20,
  species = "PICE.MAR",
  jurisdiction = "ON"
)
#> # A tibble: 1 × 3
#>   vol_total vol_merchantable vol_model         
#>       <dbl>            <dbl> <chr>             
#> 1     0.285            0.260 vol_zakrzewski2013

## --- Return all applicable models (one row per engine) ---
## Note: national Ung DBH-only and DBH+HT variants are collapsed
## into a single result (preferring the height-based variant).
vol(
  DBH = 20,
  height = 20,
  species = "PICE.MAR",
  jurisdiction = "ON",
  pick_best = FALSE,
  keep_model_id = TRUE
)
#> [[1]]
#> # A tibble: 4 × 3
#>   vol_total vol_merchantable vol_model         
#>       <dbl>            <dbl> <chr>             
#> 1     0.260            0.227 vol_honer83       
#> 2     0.282            0.267 vol_sharma2021    
#> 3     0.266            0.248 vol_ung2013       
#> 4     0.285            0.260 vol_zakrzewski2013
#> 

## --- Missing height: height-dependent models are skipped ---
## Falls back to DBH-only models where available (e.g. national Ung).
vol(
  DBH = 25,
  species = "BETU.PAP",
  jurisdiction = "QC"
)
#> # A tibble: 1 × 3
#>   vol_total vol_merchantable vol_model  
#>       <dbl>            <dbl> <chr>      
#> 1     0.441            0.428 vol_ung2013

## --- Regional model with required subregion (BC example) ---
## Kozak (1994) requires a BEC zone; if provided, it is preferred.
vol(
  DBH = 30,
  height = 22,
  species = "THUJ.PLI",
  jurisdiction = "BC",
  subregion = "CWH"
)
#> # A tibble: 1 × 3
#>   vol_total vol_merchantable vol_model  
#>       <dbl>            <dbl> <chr>      
#> 1     0.655            0.565 vol_kozak94

## --- Automatic fallback when a regional model does not cover species ---
## If no provincial model covers the species, a suitable national or
## multi-province regional model is used instead.
vol(
  DBH = 18,
  height = 15,
  species = "ACER.SAC",
  jurisdiction = "MB"
)
#> # A tibble: 1 × 3
#>   vol_total vol_merchantable vol_model  
#>       <dbl>            <dbl> <chr>      
#> 1     0.151            0.142 vol_ung2013

## --- Vectorized input ---
vol(
  DBH = c(18, 25, 32),
  height = c(15, 20, 25),
  species = c("PICE.MAR", "BETU.PAP", "ACER.RUB"),
  jurisdiction = "ON",
  keep_model_id = TRUE
)
#> # A tibble: 3 × 3
#>   vol_total vol_merchantable vol_model         
#>       <dbl>            <dbl> <chr>             
#> 1     0.172            0.150 vol_zakrzewski2013
#> 2     0.411            0.353 vol_zakrzewski2013
#> 3     0.766            0.635 vol_zakrzewski2013

#' ## --- Tidyverse workflow with multiple species and jurisdictions ---
library(dplyr)

trees <- tibble::tibble(
  DBH = c(18, 22, 30, 26),
  height = c(15, 18, 22, 20),
  species = c("PICE.MAR", "BETU.PAP", "POPU.TRE", "PSEU.MEN"),
  jurisdiction =  c("AB", "ON", "QC", "BC"),
  subregion = c(NA, NA, NA, "CWH")
)

trees |>
  dplyr::mutate(
    vol(
      DBH = DBH,
      height = height,
      species = species,
      jurisdiction = jurisdiction,
      subregion = subregion,
      keep_model_id = TRUE
    )
  )
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `.out = purrr::map2(.data$engine, .data$.args,
#>   safe_call_engine_one)`.
#> Caused by warning:
#> ! There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `merch = purrr::pmap(...)`.
#> Caused by warning:
#> ! There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `merch = purrr::pmap(...)`.
#> Caused by warning:
#> ! BEC_zone missing; using conservative BC 'UNKNOWN' utilization.
#> # A tibble: 4 × 8
#>     DBH height species  jurisdiction subregion vol_total vol_merchantable
#>   <dbl>  <dbl> <chr>    <chr>        <chr>         <dbl>            <dbl>
#> 1    18     15 PICE.MAR AB           NA            0.158            0.145
#> 2    22     18 BETU.PAP ON           NA            0.291            0.233
#> 3    30     22 POPU.TRE QC           NA           NA                0.674
#> 4    26     20 PSEU.MEN BC           CWH           0.387            0.305
#> # ℹ 1 more variable: vol_model <chr>

## --- Same data, return all applicable volume models per tree ---
trees |>
  dplyr::mutate(
    vol(
      DBH = DBH,
      height = height,
      species = species,
      jurisdiction = jurisdiction,
      subregion = subregion,
      pick_best = FALSE,
      keep_model_id = TRUE
    )
  )
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `.out = purrr::map2(.data$engine, .data$.args,
#>   safe_call_engine_one)`.
#> Caused by warning:
#> ! There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `merch = purrr::pmap(...)`.
#> Caused by warning:
#> ! There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `merch = purrr::pmap(...)`.
#> Caused by warning:
#> ! BEC_zone missing; using conservative BC 'UNKNOWN' utilization.
#> # A tibble: 4 × 6
#>     DBH height species  jurisdiction subregion `vol(...)`      
#>   <dbl>  <dbl> <chr>    <chr>        <chr>     <list>          
#> 1    18     15 PICE.MAR AB           NA        <tibble [2 × 3]>
#> 2    22     18 BETU.PAP ON           NA        <tibble [4 × 3]>
#> 3    30     22 POPU.TRE QC           NA        <tibble [4 × 3]>
#> 4    26     20 PSEU.MEN BC           CWH       <tibble [3 × 3]>


```
