# Convert total volume to merchantable volume (Boudewyn et al. 2007; Appendix 6 / Table 14)

Uses Boudewyn et al. (2007) Appendix 6 (Table 14) parameters to convert
total volume (all live trees) to merchantable volume via a genus-level
model.

## Usage

``` r
vol_total_to_merchantable(
  vol_total,
  species,
  jurisdiction,
  ecozone,
  include_prop = FALSE,
  clamp_prop = TRUE,
  warn_outside = TRUE,
  clamp_x = FALSE
)
```

## Arguments

- vol_total:

  Numeric vector. Total volume per hectare (all live trees).

- species:

  Character vector. NFI species codes (e.g. `"PICE.MAR"`). Only genus is
  used.

- jurisdiction:

  Character vector. Jurisdiction code (e.g. `"AB"`).

- ecozone:

  Ecozone identifier. Either: numeric ecozone code (1–15) or official
  ecozone name (English or French; case-insensitive).

- include_prop:

  Logical. If TRUE, return the predicted merchantable proportion.

- clamp_prop:

  Logical. If TRUE (default), clamp predicted proportion to `[0, 1]`.

- warn_outside:

  Logical. If TRUE (default), warn when `vol_total < volmin`.

- clamp_x:

  Logical. If TRUE, replace `vol_total` with `pmax(vol_total, volmin)`
  before computing the proportion model. Disabled by default.

## Value

A tibble with one row per input and column `merchantable_volume`.
Optionally includes `prop_merch` and `vol_total_used`.

## Examples

``` r
# ---- single stand ----------------------------------------------------------
vol_total_to_merchantable(
  vol_total = 300,
  species = "PICE.MAR",
  jurisdiction = "AB",
  ecozone = "Taiga Plain"
)
#> # A tibble: 1 × 1
#>   vol_merchantable
#>              <dbl>
#> 1             262.

# ---- return predicted merchantable proportion ------------------------------
vol_total_to_merchantable(
  vol_total = 300,
  species = "PICE.MAR",
  jurisdiction = "AB",
  ecozone = 4,
  include_prop = TRUE
)
#> # A tibble: 1 × 2
#>   vol_merchantable prop_merch
#>              <dbl>      <dbl>
#> 1             262.      0.872

# ---- vectorized over volume ------------------------------------------------
vol_total_to_merchantable(
  vol_total = c(50, 100, 300, 600),
  species = "PICE.MAR",
  jurisdiction = "AB",
  ecozone = 4,
  include_prop = TRUE
)
#> # A tibble: 4 × 2
#>   vol_merchantable prop_merch
#>              <dbl>      <dbl>
#> 1             7.99      0.160
#> 2            39.2       0.392
#> 3           262.        0.872
#> 4           582.        0.969

# ---- tidyverse workflow ----------------------------------------------------
library(dplyr)
library(tidyr)

stands <- tibble::tibble(
  stand_id = 1:4,
  vol_total = c(50, 150, 350, 600),
  species = c("PICE.MAR", "PICE.GLA", "ABIE.BAL", "PSEU.MEN"),
  jurisdiction = c("AB", "ON", "QC", "BC"),
  ecozone = c(6, 6, 8, 13)
)

stands |>
  mutate(
    merch = vol_total_to_merchantable(
      vol_total = vol_total,
      species = species,
      jurisdiction = jurisdiction,
      ecozone = ecozone,
      include_prop = TRUE
    )
  ) |>
  unnest(merch)
#> # A tibble: 4 × 7
#>   stand_id vol_total species  jurisdiction ecozone vol_merchantable prop_merch
#>      <int>     <dbl> <chr>    <chr>          <dbl>            <dbl>      <dbl>
#> 1        1        50 PICE.MAR AB                 6             2.16     0.0431
#> 2        2       150 PICE.GLA ON                 6           122.       0.811 
#> 3        3       350 ABIE.BAL QC                 8           310.       0.885 
#> 4        4       600 PSEU.MEN BC                13           529.       0.882 

# ---- chaining with Boudewyn v2b() ------------------------------------------
# Convert total volume -> merchantable volume -> biomass components
stands |>
  mutate(
    merch = vol_total_to_merchantable(
      vol_total = vol_total,
      species = species,
      jurisdiction = jurisdiction,
      ecozone = ecozone
    ),
    biomass = v2b(
      vol_merchantable = merch$vol_merchantable,
      species = species,
      jurisdiction = jurisdiction,
      ecozone = ecozone
    )
  ) |>
  unnest(biomass)
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `biomass = v2b(...)`.
#> Caused by warning:
#> ! Some inputs are outside the calibration range of the Boudewyn proportion models (Table 7). Results are extrapolations. Set clamp_x = TRUE to clamp to [x_min, x_max].
#> # A tibble: 4 × 10
#>   stand_id vol_total species jurisdiction ecozone merch$vol_merchantable b_total
#>      <int>     <dbl> <chr>   <chr>          <dbl>                  <dbl>   <dbl>
#> 1        1        50 PICE.M… AB                 6                   2.16    12.6
#> 2        2       150 PICE.G… ON                 6                 122.      87.2
#> 3        3       350 ABIE.B… QC                 8                 310.     192. 
#> 4        4       600 PSEU.M… BC                13                 529.     333. 
#> # ℹ 3 more variables: b_bark <dbl>, b_branches <dbl>, b_foliage <dbl>
```
