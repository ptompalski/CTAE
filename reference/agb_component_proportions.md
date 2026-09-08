# Boudewyn biomass component proportions (Tables 6–7)

Compute component proportions (stemwood, bark, branches, foliage) using
the multinomial-logit models (Table 6) and bounds (Table 7), either as a
function of merchantable volume ("vol") or total aboveground biomass
("agb").

## Usage

``` r
agb_component_proportions(
  x,
  value_type = c("vol", "agb"),
  species,
  jurisdiction,
  ecozone,
  renormalize = TRUE,
  clamp_x = FALSE
)
```

## Arguments

- x:

  Numeric vector. Input to the proportion models: merchantable volume
  (t/ha) when value_type = "vol", or total biomass (t/ha) when
  value_type = "agb".

- value_type:

  One of "vol" or "agb".

- species:

  Character vector. NFI species codes (e.g., "PICE.MAR").

- jurisdiction:

  Character vector. Jurisdiction code (e.g., "AB").

- ecozone:

  Ecozone identifier. Either: numeric ecozone code (1–15) or official
  ecozone name (English or French; case-insensitive).

- renormalize:

  Logical. Renormalize after capping to sum to 1.

- clamp_x:

  Logical. Clamp x to `[x_min, x_max]` from Table 7 before evaluating.

## Value

Tibble with p_sw, p_sb, p_br, p_fl and optional x_used.

- p_sw:

  Proportion of total biomass in stem wood.

- p_sb:

  Proportion of total biomass in bark.

- p_br:

  Proportion of total biomass in branches.

- p_fl:

  Proportion of total biomass in foliage.

## Examples

``` r
# ---- Volume-based proportions (most common use) ---------------------------

# Single stand
agb_component_proportions(
  x = 350,
  value_type = "vol",
  species = "PICE.MAR",
  jurisdiction = "AB",
  ecozone = 6
)
#> # A tibble: 1 × 4
#>    p_sw   p_sb   p_br   p_fl
#>   <dbl>  <dbl>  <dbl>  <dbl>
#> 1 0.754 0.0915 0.0946 0.0596


# ---- Biomass-based proportions --------------------------------------------

agb_component_proportions(
  x = 120,
  value_type = "agb",
  species = "PICE.MAR",
  jurisdiction = "AB",
  ecozone = 6
)
#> # A tibble: 1 × 4
#>    p_sw  p_sb  p_br   p_fl
#>   <dbl> <dbl> <dbl>  <dbl>
#> 1 0.717 0.102 0.106 0.0749


# ---- Vectorized / tidyverse workflow --------------------------------------

library(dplyr)
library(tidyr)

stands <- tibble::tibble(
  stand_id = 1:3,
  vol_merchantable = c(120, 350, 300),
  species = c("PICE.MAR", "PSEU.MEN", "PINU.BAN"),
  jurisdiction = c("AB", "BC", "ON"),
  ecozone = c(5, 13, 6)
)

stands |>
  mutate(
    props = agb_component_proportions(
      x = vol_merchantable,
      value_type = "vol",
      species = species,
      jurisdiction = jurisdiction,
      ecozone = ecozone
    )
  ) |>
  unnest(props)
#> # A tibble: 3 × 9
#>   stand_id vol_merchantable species  jurisdiction ecozone  p_sw   p_sb   p_br
#>      <int>            <dbl> <chr>    <chr>          <dbl> <dbl>  <dbl>  <dbl>
#> 1        1              120 PICE.MAR AB                 5 0.731 0.100  0.104 
#> 2        2              350 PSEU.MEN BC                13 0.738 0.138  0.0995
#> 3        3              300 PINU.BAN ON                 6 0.800 0.0766 0.0810
#> # ℹ 1 more variable: p_fl <dbl>
```
