# Estimate tree merchantable volume using Fortin et al. (2007) model (QC)

Deterministic (fixed-effects only) implementation of the Fortin et al.
general merchantable volume model developed for Québec forests.

## Usage

``` r
vol_fortin2007(DBH, height, species)
```

## Source

Fortin, M., DeBlois, J., Bédard, S., Meunier, S. (2007). *Mise au point
d’un tarif de cubage général pour les forêts québécoises : une approche
pour mieux tenir compte des effets de la dimension des arbres.*
Gouvernement du Québec, Ministère des Ressources naturelles et de la
Faune.

Full reference implementation:
<https://github.com/CWFC-CCFB/CFSForestTools>

## Arguments

- DBH:

  Numeric vector of diameter at breast height (cm).

- height:

  Numeric vector of total tree height (m).

- species:

  Character vector of species codes (standardized with
  [`standardize_species_code()`](https://ptompalski.github.io/CanadaForestAllometry/reference/standardize_species_code.md)).

## Value

A tibble with volumes (m^3):

- vol_total:

  Numeric. Total volume (m\\^3\\). Currently returned as `NA` as a
  placeholder; total volume will be handled later via expansion factors.

- vol_merchantable:

  Numeric. Merchantable stem volume (m\\^3\\, under bark).

## Details

This function returns merchantable stem volume (under bark) at the
individual-tree level as a function of diameter at breast height (DBH)
and total tree height. Only the fixed-effects (mean) component of the
original mixed-effects formulation is implemented in
CanadaForestAllometry; stochastic components (random effects and
residual variance) are intentionally omitted.

Merchantable volume is defined according to Québec provincial inventory
standards. Trees with DBH \< 9.1 cm are considered non-merchantable and
have merchantable volume equal to zero.

Users requiring the complete mixed-effects model formulation and full
functionality (e.g., stochastic predictions and uncertainty propagation)
are directed to the official Java implementation available in the
*CFSForestTools* repository:
<https://github.com/CWFC-CCFB/CFSForestTools>

## Examples

``` r
# Single tree
vol_fortin2007(
  DBH = 20,
  height = 18,
  species = "PICE.MAR"
)
#> # A tibble: 1 × 2
#>   vol_total vol_merchantable
#>       <dbl>            <dbl>
#> 1        NA            0.235

# Multiple trees, vectorized
vol_fortin2007(
  DBH = c(18, 25, 32),
  height = c(15, 20, 24),
  species = c("ABIE.BAL", "PICE.GLA", "PINU.BAN")
)
#> # A tibble: 3 × 2
#>   vol_total vol_merchantable
#>       <dbl>            <dbl>
#> 1        NA            0.157
#> 2        NA            0.423
#> 3        NA            0.813

```
