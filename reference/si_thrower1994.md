# Thrower et al. (1994) site index models for BC interior species

Unified, vectorized implementation of the field-guide formulations in
Thrower, Nussbaum, and Di Lucca (1994) for interior British Columbia
species.

## Usage

``` r
si_thrower1994(age, height = NULL, si = NULL, species)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years).

- height:

  Optional numeric vector. Top height (m). If provided, `si` is
  predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years at breast
  height). If provided, `height` is predicted.

- species:

  Character vector of NFI species codes (e.g., `"PINU.CON"`).

## Value

A tibble with columns:

- height:

  Predicted top height (m), returned when input `si` is provided.

- si:

  Predicted site index (m), returned when input `height` is provided.

## Details

**Model scope (species coverage):** this implementation includes
parameter sets for 11 species:
`PINU.CON, PICE.GLA, PSEU.MEN, ABIE.LAS, TSUG.HET, THUJ.PLI, PINU.MON, PINU.PON, LARI.OCC, POPU.TRE, BETU.PAP`.

**Age definition note:** `age` is breast-height age (years).

**Base-age note:** site index in this model family is referenced to
height at 50 years breast-height age.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

## References

Thrower, J.S., Nussbaum, A.F., and Di Lucca, C.M. (1994). Site index
curves and tables for British Columbia: interior species (2nd ed.). B.C.
Ministry of Forests, Land Management Handbook, Field Guide Insert 6.

## Examples

``` r
# Predict site index from age + height
si_thrower1994(
  age = c(25, 40, 70),
  height = c(8, 16, 24),
  species = c("PINU.CON", "PSEU.MEN", "THUJ.PLI")
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  13.5
#> 2  18.7
#> 3  19.4

# Predict height from age + site index
si_thrower1994(
  age = c(25, 40, 70),
  si = c(12, 18, 24),
  species = c("PINU.CON", "PSEU.MEN", "THUJ.PLI")
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   7.05
#> 2  15.3 
#> 3  29.7 
```
