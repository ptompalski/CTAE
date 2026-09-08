# Thrower et al. (1994) years-to-breast-height models

Species-specific years-to-breast-height (YTBH) equations used with the
Thrower et al. (1994) interior BC site-index formulations.

## Usage

``` r
ytbh_thrower1994(si, species)
```

## Arguments

- si:

  Numeric vector. Site index (m, base age 50 years at breast height).

- species:

  Character vector of NFI species codes (e.g., `"PINU.CON"`).

## Value

A tibble with one column:

- ytbh:

  Predicted years to breast height (years).

## References

Thrower, J.S., Nussbaum, A.F., and Di Lucca, C.M. (1994). Site index
curves and tables for British Columbia: interior species (2nd ed.). B.C.
Ministry of Forests, Land Management Handbook, Field Guide Insert 6.

## Examples

``` r
ytbh_thrower1994(
  si = c(12, 16, 20),
  species = c("PINU.CON", "THUJ.PLI", "ABIE.LAS")
)
#> # A tibble: 3 × 1
#>    ytbh
#>   <dbl>
#> 1  9.15
#> 2 10.6 
#> 3 10.3 
```
