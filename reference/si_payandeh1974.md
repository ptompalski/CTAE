# Payandeh (1974) site index model

Unified, vectorized implementation of the Payandeh (1974) nonlinear site
index equations.

## Usage

``` r
si_payandeh1974(age, height = NULL, si = NULL, species)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years).

- height:

  Optional numeric vector. Total tree height (m). If provided, `si` is
  predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years at breast
  height). If provided, `height` is predicted.

- species:

  Character vector of species codes (e.g., `"ABIE.BAL"`).

## Value

A tibble with columns:

- height:

  Predicted height (m), returned when input `si` is provided.

- si:

  Predicted site index (m), returned when input `height` is provided.

## Details

**Index-age note:** this model uses species-specific site-index base
ages (not a single common base age for all species). In this
implementation: 50 years for `ABIE.BAL, PICE.GLA, PICE.RUB, PINU.MON`;
80 years for `PINU.CON`; and 100 years for
`PICE.SIT, PINU.PON, PSEU.MEN, TSUG.HET`.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

Inputs/outputs are metric; the original equations are in imperial units,
so the function converts internally.

## References

Payandeh, B. (1974). Nonlinear site index equations for several major
Canadian timber species. *The Forestry Chronicle*, 50(5), 194-196.

## Examples

``` r
# Predict site index from age + height
si_payandeh1974(
  age = c(20, 40, 60),
  height = c(8, 16, 28),
  species = c("ABIE.BAL", "PICE.GLA", "PSEU.MEN")
)
#> Note that Payandeh1974 site index models use different index age by species.
#> ℹ Index age 50: ABIE.BAL, PICE.GLA, PICE.RUB, PINU.MON
#> ℹ Index age 80: PINU.CON
#> ℹ Index age 100: PICE.SIT, PINU.PON, PSEU.MEN, TSUG.HET
#> This message is displayed once per session.
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  20.5
#> 2  19.6
#> 3  29.8

# Predict height from age + site index
si_payandeh1974(
  age = c(20, 40, 60),
  si = c(12, 18, 24),
  species = c("ABIE.BAL", "PICE.GLA", "PSEU.MEN")
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   4.56
#> 2  14.9 
#> 3  18.8 

# Works with the pipe operator: predict site index from age + height
trees_h <- tibble::tibble(
  species = c("ABIE.BAL", "PICE.GLA", "PSEU.MEN"),
  age = c(20, 40, 60),
  height = c(8, 16, 28)
)

trees_h |>
  dplyr::mutate(
    si_pred = si_payandeh1974(
      age = age,
      height = height,
      species = species
    )
  ) |>
  tidyr::unnest(si_pred)
#> # A tibble: 3 × 4
#>   species    age height    si
#>   <chr>    <dbl>  <dbl> <dbl>
#> 1 ABIE.BAL    20      8  20.5
#> 2 PICE.GLA    40     16  19.6
#> 3 PSEU.MEN    60     28  29.8

# With pipe: predict height from age + site index
trees_si <- tibble::tibble(
  species = c("ABIE.BAL", "PICE.GLA", "PSEU.MEN"),
  age = c(20, 40, 60),
  si = c(12, 18, 24)
)

trees_si |>
  dplyr::mutate(
    height_pred = si_payandeh1974(
      age = age,
      si = si,
      species = species
    )
  ) |>
  tidyr::unnest(height_pred)
#> # A tibble: 3 × 4
#>   species    age    si height
#>   <chr>    <dbl> <dbl>  <dbl>
#> 1 ABIE.BAL    20    12   4.56
#> 2 PICE.GLA    40    18  14.9 
#> 3 PSEU.MEN    60    24  18.8 
```
