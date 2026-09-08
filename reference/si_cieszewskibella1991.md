# Cieszewski and Bella (1991) site-index model for major Alberta tree species

Vectorized implementation of the polymorphic, variable-age site-index
(VASI) model described by Cieszewski and Bella (1991) for four major
Alberta tree species.

## Usage

``` r
si_cieszewskibella1991(age, height = NULL, si = NULL, species)
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

  Character vector of NFI species codes.

## Value

A tibble with columns:

- height:

  Predicted top height (m), returned when input `si` is provided.

- si:

  Predicted site index (m), returned when input `height` is provided.

## Details

**Model scope (species coverage):**
`PINU.CON, PICE.GLA, PICE.MAR, POPU.TRE`.

**Age definition note:** `age` is breast-height age (years).

**Base-age note:** site index is referenced to 50 years breast-height
age for all species in this implementation.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

## References

Cieszewski, C. J., & Bella, I. E. (1991). Polymorphic height and site
index curves for the major tree species in Alberta. *Forest Management
Note*, 51, 1-8.

## Examples

``` r
# Predict site index from age + height
si_cieszewskibella1991(
  age = c(50, 60, 70),
  height = c(14, 17, 20),
  species = c("PINU.CON", "PICE.GLA", "POPU.TRE")
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  14  
#> 2  14.7
#> 3  16.6

# Predict height from age + site index
si_cieszewskibella1991(
  age = c(25, 50, 80),
  si = c(12, 16, 20),
  species = c("PINU.CON", "PICE.GLA", "POPU.TRE")
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   7.21
#> 2  16   
#> 3  25.0 
```
