# Huang et al. (1994) site index model for major Alberta tree species

Implementation of the ecologically based, reference-age invariant
polymorphic site-index equations from Huang et al. (1994) for Alberta.

## Usage

``` r
si_huang1994(age, height = NULL, si = NULL, species, subregion = "All")
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

- subregion:

  Character vector. Alberta natural-region grouping used by Huang et al.
  (1994). Defaults to `"All"` (provincial parameter set).

## Value

A tibble with columns:

- height:

  Predicted top height (m), returned when input `si` is provided.

- si:

  Predicted site index (m), returned when input `height` is provided.

## Details

**Model scope (species coverage):**
`PICE.GLA, PINU.CON, POPU.TRE, PICE.MAR, PINU.BAN, POPU.BAL, ABIE.BAL, PSEU.MEN`.

**Age definition note:** `age` is breast-height age (years).

**Base-age note:** site index is referenced to 50 years breast-height
age for all species in this implementation.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

## References

Huang, S., Titus, S. J., & Lakusta, T. W. (1994). Ecologically based
site index curves and tables for major Alberta tree species. Alberta
Environmental Protection, Land and Forest Service.

## Examples

``` r
# Predict site index from age + height, provincial parameters
si_huang1994(
  age = c(20, 35, 50),
  height = c(8, 14, 20),
  species = c("PICE.GLA", "PINU.CON", "POPU.TRE")
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  18.8
#> 2  17.8
#> 3  20  

# Predict height from age + site index with explicit subregions
si_huang1994(
  age = c(20, 35, 50),
  si = c(12, 16, 20),
  species = c("PICE.GLA", "PINU.CON", "POPU.TRE"),
  subregion = c("LF", "UF", "CM")
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   4.59
#> 2  12.5 
#> 3  20   

# Pipe example: predict SI from a tibble of age/height/species inputs
dplyr::tibble(
  age = c(25, 35),
  height = c(11, 16),
  species = c("PICE.GLA", "ABIE.BAL")
) |>
  dplyr::mutate(
    si = si_huang1994(
      age = age,
      height = height,
      species = species,
      subregion = "All"
    )
  ) |>
  tidyr::unnest(si)
#> # A tibble: 2 × 4
#>     age height species     si
#>   <dbl>  <dbl> <chr>    <dbl>
#> 1    25     11 PICE.GLA  20.4
#> 2    35     16 ABIE.BAL  21.1
```
