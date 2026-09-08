# Lundgren and Dolid (1970) site index model

Unified, vectorized implementation of the Lundgren and Dolid (1970) site
index equations for Lake States species.

## Usage

``` r
si_lundgrendolid1970(
  age,
  height = NULL,
  si = NULL,
  species,
  model = c("exponential_monomolecular", "monomolecular")
)
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

  Character vector of species codes (e.g., `"PICE.MAR"`).

- model:

  Character scalar. One of `"exponential_monomolecular"` or
  `"monomolecular"`.

## Value

A tibble with columns:

- height:

  Predicted height (m), returned when input `si` is provided.

- si:

  Predicted site index (m), returned when input `height` is provided.

## Details

Two model forms are available:

- `"exponential_monomolecular"` (default; preferred in the paper)

- `"monomolecular"`

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

Inputs/outputs are metric; the original equations are in imperial units,
so the function converts internally.

## References

Lundgren, Allen L.; Dolid, William A. 1970. Biological growth functions
describe published site index curves for Lake States timber species.
Research Paper NC-36. St. Paul, MN: U.S. Dept. of Agriculture, Forest
Service, North Central Forest Experiment Station

## Examples

``` r
# Predict site index from age + height (default model: exponential_monomolecular)
si_lundgrendolid1970(
  age = c(20, 40, 60),
  height = c(8, 16, 24),
  species = c("PICE.MAR", "PINU.BAN", "BETU.PAP")
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  17.7
#> 2  18.8
#> 3  21.7

# Predict height from age + site index, using the monomolecular alternative
si_lundgrendolid1970(
  age = c(20, 40, 60),
  si = c(12, 18, 24),
  species = c("PICE.MAR", "PINU.BAN", "BETU.PAP")
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   5.43
#> 2  15.3 
#> 3  26.5 

# Works with the pipe operator: predict site index from age + height
trees_h <- tibble::tibble(
  species = c("PICE.MAR", "LARI.LAR", "BETU.PAP"),
  age = c(25, 45, 65),
  height = c(9, 18, 23)
)

trees_h |>
  dplyr::mutate(
    si_pred = si_lundgrendolid1970(
      age = age,
      height = height,
      species = species
    )
  ) |>
  tidyr::unnest(si_pred)
#> # A tibble: 3 × 4
#>   species    age height    si
#>   <chr>    <dbl>  <dbl> <dbl>
#> 1 PICE.MAR    25      9  16.0
#> 2 LARI.LAR    45     18  19.3
#> 3 BETU.PAP    65     23  20.0

# With pipe: predict height from age + site index (monomolecular form)
trees_si <- tibble::tibble(
  species = c("PICE.MAR", "LARI.LAR", "BETU.PAP"),
  age = c(25, 45, 65),
  si = c(14, 20, 22)
)

trees_si |>
  dplyr::mutate(
    height_pred = si_lundgrendolid1970(
      age = age,
      si = si,
      species = species
    )
  ) |>
  tidyr::unnest(height_pred)
#> # A tibble: 3 × 4
#>   species    age    si height
#>   <chr>    <dbl> <dbl>  <dbl>
#> 1 PICE.MAR    25    14   7.87
#> 2 LARI.LAR    45    20  18.7 
#> 3 BETU.PAP    65    22  25.3 
```
