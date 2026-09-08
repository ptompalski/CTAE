# Ker and Bowling (1991) site index model

Unified, vectorized implementation of the Ker and Bowling (1991)
polymorphic site-index equation (model 1, conditioned form; eq. 9) for
New Brunswick softwoods.

## Usage

``` r
si_kerbowling1991(age, height = NULL, si = NULL, species)
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

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

This implementation uses the conditioned equation at index age 50 years
(breast-height age), so predicted height equals site index at
`age = 50`.

Predicting `height` from `si` is direct. Predicting `si` from `height`
is implicit (site index also appears in the exponent term), so there is
no closed-form algebraic inverse. For that mode, this implementation
solves for `si` numerically with
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html).

Inputs/outputs are metric (m), matching the source model.

## References

Ker, M. F., & Bowling, C. (1991). Polymorphic site index equations for
four New Brunswick softwood species. *Canadian Journal of Forest
Research*, 21(5), 728-732.

## Examples

``` r
# Predict site index from age + height
si_kerbowling1991(
  age = c(25, 40, 60),
  height = c(8, 14, 20),
  species = c("ABIE.BAL", "PICE.MAR", "PINU.BAN")
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  14.1
#> 2  16.3
#> 3  18.3

# Predict height from age + site index
si_kerbowling1991(
  age = c(25, 40, 60),
  si = c(11, 13, 16),
  species = c("ABIE.BAL", "PICE.MAR", "PINU.BAN")
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   5.88
#> 2  10.9 
#> 3  17.6 

# Works with the pipe operator: predict site index from age + height
trees_h <- tibble::tibble(
  species = c("ABIE.BAL", "PICE.MAR", "PINU.BAN"),
  age = c(25, 40, 60),
  height = c(8, 14, 20)
)

trees_h |>
  dplyr::mutate(
    si_pred = si_kerbowling1991(
      age = age,
      height = height,
      species = species
    )
  ) |>
  tidyr::unnest(si_pred)
#> # A tibble: 3 × 4
#>   species    age height    si
#>   <chr>    <dbl>  <dbl> <dbl>
#> 1 ABIE.BAL    25      8  14.1
#> 2 PICE.MAR    40     14  16.3
#> 3 PINU.BAN    60     20  18.3

# With pipe: predict height from age + site index
trees_si <- tibble::tibble(
  species = c("ABIE.BAL", "PICE.MAR", "PINU.BAN"),
  age = c(25, 40, 60),
  si = c(11, 13, 16)
)

trees_si |>
  dplyr::mutate(
    height_pred = si_kerbowling1991(
      age = age,
      si = si,
      species = species
    )
  ) |>
  tidyr::unnest(height_pred)
#> # A tibble: 3 × 4
#>   species    age    si height
#>   <chr>    <dbl> <dbl>  <dbl>
#> 1 ABIE.BAL    25    11   5.88
#> 2 PICE.MAR    40    13  10.9 
#> 3 PINU.BAN    60    16  17.6 
```
