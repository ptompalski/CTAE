# Sharma et al. (2015) site index model for plantation-grown jack pine and black spruce

Implementation of the non-climate-sensitive McDill-Amateis dynamic
height equation reported by Sharma et al. (2015) for plantation-grown
jack pine (`PINU.BAN`) and black spruce (`PICE.MAR`) in northern
Ontario.

## Usage

``` r
si_sharma2015(
  age,
  height = NULL,
  si = NULL,
  species,
  base_age = 50,
  total_height = TRUE
)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years), with `age > 0`.

- height:

  Optional numeric vector. Stand height (m). If `total_height = TRUE`
  (default), this is total height; otherwise it is height above breast
  height. If provided, `si` is predicted.

- si:

  Optional numeric vector. Site index (m above breast height) at
  `base_age` years breast-height age. If provided, `height` is
  predicted.

- species:

  Character vector of species codes (e.g., `"PINU.BAN"` or
  `"PICE.MAR"`).

- base_age:

  Positive numeric scalar. Site-index base age (years at breast height).
  Defaults to `50`.

- total_height:

  Logical scalar. If `TRUE` (default), interpret input `height` as total
  height and return predicted `height` as total height. If `FALSE`, use
  the source-paper scale of height above breast height.

## Value

A tibble with columns:

- height:

  Predicted stand height (m), returned when input `si` is provided. This
  is total height when `total_height = TRUE`, otherwise height above
  breast height.

- si:

  Predicted site index (m above breast height), returned when input
  `height` is provided.

## Details

**Species coverage:** `PINU.BAN`, `PICE.MAR`.

**Geographic use:** northern Ontario plantations.

**Age definition note:** `age` is breast-height age (years).

**Height definition note:** the source model uses heights above breast
height (m), not total height. For consistency with other site-index
functions in this package, this implementation defaults to using total
height in the public API (`total_height = TRUE`) and converts internally
by subtracting or adding 1.3 m as needed. Set `total_height = FALSE` to
work on the original source scale directly.

**Base-age note:** the source paper defines site index at a base age of
25 years breast-height age. The equation itself is a dynamic
McDill-Amateis form written in terms of paired ages and heights, so this
implementation treats `base_age` as user-configurable. For consistency
with other site-index functions in this package, the default is 50 years
breast-height age.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

This implementation uses the fixed-effects no-climate form (Equation 1)
that the paper recommends when climate variables are unavailable.

## References

Sharma, M., Subedi, N., Ter-Mikaelian, M., & Parton, J. (2015). Modeling
climatic effects on stand height/site index of plantation-grown jack
pine and black spruce trees. Forest Science, 61(1), 25-34.
https://doi.org/10.5849/forsci.13-190

## Examples

``` r
# Predict site index from age + total height
si_sharma2015(
  age = c(20, 30),
  height = c(8, 12),
  species = c("PINU.BAN", "PICE.MAR")
)
#> # A tibble: 2 × 1
#>      si
#>   <dbl>
#> 1  14.3
#> 2  15.7

# Predict total height from age + site index
si_sharma2015(
  age = c(20, 30),
  si = c(9, 10),
  species = c("PINU.BAN", "PICE.MAR")
)
#> # A tibble: 2 × 1
#>   height
#>    <dbl>
#> 1   4.94
#> 2   7.58
```
