# Parresol and Vissage (1998) site index model for eastern white pine

Implementation of the base-age invariant polymorphic site-index
equations published by Parresol and Vissage (1998) for eastern white
pine (`PINU.STR`).

## Usage

``` r
si_parresolvissage1998(age, height = NULL, si = NULL, base_age = 50)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years), with `age >= 10`.

- height:

  Optional numeric vector. Dominant/codominant height (m). If provided,
  `si` is predicted.

- si:

  Optional numeric vector. Site index (m) at `base_age` years
  breast-height age. If provided, `height` is predicted.

- base_age:

  Positive numeric scalar. Site-index base age (years at breast height),
  with `base_age >= 10`. Defaults to `50`.

## Value

A tibble with columns:

- height:

  Predicted dominant/codominant height (m), returned when input `si` is
  provided.

- si:

  Predicted site index (m), returned when input `height` is provided.

## Details

**Species coverage:** `PINU.STR`.

**Geographic use (Canada):** the source data were adopted for the
southern U.S. forest survey. In Canada, use is most defensible for
eastern white pine in eastern regions, and should be treated cautiously
outside that domain.

**Age definition note:** `age` is breast-height age (years).

**Base-age note:** the source paper derives a base-age invariant system.
This implementation accepts any positive `base_age`; the default is 50
years at breast height.

**Domain note:** although the algebraic form can be evaluated below age
10, the source data and paper discussion indicate reliable behavior for
ages 10 years and greater. This implementation therefore enforces
`age >= 10` and `base_age >= 10`.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

Inputs/outputs are metric; the original equations are in imperial units,
so the function converts internally.

Both directions are explicit closed forms from the source publication.

## References

Parresol, B. R., & Vissage, J. S. (1998). White pine site index for the
southern forest survey. U.S. Department of Agriculture, Forest Service,
Southern Research Station, Research Paper SRS-10.

## Examples

``` r
# Predict site index from age + height
si_parresolvissage1998(
  age = c(25, 50, 70),
  height = c(10, 18, 24)
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  22.2
#> 2  18  
#> 3  18.5

# Predict height from age + site index at a base age of 50 years
si_parresolvissage1998(
  age = c(25, 50, 70),
  si = c(12, 18, 24)
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   4.81
#> 2  18   
#> 3  30.7 

# Predict height using an alternative base age
si_parresolvissage1998(
  age = 35,
  si = 30 / 3.28084,
  base_age = 25
)
#> # A tibble: 1 × 1
#>   height
#>    <dbl>
#> 1   14.3
```
