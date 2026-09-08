# Sharma and Reid (2018) site index model for natural jack pine and black spruce

Implementation of the fixed-effects McDill-Amateis dynamic stand-height
model published by Sharma and Reid (2018) for natural-origin jack pine
(`PINU.BAN`) and black spruce (`PICE.MAR`) stands in northern Ontario.

## Usage

``` r
si_sharmareid2018(age, height = NULL, si = NULL, species, base_age = 50)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years), with `age > 0`.

- height:

  Optional numeric vector. Stand height (m). If provided, `si` is
  predicted.

- si:

  Optional numeric vector. Site index (m) at `base_age` years
  breast-height age. If provided, `height` is predicted.

- species:

  Character vector of species codes (e.g., `"PINU.BAN"` or
  `"PICE.MAR"`).

- base_age:

  Positive numeric scalar. Site-index base age (years at breast height).
  Defaults to `50`.

## Value

A tibble with columns:

- height:

  Predicted stand height (m), returned when input `si` is provided.

- si:

  Predicted site index (m), returned when input `height` is provided.

## Details

**Species coverage:** `PINU.BAN`, `PICE.MAR`.

**Geographic use:** northern Ontario natural stands.

**Age definition note:** `age` is breast-height age (years).

**Height definition note:** the source model uses stand height (top
height) in metres measured from breast height.

**Base-age note:** the source paper recommends breast-height age 50
years as the operational base age for both species in Ontario. The
underlying dynamic equation is base-age invariant, so any positive
`base_age` can be supplied.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

This implementation uses Equation 1 from Sharma and Reid (2018), i.e.
the fixed-effects form without random effects or autocorrelation. The
paper recommends this form when stand-level repeated measurements are
unavailable.

## References

Sharma, M., & Reid, D. E. B. (2018). Stand height/site index equations
for jack pine and black spruce trees grown in natural stands. Forest
Science, 64(1), 33-40. https://doi.org/10.5849/FS-2016-133

## Examples

``` r
# Predict site index from age + height
si_sharmareid2018(
  age = c(40, 60),
  height = c(12, 15),
  species = c("PINU.BAN", "PICE.MAR")
)
#> # A tibble: 2 × 1
#>      si
#>   <dbl>
#> 1  13.9
#> 2  13.3

# Predict height from age + site index
si_sharmareid2018(
  age = c(40, 60),
  si = c(16, 14),
  species = c("PINU.BAN", "PICE.MAR")
)
#> # A tibble: 2 × 1
#>   height
#>    <dbl>
#> 1   14.1
#> 2   15.7
```
