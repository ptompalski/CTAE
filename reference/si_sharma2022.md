# Sharma (2022) site index model for black spruce and trembling aspen

Implementation of the fixed-effects component of the no-climate
McDill-Amateis mixed-effects stand height model published by Sharma
(2022) for black spruce (`PICE.MAR`) and trembling aspen (`POPU.TRE`)
growing in natural-origin mixed stands in Ontario.

## Usage

``` r
si_sharma2022(age, height = NULL, si = NULL, species, base_age = 50)
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

  Character vector of species codes (e.g., `"PICE.MAR"` or
  `"POPU.TRE"`).

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

**Species coverage:** `PICE.MAR`, `POPU.TRE`.

**Geographic use:** Ontario natural-origin mixed stands.

**Age definition note:** `age` is breast-height age (years).

**Height definition note:** the source model uses stand height (top
height) in metres.

**Base-age note:** the paper defines site index as stand height at 50
years breast-height age. The underlying dynamic equation is base-age
invariant, so any positive `base_age` can be supplied; the default
remains 50 to match the source definition.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

This implementation uses the fixed-effects component of Sharma's
no-climate mixed-effects model (Equation 2), with stand-level random
effects set to zero.

## References

Sharma, M. (2022). Climate effects on black spruce and trembling aspen
productivity in natural origin mixed stands. Forests, 13(3), 430.
https://doi.org/10.3390/f13030430

## Examples

``` r
# Predict site index from age + height
si_sharma2022(
  age = c(40, 60),
  height = c(13, 18),
  species = c("PICE.MAR", "POPU.TRE")
)
#> # A tibble: 2 × 1
#>      si
#>   <dbl>
#> 1  15.6
#> 2  16.2

# Predict height from age + site index
si_sharma2022(
  age = c(40, 60),
  si = c(15, 18),
  species = c("PICE.MAR", "POPU.TRE")
)
#> # A tibble: 2 × 1
#>   height
#>    <dbl>
#> 1   12.5
#> 2   19.8
```
