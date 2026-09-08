# Carmean and Hahn (1981) site index model

Unified, vectorized implementation of the Carmean and Hahn (1981)
site-index equations for Lake States balsam fir and white spruce.

## Usage

``` r
si_carmeanhahn1981(age, height = NULL, si = NULL, species)
```

## Arguments

- age:

  Numeric vector. Total age (years).

- height:

  Optional numeric vector. Total tree height (m). If provided, `si` is
  predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years at total
  age). If provided, `height` is predicted.

- species:

  Character vector of species codes (e.g., `"ABIE.BAL"`).

## Value

A tibble with columns:

- height:

  Predicted height (m), returned when input `si` is provided.

- si:

  Predicted site index (m), returned when input `height` is provided.

## Details

**Model scope (species coverage):** this implementation includes
parameter sets for 2 species: `ABIE.BAL, PICE.GLA`.

**Geographic use:** this model was fit to revised Lake States harmonized
curves and should be used with caution outside that domain.

**Age definition note:** `age` is *total age* (years), not breast-height
age.

**Base-age note:** site index in this model is total height at 50 years
total age.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

Inputs/outputs are metric; the original equations are in imperial units,
so the function converts internally.

## References

Carmean, W. H., & Hahn, J. T. (1981). Revised site index curves for
balsam fir and white spruce in the Lake States. U.S. Department of
Agriculture, Forest Service, North Central Forest Experiment Station.

## Examples

``` r
# Predict site index from age + height
si_carmeanhahn1981(
  age = c(30, 50, 70),
  height = c(8, 15, 22),
  species = c("ABIE.BAL", "ABIE.BAL", "PICE.GLA")
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  12.8
#> 2  15.0
#> 3  16.4

# Predict height from age + site index
si_carmeanhahn1981(
  age = c(30, 50, 70),
  si = c(12, 16, 20),
  species = c("ABIE.BAL", "ABIE.BAL", "PICE.GLA")
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   7.46
#> 2  16.0 
#> 3  26.0 
```
