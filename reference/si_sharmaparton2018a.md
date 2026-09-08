# Sharma and Parton (2018) site index model for white spruce plantations

Implementation of the non-climate-sensitive McDill-Amateis dynamic
height equation reported by Sharma and Parton (2018) for
plantation-grown white spruce (`PICE.GLA`) in Ontario.

## Usage

``` r
si_sharmaparton2018a(
  age,
  height = NULL,
  si = NULL,
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

**Species coverage:** `PICE.GLA`.

**Geographic use:** Ontario white spruce plantations.

**Age definition note:** `age` is breast-height age (years).

**Height definition note:** this implementation assumes the source model
uses heights above breast height (m), not total height. For consistency
with other site-index functions in this package, it defaults to using
total height in the public API (`total_height = TRUE`) and converts
internally by subtracting or adding 1.3 m as needed. Set
`total_height = FALSE` to work on the source scale directly.

**Base-age note:** the underlying dynamic equation is written in a
base-age invariant paired-age form. For consistency with other
site-index functions in this package, this implementation defaults to a
base age of 50 years breast-height age, while still allowing users to
supply any positive `base_age`.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

Inputs and outputs are metric and match the source model scale.

Both directions are explicit closed forms derived from the same dynamic
equation.

## References

Sharma, M., & Parton, J. (2018). Analyzing and modelling effects of
climate on site productivity of white spruce plantations. The Forestry
Chronicle, 94, 173-182.

## Examples

``` r
# Predict site index from age + height
si_sharmaparton2018a(
  age = c(20, 30, 40),
  height = c(6, 9, 12)
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  12.2
#> 2  12.7
#> 3  13.2

# Predict height from age + site index
si_sharmaparton2018a(
  age = c(20, 30, 40),
  si = c(8, 10, 12)
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   4.18
#> 2   7.17
#> 3  11.0 
```
