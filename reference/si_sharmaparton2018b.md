# Sharma and Parton (2018) site index model for red pine plantations

Implementation of the non-climate-sensitive McDill-Amateis dynamic
height equation reported by Sharma and Parton (2018) for
plantation-grown red pine (`PINU.RES`) in Ontario.

## Usage

``` r
si_sharmaparton2018b(
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

**Species coverage:** `PINU.RES`.

**Geographic use:** Ontario red pine plantations.

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

Sharma, M., & Parton, J. (2018). Climatic effects on site productivity
of red pine plantations. Forest Science, 64(5), 544-554.

## Examples

``` r
# Predict site index from age + height
si_sharmaparton2018b(
  age = c(20, 30, 40),
  height = c(7, 10, 13)
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  13.6
#> 2  13.7
#> 3  14.1

# Predict height from age + site index
si_sharmaparton2018b(
  age = c(20, 30, 40),
  si = c(9, 11, 13)
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   4.77
#> 2   8.04
#> 3  12.0 
```
