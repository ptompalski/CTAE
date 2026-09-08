# Sharma and Parton (2019) site index model for white pine plantations

Implementation of the non-climate-sensitive McDill-Amateis dynamic
height equation fitted by Sharma and Parton (2019) for plantation-grown
eastern white pine (`PINU.STR`) in Ontario.

## Usage

``` r
si_sharmaparton2019(
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

**Species coverage:** `PINU.STR`.

**Geographic use:** Ontario white pine plantations.

**Age definition note:** `age` is breast-height age (years).

**Height definition note:** the source model uses heights above breast
height (m), not total height. For consistency with other site-index
functions in this package, this implementation defaults to using total
height in the public API (`total_height = TRUE`) and converts internally
by subtracting or adding 1.3 m as needed. Set `total_height = FALSE` to
work on the original source scale directly.

**Base-age note:** the underlying dynamic equation is base-age
invariant. The source paper illustrates site-index curves using a base
age of 25 years breast-height age. For consistency with other site-index
functions in this package, this implementation instead defaults to a
base age of 50 years breast-height age. Any positive `base_age` can be
supplied.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

Inputs and outputs are metric and match the source model scale.

Both directions are explicit closed forms derived from the same dynamic
equation.

## References

Sharma, M., & Parton, J. (2019). Modelling the effects of climate on
site productivity of white pine plantations. Canadian Journal of Forest
Research, 49, 1289-1297. https://doi.org/10.1139/cjfr-2019-0165

## Examples

``` r
# Predict site index from age + height
si_sharmaparton2019(
  age = c(20, 25, 40),
  height = c(7, 9, 15)
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  13.3
#> 2  14.4
#> 3  16.6

# Predict height from age + site index (default base age 50)
si_sharmaparton2019(
  age = c(20, 25, 40),
  si = c(8, 10, 12)
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   4.58
#> 2   6.49
#> 3  11.1 

# Predict height using an alternative base age
si_sharmaparton2019(
  age = 35,
  si = 12,
  base_age = 25
)
#> # A tibble: 1 × 1
#>   height
#>    <dbl>
#> 1   17.4
```
