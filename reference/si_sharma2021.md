# Sharma (2021) site index model for jack pine and black spruce in mixed natural stands

Implementation of the fixed-effects non-climate McDill-Amateis dynamic
height equation reported by Sharma (2021) for jack pine (`PINU.BAN`) and
black spruce (`PICE.MAR`) growing in natural-origin mixed stands in
Ontario.

## Usage

``` r
si_sharma2021(
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

**Geographic use:** northern Ontario natural-origin mixed stands.

**Age definition note:** `age` is breast-height age (years).

**Height definition note:** the source model uses heights above breast
height (m), not total height. For consistency with other site-index
functions in this package, this implementation defaults to using total
height in the public API (`total_height = TRUE`) and converts internally
by subtracting or adding 1.3 m as needed. Set `total_height = FALSE` to
work on the source scale directly.

**Base-age note:** site index is defined at 50 years breast-height age.
The underlying dynamic equation is written in paired-age form, so this
implementation allows any positive `base_age`; the default remains `50`
to match the source definition.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

This implementation uses the fixed-effects no-climate form (Equation 8)
with stand-level random effects omitted.

## References

Sharma, M. (2021). Climate effects on jack pine and black spruce
productivity in natural origin mixed stands and site index conversion
equations. Trees, Forests and People, 5, 100089.
https://doi.org/10.1016/j.tfp.2021.100089

## Examples

``` r
# Predict site index from age + height
si_sharma2021(
  age = c(60, 80),
  height = c(16, 18),
  species = c("PINU.BAN", "PICE.MAR")
)
#> # A tibble: 2 × 1
#>      si
#>   <dbl>
#> 1  13.3
#> 2  12.6

# Predict height from age + site index
si_sharma2021(
  age = c(60, 80),
  si = c(14, 13),
  species = c("PINU.BAN", "PICE.MAR")
)
#> # A tibble: 2 × 1
#>   height
#>    <dbl>
#> 1   16.7
#> 2   18.4
```
