# Buckman et al. (2006) site index model for red pine

Implementation of the Appendix III site-index equations from Buckman et
al. (2006) for red pine (`PINU.RES`) in the Lake States. Appendix III
presents this model as an improved version of the Lundgren and Dolid
red-pine height function implemented in
[`si_lundgrendolid1970()`](https://ptompalski.github.io/CanadaForestAllometry/reference/si_lundgrendolid1970.md).
Buckman et al. note two imperfections in the earlier Lundgren-Dolid
form: height predicted at age 50 was slightly below site index rather
than equal to it exactly, and the curve overpredicted heights for young
stands below age 20. To address this, they refit the age-20+ equation
with the constraint that height at age 50 equals site index, and they
replace the younger-age portion with a separate polynomial curve that
joins smoothly with the older-age curve at age 20.

## Usage

``` r
si_buckman2006(age, height = NULL, si = NULL)
```

## Arguments

- age:

  Numeric vector. Stand age from seed (years), with `age > 0`.

- height:

  Optional numeric vector. Total stand height (m). If provided, `si` is
  predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years from seed).
  If provided, `height` is predicted.

## Value

A tibble with columns:

- height:

  Predicted total stand height (m), returned when input `si` is
  provided.

- si:

  Predicted site index (m at total age 50), returned when input `height`
  is provided.

## Details

**Species coverage:** `PINU.RES`.

**Geographic use:** Ontario

**Age definition note:** `age` is stand age from seed (years)

**Height definition note:** `height` is total stand height (m).

**Base-age note:** site index is total height at 50 years.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

The source model is piecewise:

- for `age < 20`: \\H = SI (k t^2 - m t^4)\\

- for `age >= 20`: \\H = A SI (1 - e^{-Bt})^C\\

The constrained Buckman refit uses `A = 1.8604`, `B = 0.020928`, and
`C = 1.4349`. The younger-age correction uses `k = 1.41876e-3` and
`m = 1.05304e-6`, chosen so the two segments join smoothly at age 20.

Inputs and outputs are metric; the original equations are in imperial
units, so the function converts internally.

## References

Buckman, R. E., et al. (2006). Growth and yield of red pine in the Lake
States. USDA Forest Service.

Lundgren, A. L., & Dolid, W. A. (1970). Biological growth functions
describe published site index curves for Lake States timber species.
Research Paper NC-36. St. Paul, MN: U.S. Department of Agriculture,
Forest Service, North Central Forest Experiment Station.

## Examples

``` r
# Predict site index from age + height
si_buckman2006(
  age = c(15, 25, 50),
  height = c(4, 10, 18)
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  15.0
#> 2  19.5
#> 3  18.0

# Predict height from age + site index
si_buckman2006(
  age = c(15, 25, 50),
  si = c(14, 16, 18)
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   3.72
#> 2   8.21
#> 3  18.0 

```
