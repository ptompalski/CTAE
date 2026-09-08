# Brisco, Klinka and Nigh (2002) western larch height-age (site index) model

Unified, vectorized implementation of the recommended height-growth
(site index) model in Brisco, Klinka and Nigh (2002) for western larch
(*Larix occidentalis*) in British Columbia.

## Usage

``` r
si_brisco2002(age, height = NULL, si = NULL)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years).

- height:

  Optional numeric vector. Site height (m). If provided, `si` is
  predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years at breast
  height). If provided, `height` is predicted.

## Value

A tibble with a single column:

- height:

  Predicted site height (m), returned when `si` is provided.

- si:

  Predicted site index (m), returned when `height` is provided.

## Details

**Model scope (species coverage):** western larch, NFI code `LARI.OCC`.

**Age definition note:** `age` is breast-height age (BHA, years).

**Base-age note:** site index is site height at breast-height age 50.

**Model form:** the authors compared four model forms and recommend the
unconstrained Chapman-Richards model (their eq. 3) refit to the complete
data set (their eq. 6): \$\$H = 1.3 + b_1 (S - 1.3)^{b_2} \left(1 -
e^{b_3 (A - 0.5)}\right)^{b_4 (S - 1.3)^{b_5}}\$\$ with \\b_1 = 3.875\\,
\\b_2 = 0.7850\\, \\b_3 = -0.01497\\, \\b_4 = 2.193\\, \\b_5 =
-0.2318\\, where \\H\\ is site height (m), \\A\\ is breast-height age
(years), and \\S\\ is site index (m). At \\A = 50\\ the equation returns
\\H = S\\.

Provide exactly one of `height` or `si`:

- If `si` is provided, the function predicts `height` directly from eq.
  6.

- If `height` is provided, the function predicts `si`. Because \\S\\
  appears in the base, the leading power, and the outer exponent of eq.
  6, it cannot be isolated in closed form and is obtained by numerically
  inverting eq. 6 for each (age, height) pair.

## References

Brisco, D., Klinka, K., and Nigh, G. 2002. Height growth models for
western larch in British Columbia. West. J. Appl. For. 17(2):66–74.

## Examples

``` r
# Predict height from age + site index
si_brisco2002(age = c(25, 50, 80), si = c(15, 20, 25))
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   8.67
#> 2  20.1 
#> 3  33.0 

# Predict site index from age + height
si_brisco2002(age = c(25, 50, 80), height = c(10, 20, 28))
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  17.2
#> 2  19.9
#> 3  20.8
```
