# Alemdag (1991) national site-index and height-growth model for white spruce

Vectorized implementation of the national (Canada-wide) site-index and
height-growth equations of Alemdag (1991) for white spruce (*Picea
glauca* (Moench) Voss), developed from stem-analysis data pooled across
the two territories and eight provinces (no data from British Columbia
or Nova Scotia), from the Yukon to Newfoundland.

## Usage

``` r
si_alemdag1991(age, height = NULL, si = NULL)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years).

- height:

  Optional numeric vector. Total tree height (m). If provided, \`si\` is
  predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years at breast
  height). If provided, \`height\` is predicted.

## Value

A tibble with a single column:

- height:

  Predicted total tree height (m), returned when \`si\` is provided.

- si:

  Predicted site index (m), returned when \`height\` is provided.

## Details

**Model scope (species coverage):** white spruce, NFI code `PICE.GLA`.
This is a single national model with no species argument.

**Age definition note:** \`age\` is breast-height age (years), i.e.
rings counted at 1.30 m. Both equations use height and site index
measured above breast height (\\H - 1.30\\, \\S - 1.30\\).

**Base-age note:** site index is total tree height at index (base) age
\\A_1 = 50\\ years breast-height age. Both equations are conditioned so
that the predicted value equals the input at \\A = 50\\ (\\S = H\\
there).

The site-index equation (Alemdag 1991, model \[9\], \\c\\ coefficients)
is \$\$S = 1.30 + \frac{1}{c_1 (H - 1.30)^{c_2} \left(1 -
m^{A/A_1}\right)^{c_4 (H - 1.30)^{c_5}}},\$\$ with (model \[8\]) \$\$m =
1 - \left\[\frac{1}{c_1 (H - 1.30)^{1 + c_2}}\right\] ^{1 / (c_4 (H -
1.30)^{c_5})}.\$\$

The height-growth equation (Alemdag 1991, model \[4\], \\b\\
coefficients) is \$\$H = 1.30 + b_1 (S - 1.30)^{b_2} \left(1 -
k^{A/A_1}\right)^{b_4 (S - 1.30)^{b_5}},\$\$ with (model \[5\]) \$\$k =
1 - \left\[\frac{S - 1.30}{b_1 (S - 1.30)^{b_2}}\right\] ^{1 / (b_4 (S -
1.30)^{b_5})}.\$\$

The two equations were fitted independently (they are not exact inverses
of one another), each on the combined national data. Both directions are
closed-form.

Provide exactly one of \`height\` or \`si\`:

- If \`si\` is provided, the function predicts \`height\` (model \[4\]).

- If \`height\` is provided, the function predicts \`si\` (model \[9\]).

## References

Alemdag, I.S. (1991). National site-index and height-growth curves for
white spruce growing in natural stands in Canada. Canadian Journal of
Forest Research 21(10): 1466–1474.
[doi:10.1139/x91-206](https://doi.org/10.1139/x91-206)

## Examples

``` r
# Predict height from age + site index
si_alemdag1991(age = c(25, 50, 80), si = c(12, 15, 18))
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   6.01
#> 2  15   
#> 3  24.5 

# Predict site index from age + height
si_alemdag1991(age = c(25, 50, 80), height = c(9, 15, 20))
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  15.8
#> 2  15.0
#> 3  14.2
```
