# Nigh (1997) Sitka spruce height-age (site index) model for coastal British Columbia

Vectorized implementation of the logistic height-age (site index) model
in Nigh (1997) for Sitka spruce (*Picea sitchensis*) in coastal British
Columbia. The model was developed from stem-analysis data collected in
the Queen Charlotte Islands and is recommended for use in British
Columbia because of its improved extrapolation to old ages.

## Usage

``` r
si_nigh1997(age, height = NULL, si = NULL)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years).

- height:

  Optional numeric vector. Site height (m). If provided, \`si\` is
  predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years at breast
  height). If provided, \`height\` is predicted.

## Value

A tibble with a single column:

- height:

  Predicted site height (m), returned when \`si\` is provided.

- si:

  Predicted site index (m), returned when \`height\` is provided.

## Details

**Model scope (species coverage):** Sitka spruce, NFI code `PICE.SIT`.

**Age definition note:** \`age\` is breast-height age (BHA, years). The
model is conditioned to return \`height = si\` exactly at BHA 50.

**Base-age note:** site index is site height at breast-height age 50.

The recommended model (Nigh 1997, eq. 8) is the integral logistic form
\$\$H = 1.3 + (SI - 1.3) \times \frac{1 + e^{a_0 + a_1 \ln(49.5) + a_2
\ln(SI - 1.3)}} {1 + e^{a_0 + a_1 \ln(BHA - 0.5) + a_2 \ln(SI -
1.3)}}\$\$ with \\a_0 = 8.947\\, \\a_1 = -1.357\\, \\a_2 = -1.013\\
(Table 3, model \[7\]).

Because \\SI\\ appears both as a multiplier and inside a logarithm, the
equation has no closed-form inverse in \\SI\\; when predicting site
index the equation is solved numerically.

Provide exactly one of \`height\` or \`si\`:

- If \`si\` is provided, the function predicts \`height\`.

- If \`height\` is provided, the function predicts \`si\`.

## References

Nigh, G.D. (1997). A Sitka spruce height-age model with improved
extrapolation properties. The Forestry Chronicle 73(3): 363–369.

## Examples

``` r
# Predict height from age + site index
si_nigh1997(age = c(25, 50, 80), si = c(20, 30, 38))
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   10.4
#> 2   30  
#> 3   49.4

# Predict site index from age + height
si_nigh1997(age = c(25, 50, 80), height = c(15, 30, 42))
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  27.8
#> 2  30  
#> 3  31.4
```
