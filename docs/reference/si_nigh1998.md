# Nigh (1998) height-age (site index) model for interior western hemlock

Vectorized implementation of the log-logistic height–breast-height-age
(site index) model in Nigh (1998) for western hemlock (`TSUG.HET`) in
the interior of British Columbia.

## Usage

``` r
si_nigh1998(age, height = NULL, si = NULL)
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

**Model scope (species coverage):** western hemlock, NFI code
`TSUG.HET`.

**Age definition note:** \`age\` is breast-height age (BHA, years). The
model is conditioned to return \`height = si\` exactly at BHA 50.

**Base-age note:** site index is site height at breast-height age 50.

The fitted model (eq. 6) is the log-logistic form \$\$H = 1.3 + (SI -
1.3) \times \frac{1 + e^{b_0 + b_1 \ln(49.5) + b_2 \ln(SI - 1.3)}} {1 +
e^{b_0 + b_1 \ln(A - 0.5) + b_2 \ln(SI - 1.3)}}\$\$ with \\b_0 =
8.998\\, \\b_1 = -1.434\\, \\b_2 = -1.051\\, where \`A\` is
breast-height age (years).

Because \\SI\\ appears both as a multiplier and inside a logarithm, the
equation has no closed-form inverse in \\SI\\; when predicting site
index the equation is solved numerically.

Provide exactly one of \`height\` or \`si\`:

- If \`si\` is provided, the function predicts \`height\`.

- If \`height\` is provided, the function predicts \`si\`.

## References

Nigh, G.D. (1998). A system for estimating height and site index of
western hemlock in the interior of British Columbia. The Forestry
Chronicle 74(4): 588–596.

## Examples

``` r
# Predict height from age + site index
si_nigh1998(age = c(25, 50, 80), si = c(12, 18, 24))
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   6.07
#> 2  18   
#> 3  32.0 

# Predict site index from age + height
si_nigh1998(age = c(25, 50, 80), height = c(8, 18, 26))
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  15.6
#> 2  18  
#> 3  18.7
```
