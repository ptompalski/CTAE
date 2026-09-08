# Nigh, Krestov and Klinka (2002) trembling aspen height-age (site index) model for British Columbia

Unified, vectorized implementation of the logistic height-age (site
index) model in Nigh, Krestov and Klinka (2002) for trembling aspen
(*Populus tremuloides*) in British Columbia.

## Usage

``` r
si_nigh2002(age, height = NULL, si = NULL, bec_zone = NULL)
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

- bec_zone:

  Optional character vector selecting the extended (per-zone) model. One
  of \`"BWBS"\`, \`"ICH"\`, \`"IDF"\`, \`"MS"\`, \`"SBPS"\`, \`"SBS"\`.
  When \`NULL\` (default) the general base model is used.

## Value

A tibble with a single column:

- height:

  Predicted site height (m), returned when \`si\` is provided.

- si:

  Predicted site index (m), returned when \`height\` is provided.

## Details

**Model scope (species coverage):** trembling aspen, NFI code
`POPU.TRE`.

**Age definition note:** \`age\` is breast-height age (BHA, years). The
model is conditioned to return \`height = si\` exactly at BHA 50.

**Base-age note:** site index is site height at breast-height age 50.

**Base vs. extended model:** the paper fits one logistic form (eq. 1)
\$\$HT = 1.3 + (SI - 1.3) \times \frac{1 + e^{a_0 + a_1 \ln(49.5) + a_2
\ln(SI - 1.3)}} {1 + e^{a_0 + a_1 \ln(BHA - 0.5) + a_2 \ln(SI -
1.3)}}\$\$ where \\a_0, a_1, a_2\\ are either the general
\*\*base-model\*\* coefficients (used anywhere in BC, or when the
biogeoclimatic zone is unknown) or the \*\*extended-model\*\* per-zone
coefficients calibrated to six BEC zones. Supply \`bec_zone\` to use the
extended model; leave it \`NULL\` (the default) for the base model.

Because \\SI\\ appears both as a multiplier and inside a logarithm, eq.
1 has no closed-form inverse in \\SI\\; when predicting site index the
equation is solved numerically.

Provide exactly one of \`height\` or \`si\`:

- If \`si\` is provided, the function predicts \`height\`.

- If \`height\` is provided, the function predicts \`si\`.

## References

Nigh, G.D., Krestov, P.V., and Klinka, K. (2002). Trembling aspen
height-age models for British Columbia. Northwest Science 76(3):
202–212.

## Examples

``` r
# Base model: predict height from age + site index
si_nigh2002(age = c(25, 50, 80), si = c(12, 18, 24))
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   7.18
#> 2  18   
#> 3  29.8 

# Base model: predict site index from age + height
si_nigh2002(age = c(25, 50, 80), height = c(8, 18, 26))
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  13.3
#> 2  18  
#> 3  20.6

# Extended model (per BEC zone)
si_nigh2002(age = c(30, 60), si = c(15, 20), bec_zone = "BWBS")
#> # A tibble: 2 × 1
#>   height
#>    <dbl>
#> 1   10.7
#> 2   22.0
```
