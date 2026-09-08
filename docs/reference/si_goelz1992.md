# Goelz and Burk (1992) base-age invariant site index model for jack pine in north central Ontario

Unified, vectorized implementation of the base-age invariant version of
the Chapman-Richards difference equation (eq. 16) published in Goelz and
Burk (1992) for jack pine (*Pinus banksiana*) in north central Ontario.

## Usage

``` r
si_goelz1992(age, height = NULL, si = NULL)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years), with \`age \> 0\`.

- height:

  Optional numeric vector. Site height (m), with \`height \> 1.3\`. If
  provided, \`si\` is predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years at breast
  height), with \`si \> 1.3\`. If provided, \`height\` is predicted.

## Value

A tibble with a single column:

- height:

  Predicted site height (m), returned when \`si\` is provided.

- si:

  Predicted site index (m), returned when \`height\` is provided.

## Details

**Model scope (species coverage):** jack pine (`PINU.BAN`).

**Age definition note:** \`age\` is breast-height age (years). Heights
are referenced to a breast height of 1.3 m.

**Base-age note:** the equation is base-age invariant. Site index is
returned as the predicted height (m) at a breast-height age of 50 years,
the reference base age used throughout Goelz and Burk (1992).

**Domain note:** the equation was fitted to 109 plots (32 held out for
validation) in north central Ontario. Breast-height ages of roughly
20-80 years represent the range likely applied to jack pine in the
region.

The model form (eq. 16) predicts height \\\hat{H}\_2\\ at breast-height
age \\A_2\\ from a known height \\H_1\\ at breast-height age \\A_1\\:
\$\$\hat{H}\_2 = 1.3 + (H_1 - 1.3) \frac{\left\[1 - \exp\left(-b_1
(H_1/A_1)^{b_2} A_1^{b_3} A_2\right)\right\]^{b_4}} {\left\[1 -
\exp\left(-b_1 (H_1/A_1)^{b_2} A_1^{b_3} A_1\right)\right\]^{b_4}}.\$\$
Because the equation is base-age invariant, the same form is used in
both directions: site index is obtained by setting \\A_2 = 50\\, and
height at a given age is obtained by setting \\H_1 = SI\\, \\A_1 = 50\\.

**Note on approximate invariance:** eq. 16 was not constrained to be
exactly base-age invariant (Goelz and Burk 1992, criterion 7).
Predicting height from site index and then predicting site index back
from that height therefore need not recover the original value exactly;
the round-trip discrepancy is small (the source reports curve
differences across base ages of less than 0.5 m).

Provide exactly one of \`height\` or \`si\`:

- If \`si\` is provided, the function predicts \`height\` at \`age\`.

- If \`height\` is provided, the function predicts \`si\` (height at
  base age 50) from the observed (\`age\`, \`height\`) pair.

This model is specific to jack pine (\`PINU.BAN\`); the species is fixed
and there is no \`species\` argument.

## References

Goelz, J.C.G., and Burk, T.E. (1992). Development of a well-behaved site
index equation: jack pine in north central Ontario. Canadian Journal of
Forest Research 22: 776–784.

## Examples

``` r
# Predict height from age + site index
si_goelz1992(age = c(20, 50, 80), si = c(16, 16, 16))
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   8.85
#> 2  16   
#> 3  19.8 

# Predict site index from age + height
si_goelz1992(age = c(20, 50, 80), height = c(8.83, 16, 19.82))
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  15.8
#> 2  16  
#> 3  15.6
```
