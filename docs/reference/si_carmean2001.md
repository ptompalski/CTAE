# Carmean, Niznowski and Hazenberg (2001) polymorphic site index model for northern Ontario

Unified, vectorized implementation of the Newnham (1988) constrained
polymorphic height-age (site index) model published in Carmean,
Niznowski and Hazenberg (2001) for jack pine (*Pinus banksiana*) in
northern Ontario.

## Usage

``` r
si_carmean2001(age, height = NULL, si = NULL)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years), with \`age \> 0\`.

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

**Model scope (species coverage):** jack pine (`PINU.BAN`).

**Age definition note:** \`age\` is breast-height age (years). Curves
start at breast height (0 years at BH) and the model is constrained so
that \`height = si\` exactly at breast-height age 50.

**Base-age note:** site index is total height (m) of dominant and
codominant trees at 50 years breast-height age.

**Domain note:** the recommended equation was fitted to data 100 years
and less breast-height age, combining 383 plots across the Northwestern,
North Central, Northern and Northeastern regions of northern Ontario.
Site index ranged 7.6-22.4 m and breast-height age 50-157 years in the
fitting data (Table 1).

The model form (eq. 1) is \$\$\hat{H} = 1.3 + b_1 (SI - 1.3)^{b_2}
\left\[1 - k^{Age/50}\right\]^{b_3 (SI - 1.3)^{b_4}}\$\$ with \$\$k =
1 - \left\[\frac{SI - 1.3}{b_1 (SI - 1.3)^{b_2}}\right\]^ {1 / (b_3
(SI - 1.3)^{b_4})}.\$\$ Because \\SI\\ appears in several nonlinear
positions the model has no closed-form inverse in \\SI\\; when
predicting site index the equation is solved numerically.

Provide exactly one of \`height\` or \`si\`:

- If \`si\` is provided, the function predicts \`height\`.

- If \`height\` is provided, the function predicts \`si\`.

This model is specific to jack pine (\`PINU.BAN\`); the species is fixed
and there is no \`species\` argument.

## References

Carmean, W.H., Niznowski, G.P., and Hazenberg, G. (2001). Polymorphic
site index curves for jack pine in northern Ontario. The Forestry
Chronicle 77(1): 141–150.

Newnham, R.M. (1988). A modification of the Ek-Payandeh nonlinear
regression model for site index curves. Canadian Journal of Forest
Research 18: 115–120.

## Examples

``` r
# Predict height from age + site index
si_carmean2001(age = c(25, 50, 80), si = c(12, 16, 20))
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   7.53
#> 2  16   
#> 3  23.9 

# Predict site index from age + height
si_carmean2001(age = c(25, 50, 80), height = c(8, 16, 22))
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  12.7
#> 2  16  
#> 3  18.2
```
