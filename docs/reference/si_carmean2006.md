# Carmean, Hazenberg and Deschamps (2006) polymorphic site index models for northwest Ontario

Unified, vectorized implementation of the Newnham (1988) constrained
polymorphic height-age (site index) model published in Carmean,
Hazenberg and Deschamps (2006) for black spruce (*Picea mariana*) and
trembling aspen (*Populus tremuloides*) in northwest Ontario.

## Usage

``` r
si_carmean2006(age, height = NULL, si = NULL, species)
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

- species:

  Character vector of NFI species codes: \`"PICE.MAR"\` (black spruce)
  or \`"POPU.TRE"\` (trembling aspen).

## Value

A tibble with a single column:

- height:

  Predicted site height (m), returned when \`si\` is provided.

- si:

  Predicted site index (m), returned when \`height\` is provided.

## Details

**Model scope (species coverage):** black spruce (`PICE.MAR`) and
trembling aspen (`POPU.TRE`).

**Age definition note:** \`age\` is breast-height age (years). Curves
start at breast height (0 years at BH) and the model is constrained so
that \`height = si\` exactly at breast-height age 50.

**Base-age note:** site index is height (m) at 50 years breast-height
age.

**Domain note:** the curves were fitted to data 100 years and less
breast-height age; the source notes they may be extended to about 150
years with reduced precision.

**Source legibility caveat:** the published PDF renders equations 1 and
2 (p. 7) as low-quality raster images, and the printed coefficient
exponents are partly degraded. For black spruce the exponent \\b_2\\
could not be read unambiguously from the source raster and is
transcribed as its best reading, \\0.1136\\. Treat the black spruce
coefficients (in particular \\b_2\\) as provisional.

The model form is \$\$\hat{H} = 1.3 + b_1 (S - 1.3)^{b_2} \left\[1 -
k^{Age/50}\right\]^{b_3 (S - 1.3)^{b_4}}\$\$ with \$\$k = 1 -
\left\[\frac{S - 1.3}{b_1 (S - 1.3)^{b_2}}\right\]^ {1 / (b_3 (S -
1.3)^{b_4})}.\$\$ Because \\S\\ appears in several nonlinear positions
the model has no closed-form inverse in \\S\\; when predicting site
index the equation is solved numerically.

Provide exactly one of \`height\` or \`si\`:

- If \`si\` is provided, the function predicts \`height\`.

- If \`height\` is provided, the function predicts \`si\`.

## References

Carmean, W.H., Hazenberg, G., and Deschamps, K.C. (2006). Polymorphic
site index curves for black spruce and trembling aspen in northwest
Ontario. The Forestry Chronicle 82(2): 213–231.

Newnham, R.M. (1988). A modification of the Ek-Payandeh nonlinear
regression model for site index curves. Canadian Journal of Forest
Research 18: 115–120.

## Examples

``` r
# Predict height from age + site index
si_carmean2006(
  age = c(25, 50, 80),
  si = c(12, 17, 20),
  species = c("PICE.MAR", "PICE.MAR", "POPU.TRE")
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   6.88
#> 2  17   
#> 3  25.1 

# Predict site index from age + height
si_carmean2006(
  age = c(25, 50, 80),
  height = c(8, 17, 24),
  species = c("PICE.MAR", "PICE.MAR", "POPU.TRE")
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  13.9
#> 2  17  
#> 3  19.1
```
