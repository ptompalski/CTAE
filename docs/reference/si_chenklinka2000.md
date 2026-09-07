# Chen and Klinka (2000) height-growth / site-index model for ESSF species

Unified, vectorized implementation of the conditioned Chapman-Richards
height-growth models of Chen and Klinka (2000) for high-elevation
subalpine fir, Engelmann spruce, and lodgepole pine in the Engelmann
Spruce-Subalpine Fir (ESSF) zone of British Columbia.

## Usage

``` r
si_chenklinka2000(age, height = NULL, si = NULL, species)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years).

- height:

  Optional numeric vector. Top height (m). If provided, \`si\` is
  predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years at breast
  height). If provided, \`height\` is predicted.

- species:

  Character vector of NFI species codes (\`"ABIE.LAS"\`, \`"PICE.ENG"\`,
  or \`"PINU.CON"\`).

## Value

A tibble with columns:

- height:

  Predicted top height (m), returned when input \`si\` is provided.

- si:

  Predicted site index (m), returned when input \`height\` is provided.

## Details

**Model scope (species coverage):** `ABIE.LAS` (subalpine fir),
`PICE.ENG` (Engelmann spruce), `PINU.CON` (lodgepole pine).

**Age definition note:** \`age\` is breast-height age (years).

**Base-age note:** site index is referenced to top height at 50 years
breast-height age.

The model form (identical for all three species, differing only in the
coefficients \\b_1, b_2, b_3\\) is \$\$H = 1.3 + b_1 \left\[(S -
1.3)^{b_2} (1 - e^{-b_3 A})^p\right\],\$\$ where \$\$p =
\frac{\ln\left((S - 1.3)^{1 - b_2} / b_1\right)}{\ln\left(1 - e^{-b_3
\times 50}\right)}.\$\$

Provide exactly one of \`height\` or \`si\`:

- If \`height\` is provided, the function predicts \`si\` (solved
  numerically; the model has no closed-form inverse).

- If \`si\` is provided, the function predicts \`height\`.

## References

Chen, H.Y.H. and Klinka, K. (2000). Height growth models for
high-elevation subalpine fir, Engelmann spruce, and lodgepole pine in
British Columbia. *Western Journal of Applied Forestry* 15(2): 62-69.

## Examples

``` r
# Predict top height from age + site index
si_chenklinka2000(
  age = c(25, 50, 80),
  si = c(12, 16, 20),
  species = c("ABIE.LAS", "PICE.ENG", "PINU.CON")
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   6.24
#> 2  16   
#> 3  26.3 

# Predict site index from age + top height
si_chenklinka2000(
  age = c(25, 50, 80),
  height = c(8, 16, 24),
  species = c("ABIE.LAS", "PICE.ENG", "PINU.CON")
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  14.7
#> 2  16  
#> 3  18.2
```
