# Nigh (2017) lodgepole pine height-age (site index) model for British Columbia

Unified, vectorized implementation of the grounded-Generalized Algebraic
Difference Approach (g-GADA) height-age (site index) model in Nigh
(2017) for lodgepole pine (*Pinus contorta* var. *latifolia*) in British
Columbia.

## Usage

``` r
si_nigh2017(age, height = NULL, si = NULL)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years).

- height:

  Optional numeric vector. Site height (m). If provided, \`si\` is
  predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years at breast
  height). If provided, \`height\` is predicted. The paper's
  SI-to-parameter conversion is intended for site index in the range
  5–30 m.

## Value

A tibble with a single column:

- height:

  Predicted site height (m), returned when \`si\` is provided.

- si:

  Predicted site index (m), returned when \`height\` is provided.

## Details

**Model scope (species coverage):** lodgepole pine, NFI code `PINU.CON`.

**Age definition note:** \`age\` is breast-height age (BHA, years).

**Base-age note:** site index is site height at breast-height age 50.

**Model form:** the fitted g-GADA model (eq. 4) is \$\$HT = 1.3 +
\beta_0 \left(1 - e^{(\beta\_{10} + \beta\_{11}\beta_0) (BHA -
0.5)}\right)^{\beta\_{20} + \beta\_{21}\beta_0}\$\$ where \\\beta_0\\ is
a tree-specific parameter that localizes the curve and \\\beta\_{10},
\beta\_{11}, \beta\_{20}, \beta\_{21}\\ are the fitted global parameters
(Table 2).

The g-GADA localizes on the asymptote-like parameter \\\beta_0\\, not
directly on site index. This implementation follows the paper's
recommended base-age-50 workflow:

- When \`si\` is supplied, \\\beta_0\\ is obtained from the paper's
  cubic SI-to-\\\beta_0\\ conversion (Discussion, p. 18), then height is
  evaluated with eq. 4. The cubic is valid for site index 5–30 m and has
  a stated maximum \\\beta_0\\ error of 16 cm, so \`height\` equals
  \`si\` at BHA 50 only to within that tolerance (not exactly).

- When \`height\` is supplied, \\\beta_0\\ is calibrated numerically to
  the (BHA, height) pair, then site index is the height predicted at BHA
  50.

Provide exactly one of \`height\` or \`si\`:

- If \`si\` is provided, the function predicts \`height\`.

- If \`height\` is provided, the function predicts \`si\`.

## References

Nigh, G.D. 2017. Development of a lodgepole pine site index model with
the grounded-Generalized Algebraic Difference Approach (g-GADA). Prov.
B.C., Victoria, B.C. Res. Rep. 31.

## Examples

``` r
# Predict height from age + site index
si_nigh2017(age = c(25, 50, 80), si = c(12, 18, 24))
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   6.91
#> 2  18.0 
#> 3  29.9 

# Predict site index from age + height
si_nigh2017(age = c(25, 50, 80), height = c(8, 18, 26))
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  13.8
#> 2  18  
#> 3  20.4
```
