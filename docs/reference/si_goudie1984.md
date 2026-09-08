# Goudie (1984) lodgepole pine and white spruce height-age (site index) model

Unified, vectorized implementation of the Goudie (1984) logistic
height-age (site index) curves for lodgepole pine (*Pinus contorta*) and
white spruce (*Picea glauca*) in British Columbia.

## Usage

``` r
si_goudie1984(age, height = NULL, si = NULL, species)
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

- species:

  Character vector of species codes. One of \`"PINU.CON"\` (lodgepole
  pine) or \`"PICE.GLA"\` (white spruce); compact and jurisdiction codes
  are standardized.

## Value

A tibble with a single column:

- height:

  Predicted site height (m), returned when \`si\` is provided.

- si:

  Predicted site index (m), returned when \`height\` is provided.

## Details

**Model scope (species coverage):** lodgepole pine, NFI code `PINU.CON`;
white spruce, NFI code `PICE.GLA`.

**Age definition note:** \`age\` is breast-height age (BHA, years). The
curve is conditioned so that \`height = si\` at BHA 50.

**Base-age note:** site index is site height at breast-height age 50.

**Implementation basis (differs from the original publication):** the
coefficients and functional form here incorporate two later
modifications, not the equations as originally printed in Goudie (1984):

- **Half-year age shift.** A 2004 modification subtracts 0.5 year from
  both the age and the base age inside the logistic (using
  \\\ln(\mathrm{BHA} - 0.5)\\ and \\\ln(49.5)\\), so that height equals
  1.3 m at BHA 0.5 years. The original paper uses \\\ln(\mathrm{BHA})\\
  and \\\ln(50)\\.

- **Lodgepole pine: dry-site coefficients only.** Goudie (1984) eq. 7
  gives habitat-specific pine coefficients (dry vs. wet site). This
  implementation uses only the *dry-site* coefficients, which the author
  recommends when no ecological information is available. There is no
  habitat argument; white spruce is unaffected.

The height-age curve is \$\$HT = 1.3 + (SI - 1.3) \times \frac{1 +
e^{b_1 + b_2 \ln(49.5) - b_3 \ln(SI - 1.3)}} {1 + e^{b_1 + b_2 \ln(BHA -
0.5) - b_3 \ln(SI - 1.3)}}\$\$ with per-species coefficients \\b_1, b_2,
b_3\\. Because \\SI\\ appears both as a multiplier and inside a
logarithm, the curve has no closed-form inverse in \\SI\\; when
predicting site index the equation is solved numerically.

Provide exactly one of \`height\` or \`si\`:

- If \`si\` is provided, the function predicts \`height\`.

- If \`height\` is provided, the function predicts \`si\`.

## References

Goudie, J.W. (1984). Height Growth and Site Index Curves for Lodgepole
Pine and White Spruce and Interim Managed Stand Yield Tables for
Lodgepole Pine in British Columbia. Final Report FY-1983-84. Research
Branch, British Columbia Ministry of Forests, Victoria, B.C.

## Examples

``` r
# Predict height from age + site index
si_goudie1984(age = c(25, 50, 80), si = c(12, 18, 24),
              species = c("PINU.CON", "PICE.GLA", "PINU.CON"))
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   6.98
#> 2  18   
#> 3  29.3 

# Predict site index from age + height
si_goudie1984(age = c(25, 50, 80), height = c(8, 18, 26),
              species = "PICE.GLA")
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  15.9
#> 2  18  
#> 3  18.6
```
