# Nigh (2004) juvenile height-age (site index) model for British Columbia

Unified, vectorized implementation of the juvenile height-age (site
index) models in Nigh (2004) for lodgepole pine (*Pinus contorta* var.
*latifolia*) and interior spruce (*Picea glauca*, *P. engelmannii*, and
their hybrid) in British Columbia.

## Usage

``` r
si_nigh2004(age, species, height = NULL, si = NULL, bec_zone = NULL)
```

## Arguments

- age:

  Numeric vector. Total age (years).

- species:

  Character vector of NFI species codes: \`"PINU.CON"\` (lodgepole pine)
  or \`"PICE.GLA"\` (interior spruce). Recycled to a common length with
  the other inputs.

- height:

  Optional numeric vector. Site height (m). If provided, \`si\` is
  predicted.

- si:

  Optional numeric vector. Site index (m). If provided, \`height\` is
  predicted.

- bec_zone:

  Optional character vector selecting zone-specific coefficients. One of
  \`"BWBS"\`, \`"ESSF"\`, \`"ICH"\`, \`"IDF"\`, \`"MS"\`, \`"SBS"\`,
  \`"SBPS"\`. When \`NULL\` (default), the province-wide parameters are
  used.

## Value

A tibble with a single column:

- height:

  Predicted site height (m), returned when \`si\` is provided.

- si:

  Predicted site index (m), returned when \`height\` is provided.

## Details

**Model scope (species coverage):** lodgepole pine (NFI code `PINU.CON`)
and interior spruce, modelled as white spruce (NFI code `PICE.GLA`),
following the source publication.

**Age definition note:** \`age\` is *total* age (years), not
breast-height age. The model is conditioned to predict a height of zero
at total age zero.

**Model form:** the base model (eq. 3) is \$\$H = a_1 \times SI \times
A^{a_2 + a_3 SI} \times a_4^{A}\$\$ where \\H\\ is height (m), \\SI\\ is
site index (m), \\A\\ is total age (years), and \\a_1, a_2, a_3, a_4\\
are fitted parameters. The parameters were fitted province-wide (Table
2) and then allowed to vary by biogeoclimatic zone via additive
indicator terms (eqs. 4-6, Table 3). This implementation stores the
resolved per-zone \\a_1, a_2, a_3, a_4\\ plus a province-wide set.

**Zone selection:** supply \`bec_zone\` to use a zone-specific parameter
set (one of \`"BWBS"\`, \`"ESSF"\`, \`"ICH"\`, \`"IDF"\`, \`"MS"\`,
\`"SBS"\`, \`"SBPS"\`). When \`bec_zone\` is \`NULL\` (the default), the
province-wide average parameters are used, as recommended by the author
when the zone is unknown or was not sampled.

Because \\SI\\ appears both as a multiplier and inside the exponent of
\\A\\, eq. 3 has no closed-form inverse in \\SI\\; when predicting site
index the equation is solved numerically.

Provide exactly one of \`height\` or \`si\`:

- If \`si\` is provided, the function predicts \`height\`.

- If \`height\` is provided, the function predicts \`si\`.

## References

Nigh, G.D. 2004. Juvenile height models for lodgepole pine and interior
spruce: validation of existing models and development of new models.
Res. Rep. 25. B.C. Ministry of Forests, Forest Science Program,
Victoria, B.C.

## Examples

``` r
# Province-wide: predict height from age + site index
si_nigh2004(age = c(5, 10, 15), species = "PINU.CON", si = 20)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1  0.733
#> 2  2.59 
#> 3  4.92 

# Zone-specific interior spruce
si_nigh2004(age = 12, species = "PICE.GLA", si = 18, bec_zone = "SBS")
#> # A tibble: 1 × 1
#>   height
#>    <dbl>
#> 1   1.28

# Predict site index from age + height
si_nigh2004(age = 10, species = "PINU.CON", height = 2.5)
#> # A tibble: 1 × 1
#>      si
#>   <dbl>
#> 1  19.6
```
