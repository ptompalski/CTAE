# Cieszewski, Bella and Yeung (1993) variable-age site-index model for Saskatchewan

Unified, vectorized implementation of the preliminary variable-age
height-growth / site-index model of Cieszewski, Bella and Yeung (1993)
for eleven timber species in Saskatchewan. The model is a simplified
form (their eq. 2) of the Cieszewski and Bella (1989) polymorphic
variable-age height-growth model.

## Usage

``` r
si_cieszewski1993(age, height = NULL, si = NULL, species)
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

  Character vector of NFI species codes (see scope above).

## Value

A tibble with a single column:

- height:

  Predicted site height (m), returned when \`si\` is provided.

- si:

  Predicted site index (m), returned when \`height\` is provided.

## Details

**Model scope (species coverage):** eleven species, mapped to the NFI
codes `ABIE.BAL` (balsam fir), `POPU.BAL` (balsam poplar), `PICE.MAR`
(black spruce), `PINU.BAN` (jack pine), `PINU.CON` (lodgepole pine),
`ACER.NEG` (manitoba maple), `POPU.TRE` (trembling aspen), `LARI.LAR`
(tamarack), `BETU.PAP` (white birch), `ULMU.AME` (white elm), and
`PICE.GLA` (white spruce).

**Age definition note:** \`age\` is breast-height age (years). Curves
pass through breast height (1.3 m) at age 0; the model is defined only
for post-breast-height growth.

**Base-age note:** site index is site height at breast-height age 50.

The height-growth form (their eq. 2) is \$\$H = 1.3 + \frac{h_x + d +
r}{2 + \dfrac{4 b / t^{a}}{h_x - d + r}}, \quad r = \sqrt{(h_x - d)^2 +
\frac{4 b\\ h_x}{t_r^{a}}}, \quad d = \frac{b}{50^{a}},\$\$ where \\h_x
= SI - 1.3\\ is the reference height above breast height at the
reference age \\t_r = 50\\, \\t\\ is the prediction (breast-height) age,
and \\a, b\\ are species-specific fitted coefficients. Because the
reference age equals the base age (50), both directions are closed form:
at \\t = 50\\ the curve returns \\H = SI\\ exactly, and site index is
recovered analytically from an observed (age, height) pair.

Provide exactly one of \`height\` or \`si\`:

- If \`si\` is provided, the function predicts \`height\`.

- If \`height\` is provided, the function predicts \`si\`.

## References

Cieszewski, C.J., Bella, I.E., and Yeung, D.P. (1993). Preliminary
site-index height growth curves for eleven timber species in
Saskatchewan. Draft unpublished project report, Canada–Saskatchewan
Partnership Agreement in Forestry. Natural Resources Canada, Canadian
Forest Service, Prince Albert, Saskatchewan.

Cieszewski, C.J., and Bella, I.E. (1989). Polymorphic height and site
index curves for lodgepole pine in Alberta. Canadian Journal of Forest
Research 19: 1151–1160.

## Examples

``` r
# Predict height from age + site index
si_cieszewski1993(age = c(25, 50, 80), si = c(12, 16, 20), species = "PINU.BAN")
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   7.59
#> 2  16   
#> 3  24.0 

# Predict site index from age + height
si_cieszewski1993(age = c(25, 50, 80), height = c(9, 16, 21), species = "PINU.BAN")
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  14.0
#> 2  16  
#> 3  17.2
```
