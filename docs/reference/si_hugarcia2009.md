# Hu and García (2009) height-growth and site-index model for interior spruce

Unified, vectorized implementation of the Bertalanffy–Richards
height-growth and site-index model in Hu and García (2009) for interior
spruce in the Sub-Boreal Spruce (SBS) biogeoclimatic zone of British
Columbia.

## Usage

``` r
si_hugarcia2009(age, height = NULL, si = NULL, species)
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

  Character vector of NFI species codes (\`"PICE.GLA"\` or
  \`"PICE.ENG"\`).

## Value

A tibble with columns:

- height:

  Predicted top height (m), returned when input \`si\` is provided.

- si:

  Predicted site index (m), returned when input \`height\` is provided.

## Details

**Model scope (species coverage):** interior spruce, mapped to the NFI
codes `PICE.GLA` (white spruce) and `PICE.ENG` (Engelmann spruce). The
source treats the white x Engelmann interior-spruce complex as a single
entity; both codes use the same fitted coefficients.

**Age definition note:** \`age\` is breast-height age (years). The
height curve passes through the origin \\(t_0, H_0) = (0.5\\\mathrm{yr},
1.3\\\mathrm{m})\\; the model is defined only for post-breast-height
growth.

**Base-age note:** site index is the predicted top height at 50 years
breast-height age.

The selected model (Hu and García 2009, "combined model 4") is the
polymorphic Bertalanffy–Richards form \$\$H = a\left\\1 - \left\[1 -
(H_0/a)^c\right\] \exp\[-b(t - t_0)\]\right\\^{1/c},\$\$ with a
site-dependent asymptote \\a = 283.9\\q^{0.5137}\\, rate \\b = q\\, and
global shape constant \\c = 0.5829\\. The site parameter \\q\\ has no
closed form and is solved numerically.

Provide exactly one of \`height\` or \`si\`:

- If \`height\` is provided, the function predicts \`si\`.

- If \`si\` is provided, the function predicts \`height\`.

## References

Hu, Z., and García, O. (2010). A height-growth and site-index model for
interior spruce in the Sub-Boreal Spruce biogeoclimatic zone of British
Columbia. Canadian Journal of Forest Research 40(6): 1175–1183.
[doi:10.1139/X10-076](https://doi.org/10.1139/X10-076)

## Examples

``` r
# Predict height from age + site index
si_hugarcia2009(
  age = c(25, 50, 80),
  si = c(12, 18, 24),
  species = "PICE.GLA"
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   6.42
#> 2  18.0 
#> 3  32.3 

# Predict site index from age + height
si_hugarcia2009(
  age = c(25, 50, 80),
  height = c(8, 18, 26),
  species = c("PICE.GLA", "PICE.ENG", "PICE.GLA")
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  15.1
#> 2  18.0
#> 3  18.5
```
