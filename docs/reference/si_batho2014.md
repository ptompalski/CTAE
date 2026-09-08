# Batho and García (2014) height-growth and site-index model for lodgepole pine

Unified, vectorized implementation of the Bertalanffy–Richards
height-growth and site-index model in Batho and García (2014) for
lodgepole pine (*Pinus contorta* var. *latifolia*) in the Sub-Boreal
Spruce (SBS) biogeoclimatic zone of British Columbia.

## Usage

``` r
si_batho2014(age, height = NULL, si = NULL, species)
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

  Character vector of NFI species codes (\`"PINU.CON"\`).

## Value

A tibble with columns:

- height:

  Predicted top height (m), returned when input \`si\` is provided.

- si:

  Predicted site index (m), returned when input \`height\` is provided.

## Details

**Model scope (species coverage):** lodgepole pine, NFI code `PINU.CON`.

**Age definition note:** \`age\` is breast-height age (years). The
height curve passes through \\(t_0, H_0) = (0.5\\\mathrm{yr},
1.3\\\mathrm{m})\\; the model is defined only for post-breast-height
growth.

**Base-age note:** site index is the predicted top height at 50 years
breast-height age.

The final published model (the "Power combined" fit) is the polymorphic
Bertalanffy–Richards form \$\$H = a_q\left\\1 - \left\[1 -
(H_0/a_q)^c\right\] \exp\[-q(t - t_0)\]\right\\^{1/c},\$\$ with a
site-dependent asymptote \\a_q = 12313\\q^{1.645}\\ and global shape
constant \\c = 0.8297\\ (Batho and García 2014, Eqs. 3–4). The site
parameter \\q\\ has no closed form and is solved numerically from an
observed (age, height) pair (Eq. 6).

Provide exactly one of \`height\` or \`si\`:

- If \`height\` is provided, the function predicts \`si\`.

- If \`si\` is provided, the function predicts \`height\`.

## References

Batho, A., and García, O. (2014). A Site Index Model for Lodgepole Pine
in British Columbia. Forest Science 60(5): 982–987.
[doi:10.5849/forsci.13-509](https://doi.org/10.5849/forsci.13-509)

## Examples

``` r
# Predict height from age + site index
si_batho2014(
  age = c(25, 50, 80),
  si = c(12, 18, 24),
  species = "PINU.CON"
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   7.49
#> 2  18.0 
#> 3  29.2 

# Predict site index from age + height
si_batho2014(
  age = c(25, 50, 80),
  height = c(8, 18, 26),
  species = "PINU.CON"
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  12.8
#> 2  18.0
#> 3  21.2
```
