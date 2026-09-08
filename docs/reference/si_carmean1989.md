# Carmean et al. (1989) site index model

Implementation of selected Carmean et al. (1989) site-index equations
for eastern North American tree species

## Usage

``` r
si_carmean1989(age, height = NULL, si = NULL, species)
```

## Arguments

- age:

  Numeric vector. Total age (years).

- height:

  Optional numeric vector. Total tree height (m). If provided, \`si\` is
  predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years at total
  age). If provided, \`height\` is predicted.

- species:

  Character vector of species codes (e.g., \`"ACER.SAH"\`).

## Value

A tibble with columns:

- height:

  Predicted height (m), returned when input \`si\` is provided.

- si:

  Predicted site index (m), returned when input \`height\` is provided.

## Details

**Model scope (species coverage):** this implementation includes only
selected Carmean et al. (1989) species that occur in Canada:
`ACER.SAH, BETU.ALL, FAGU.GRA, FRAX.AME, FRAX.NIG, PRUN.SER, QUER.RUB, TILI.AME, ULMU.AME, CHAM.THY, TSUG.CAN`.

**Geographic use:** use for eastern species only, and with caution
outside the source curve domains.

**Age definition note:** \`age\` is total age (years). For users working
with breast-height age, years-to-breast-height can be obtained
separately with
[`ytbh_carmean1989`](https://ptompalski.github.io/CanadaForestAllometry/reference/ytbh_carmean1989.md).

**Base-age note:** site index in this model is total height at 50 years
total age.

Provide exactly one of \`height\` or \`si\`:

- If \`height\` is provided, the function predicts \`si\`.

- If \`si\` is provided, the function predicts \`height\`.

Inputs/outputs are metric; the original equations are in imperial units,
so the function converts internally.

## References

Carmean, W. H., Hahn, J. T., & Jacobs, R. D. (1989). Site index curves
for forest tree species in the eastern United States. U.S. Department of
Agriculture, Forest Service, Northern Research Station.

## Examples

``` r
# Predict site index from age + height
si_carmean1989(
  age = c(30, 50, 60),
  height = c(12, 18, 20),
  species = c("ACER.SAH", "BETU.ALL", "QUER.RUB")
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  17.8
#> 2  17.9
#> 3  18.0

# Predict height from age + site index
si_carmean1989(
  age = c(30, 50, 60),
  si = c(18, 20, 22),
  species = c("ACER.SAH", "BETU.ALL", "QUER.RUB")
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   12.2
#> 2   20.1
#> 3   24.1
```
