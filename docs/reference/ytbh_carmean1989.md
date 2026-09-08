# Carmean et al. (1989) years-to-breast-height model

Returns years to breast height (YTBH) for the species retained from
Carmean et al. (1989).

## Usage

``` r
ytbh_carmean1989(si, species)
```

## Arguments

- si:

  Numeric vector. Site index (m, base age 50 years at total age).

- species:

  Character vector of species codes (e.g., \`"ACER.SAH"\`).

## Value

A tibble with one column:

- ytbh:

  Predicted years to breast height (years).

## Details

**Model scope (species coverage):** this implementation includes only
selected Carmean et al. (1989) species that occur in Canada:
`ACER.SAH, BETU.ALL, FAGU.GRA, FRAX.AME, FRAX.NIG, PRUN.SER, QUER.RUB, TILI.AME, ULMU.AME, CHAM.THY, TSUG.CAN`.

For most species, the source provides a fixed years-to-breast-height
value in the figure caption. For Atlantic white-cedar (`CHAM.THY`), the
source provides a site-index table in feet, and this implementation uses
linear interpolation between the tabulated values.

## References

Carmean, W. H., Hahn, J. T., & Jacobs, R. D. (1989). Site index curves
for forest tree species in the eastern United States. U.S. Department of
Agriculture, Forest Service, Northern Research Station.

## Examples

``` r
ytbh_carmean1989(
  si = c(18, 20, 22),
  species = c("ACER.SAH", "CHAM.THY", "TSUG.CAN")
)
#> # A tibble: 3 × 1
#>    ytbh
#>   <dbl>
#> 1  4   
#> 2  6.44
#> 3  6   
```
