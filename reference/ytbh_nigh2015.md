# Nigh (2015) years-to-breast-height model for Engelmann spruce

Implementation of the Nigh (2015) years-to-breast-height model for
Engelmann spruce (`PICE.ENG`) in the Engelmann Spruce – Subalpine Fir
(ESSF) biogeoclimatic zone of British Columbia.

## Usage

``` r
ytbh_nigh2015(si)
```

## Arguments

- si:

  Numeric vector. Site index (m, base age 50 years at breast height).

## Value

A tibble with one column:

- ytbh:

  Predicted years to breast height (years).

## Details

The source equation (model 2 in Nigh 2015) is: \$\$YTBH = 4.465 + 154.6
/ SI\$\$

This model was developed specifically for Engelmann spruce and is
recommended in place of the natural-stand white spruce
years-to-breast-height model that had previously been used for the
species. It may be extrapolated into zones neighbouring the ESSF
provided the species is truly Engelmann spruce; for the white x
Engelmann cross, the white spruce models should be used instead.

## References

Nigh, G. D. (2015). Years-to-breast-height model for Engelmann spruce in
the Engelmann Spruce – Subalpine Fir biogeoclimatic zone. Province of
British Columbia, Victoria, B.C. Extension Note 115.

## Examples

``` r
ytbh_nigh2015(
  si = c(10, 15, 20)
)
#> # A tibble: 3 × 1
#>    ytbh
#>   <dbl>
#> 1  19.9
#> 2  14.8
#> 3  12.2
```
