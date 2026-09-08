# Nigh (1998) years-to-breast-height model for interior western hemlock

Implementation of the Nigh (1998) years-to-breast-height (YTBH) model
for western hemlock (`TSUG.HET`) in the interior of British Columbia.
The model estimates the number of years a top-height tree takes to grow
from germination to breast height (1.3 m), which is used to convert
breast-height age to total age (and vice versa).

## Usage

``` r
ytbh_nigh1998(si = NULL, ytbh = NULL)
```

## Arguments

- si:

  Optional numeric vector. Site index (m, base age 50 years at breast
  height). If provided, \`ytbh\` is predicted.

- ytbh:

  Optional numeric vector. Years to breast height (years). If provided,
  \`si\` is predicted.

## Value

A tibble with a single column:

- ytbh:

  Predicted years to breast height (years), returned when \`si\` is
  provided.

- si:

  Predicted site index (m), returned when \`ytbh\` is provided.

## Details

The source equation (eq. 5) is: \$\$YTBH = 446.6 \times SI^{-1.432}\$\$

Provide exactly one of \`si\` or \`ytbh\`:

- If \`si\` is provided, the function predicts \`ytbh\`.

- If \`ytbh\` is provided, the function predicts \`si\` by inverting the
  equation: \\SI = (YTBH / 446.6)^{1 / -1.432}\\.

## References

Nigh, G.D. (1998). A system for estimating height and site index of
western hemlock in the interior of British Columbia. The Forestry
Chronicle 74(4): 588–596.

## Examples

``` r
# Predict years-to-breast-height from site index
ytbh_nigh1998(si = c(10, 15, 20))
#> # A tibble: 3 × 1
#>    ytbh
#>   <dbl>
#> 1 16.5 
#> 2  9.24
#> 3  6.12

# Invert: predict site index from years-to-breast-height
ytbh_nigh1998(ytbh = c(8, 4, 3))
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  16.6
#> 2  26.9
#> 3  32.9
```
