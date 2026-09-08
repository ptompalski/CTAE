# Nigh (2000) growth-intercept site index model for interior western redcedar

Implementation of the Nigh (2000) growth-intercept model for interior
western redcedar (`THUJ.PLI`).

## Usage

``` r
si_nigh2000_gi(age, gi)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years), expected in `[1, 50]`.

- gi:

  Numeric vector. Growth intercept (cm/year).

## Value

A tibble with one column:

- si:

  Predicted site index (m, base age 50 years at breast height).

## Details

The model form is age-specific: \$\$SI = 1.3 + b_0(A)\times
GI^{b_1(A)}\$\$ where `A` is breast-height age (1 to 50) and `GI` is
growth intercept (cm/year).

## References

Nigh, G. D. (2000). Western redcedar site index models for the interior
of British Columbia. British Columbia Ministry of Forests, Research
Report 18.

## Examples

``` r
si_nigh2000_gi(
  age = c(5, 15, 30, 45),
  gi = c(12, 8, 6, 5)
)
#> # A tibble: 4 × 1
#>      si
#>   <dbl>
#> 1 13.0 
#> 2  9.87
#> 3  6.28
#> 4  4.14
```
