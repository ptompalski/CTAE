# Nigh (1998) growth-intercept site index model for interior western hemlock

Implementation of the Nigh (1998) growth-intercept site index model for
western hemlock (`TSUG.HET`) in the interior of British Columbia. Site
index is estimated from the growth intercept (early height growth) with
a separate parameter pair for each breast-height age.

## Usage

``` r
si_nigh1998_gi(age, gi)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years); one of 5, 10, 20, 30, 40,
  50.

- gi:

  Numeric vector. Growth intercept (cm/year).

## Value

A tibble with one column:

- si:

  Predicted site index (m, base age 50 years at breast height).

## Details

The model form (eq. 4) is age-specific: \$\$SI = 1.3 + e^{b_1(A)} \times
GI^{b_2(A)}\$\$ where \`A\` is breast-height age and \`GI\` is the
growth intercept (cm/year). Following the source, parameter \\b_1\\ was
fitted on the log scale and enters the model as \\e^{b_1}\\ (Ratkowsky
transformation); the tabulated \\b_1\\ values (stored as \`b1_log\`) are
therefore exponentiated internally.

Coefficients are tabulated (Table 3) for breast-height ages 5, 10, 20,
30, 40, and 50; \`age\` must be one of these values.

## References

Nigh, G.D. (1998). A system for estimating height and site index of
western hemlock in the interior of British Columbia. The Forestry
Chronicle 74(4): 588–596.

## Examples

``` r
si_nigh1998_gi(
  age = c(5, 20, 50),
  gi = c(30, 18, 12)
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1 18.0 
#> 2 12.1 
#> 3  7.25
```
