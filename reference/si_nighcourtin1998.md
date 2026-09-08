# Nigh and Courtin (1998) site index model for red alder (BC coast)

Implementation of the Nigh and Courtin (1998) anamorphic
height-age/site-index equations for red alder (`ALNU.RUB`) in coastal
British Columbia.

## Usage

``` r
si_nighcourtin1998(age, height = NULL, si = NULL, si50 = FALSE)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years).

- height:

  Optional numeric vector. Dominant height (m). If provided, `si` is
  predicted.

- si:

  Optional numeric vector. Site index SI25 (m at breast-height age 25).
  If provided, `height` is predicted.

- si50:

  Logical scalar. Default `FALSE`. If `TRUE`, interpret input `si` (when
  provided) as SI50 and return predicted `si` (when `height` is
  provided) as SI50 using: \$\$SI50 = -0.4063 + 1.313 \times SI25\$\$
  and its inverse.

## Value

A tibble with columns:

- height:

  Predicted dominant height (m), returned when input `si` is provided.

- si:

  Predicted site index SI25 (m), returned when input `height` is
  provided.

## Details

**Species coverage:** `ALNU.RUB`.

**Age definition note:** `age` is breast-height age (years).

**Base-age note:** site index in this model is SI25 (m at breast-height
age 25 years), not SI50.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

Both directions are explicit closed forms from the source publication.

## References

Nigh, G. D., & Courtin, P. J. (1998). Height models for Red Alder
(*Alnus rubra* Bong.) in British Columbia. *New Forests*, 16, 59-70.

## Examples

``` r
# Predict site index SI25 from age + height
si_nighcourtin1998(
  age = c(10, 25, 40),
  height = c(8, 18, 24)
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  14.1
#> 2  18.0
#> 3  19.8

# Predict height from age + site index SI25
si_nighcourtin1998(
  age = c(10, 25, 40),
  si = c(14, 18, 22)
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   7.93
#> 2  18.0 
#> 3  26.6 

# Use SI50 instead of SI25
si_nighcourtin1998(
  age = c(10, 25, 40),
  si = c(18, 23, 28),
  si50 = TRUE
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   7.94
#> 2  17.8 
#> 3  26.2 
```
