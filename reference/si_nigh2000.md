# Nigh (2000) site index model for interior western redcedar (BC)

Implementation of the Nigh (2000) polymorphic site-index equation for
interior western redcedar (`THUJ.PLI`).

## Usage

``` r
si_nigh2000(age, height = NULL, si = NULL)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years).

- height:

  Optional numeric vector. Top height (m). If provided, `si` is
  predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years at breast
  height). If provided, `height` is predicted.

## Value

A tibble with columns:

- height:

  Predicted top height (m), returned when input `si` is provided.

- si:

  Predicted site index (m), returned when input `height` is provided.

## Details

**Species coverage:** `THUJ.PLI`.

**Geographic use:** interior British Columbia.

**Age definition note:** `age` is breast-height age (years).

**Base-age note:** site index in this model is referenced to base age 50
years at breast height.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

Inputs/outputs are metric (m), matching the source model.

Predicting `height` from `si` is direct. Predicting `si` from `height`
is implicit and solved numerically with
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html).

## References

Nigh, G. D. (2000). Western redcedar site index models for the interior
of British Columbia. British Columbia Ministry of Forests, Research
Report 18.

## Examples

``` r
# Predict site index from age + height
si_nigh2000(
  age = c(25, 50, 90),
  height = c(9, 16, 27)
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  17.3
#> 2  16  
#> 3  17.3

# Predict height from age + site index
si_nigh2000(
  age = c(25, 50, 90),
  si = c(12, 16, 22)
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   6.10
#> 2  16   
#> 3  32.6 
```
