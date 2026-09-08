# Scott and Voorhis (1986) site index model

Unified, vectorized implementation of the Scott and Voorhis (1986)
polymorphic site-index equation for northeastern species.

## Usage

``` r
si_scottvoorhis1986(
  age,
  height = NULL,
  si = NULL,
  species,
  convert_to_total_age = FALSE
)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years).

- height:

  Optional numeric vector. Total tree height (m). If provided, `si` is
  predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years at breast
  height). If provided, `height` is predicted.

- species:

  Character vector of species codes (e.g., `"ABIE.BAL"`).

- convert_to_total_age:

  Logical scalar. If `TRUE`, converts breast-height age to total age
  internally using the Scott and Voorhis age-to-breast-height relation
  (source-consistent behavior). If `FALSE` (default), uses `age`
  directly in the height equation.

## Value

A tibble with columns:

- height:

  Predicted height (m), returned when input `si` is provided.

- si:

  Predicted site index (m), returned when input `height` is provided.

## Details

**Model scope (species coverage):** this implementation includes
parameter sets for 19 species:
`ABIE.BAL, PICE.GLA, PICE.MAR, PICE.RUB, PINU.BAN, PINU.ECH, PINU.RES, PINU.STR, PINU.TAE, PINU.VIR, THUJ.OCC, ACER.SAH, BETU.ALL, BETU.PAP, FRAX.AME, LIQU.STY, LIRI.TUL, POPU.GRA, QUER.ALB`.

**Geographic use (Canada):** the model is intended for northeastern
forest conditions. In Canada, it is most defensible for
species/populations in eastern regions (notably Atlantic Canada and
adjacent central Canada where these species occur). Use caution outside
that domain.

**Age definition note:** `age` is breast-height age (years). The model
internally computes age-to-breast-height and uses total age in the
height equation.

**Base-age note:** site index in this model is referenced to base age 50
years, but the fitted equations were not constrained to pass exactly
through SI at base age for every species.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

In this model, total age is the sum of breast-height age and
age-to-breast-height. Age-to-breast-height is itself a function of site
index, so predicting `si` from `height` is implicit and solved
numerically with
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html).

Inputs/outputs are metric; the original equations are in imperial units,
so the function converts internally.

## References

Scott, C. T., & Voorhis, N. G. (1986). Northeastern forest survey site
index equations and site productivity classes. *Northern Journal of
Applied Forestry*, 3(4), 144-148.

## Examples

``` r
# Predict site index from age + height
si_scottvoorhis1986(
  age = c(25, 40, 60),
  height = c(8, 14, 20),
  species = c("ABIE.BAL", "PICE.MAR", "PINU.BAN")
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  22.7
#> 2  16.9
#> 3  18.2

# Predict height from age + site index
si_scottvoorhis1986(
  age = c(25, 40, 60),
  si = c(11, 13, 16),
  species = c("ABIE.BAL", "PICE.MAR", "PINU.BAN"),
  convert_to_total_age = TRUE
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   8.85
#> 2  12.1 
#> 3  18.3 
```
