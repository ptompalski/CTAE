# Huang, Meng and Yang (2009) GYPSY site index models for Alberta species

Unified, vectorized implementation of the GYPSY top-height / site-index
models in Huang, Meng, and Yang (2009) for four main Alberta tree
species.

## Usage

``` r
si_huang2009(
  age,
  height = NULL,
  si = NULL,
  species,
  index_age = c("breast_height", "total")
)
```

## Arguments

- age:

  Numeric vector. Total age (years from germination).

- height:

  Optional numeric vector. Top height (m), i.e. average height of the
  100 largest-DBH trees per hectare. If provided, `si` is predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years on the
  `index_age` scale). If provided, `height` is predicted.

- species:

  Character vector of NFI species codes (e.g., `"PINU.CON"`).

- index_age:

  Character scalar, `"breast_height"` (default) or `"total"`. Selects
  whether site index is expressed at 50 years breast-height age
  (\\SI\_{bh}\\) or 50 years total age (\\SI_t\\).

## Value

A tibble with columns:

- height:

  Predicted top height (m), returned when input `si` is provided.

- si:

  Predicted site index (m), returned when input `height` is provided.

## Details

**Model scope (species coverage):** four species: `POPU.TRE` (aspen),
`PICE.MAR` (black spruce), `PINU.CON` (lodgepole pine), `PICE.GLA`
(white spruce).

**Age definition note:** `age` is *total* age (years from the point of
germination). This is the age basis on which the GYPSY top-height models
are fitted and inverted.

**Base-age note:** site index is referenced to height at 50 years. The
`index_age` argument selects which 50-year index is used:

- `"breast_height"` (default): \\SI\_{bh}\\, top height at 50 years
  breast-height age (the index currently used in Alberta).

- `"total"`: \\SI_t\\, top height at 50 years total age (the quantity
  the model form uses directly).

The conversion between the two uses the years-to-breast-height (Y2BH)
relation embedded in the source SAS program (Appendix 1).

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si` (on the
  `index_age` scale).

- If `si` is provided (on the `index_age` scale), the function predicts
  top height.

The top-height model is not closed-form invertible in site index;
predicting `si` from `height` is solved by the damped fixed-point
iteration used in the source SAS program. When
`index_age = "breast_height"`, recovering \\SI_t\\ from a supplied
\\SI\_{bh}\\ is solved numerically with
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html).

## References

Huang, S., Meng, S.X., and Yang, Y. (2009). A Growth and Yield
Projection System (GYPSY) for Natural and Post-harvest Stands in
Alberta. Technical Report Pub. No. T/216. Forest Management Branch,
Alberta Sustainable Resource Development, Edmonton, Alberta.

## Examples

``` r
# Predict site index (SIbh) from total age + top height
si_huang2009(
  age = c(50, 70),
  height = c(20, 20),
  species = c("PICE.MAR", "PINU.CON")
)
#> # A tibble: 2 × 1
#>      si
#>   <dbl>
#> 1  21.4
#> 2  17.7

# Predict top height from total age + total-age site index (SIt)
si_huang2009(
  age = c(50, 90),
  si = c(20, 12),
  species = c("POPU.TRE", "PICE.GLA"),
  index_age = "total"
)
#> # A tibble: 2 × 1
#>   height
#>    <dbl>
#> 1   20  
#> 2   19.7
```
