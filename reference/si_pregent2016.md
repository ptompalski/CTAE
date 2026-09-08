# Prégent et al. (2016) site index model for Norway spruce plantations

Implementation of the Schumacher difference equation reported by Prégent
et al. (2016) for Norway spruce plantations (`PICE.ABI`) in Quebec.

## Usage

``` r
si_pregent2016(age, height = NULL, si = NULL, base_age = 25)
```

## Arguments

- age:

  Numeric vector. Plantation age (years), with `age > 0`. A warning is
  emitted when `age > 70`, because that exceeds the published range.

- height:

  Optional numeric vector. Dominant height (m). If provided, `si` is
  predicted.

- si:

  Optional numeric vector. Site index (m) at `base_age` years. If
  provided, `height` is predicted.

- base_age:

  Positive numeric scalar. Site-index base age (years since planting).
  Defaults to `25`.

## Value

A tibble with columns:

- height:

  Predicted dominant height (m), returned when input `si` is provided.

- si:

  Predicted site index (m), returned when input `height` is provided.

## Details

**Species coverage:** `PICE.ABI`.

**Geographic use:** Quebec Norway spruce plantations.

**Age definition note:** `age` is plantation age (years since planting),
not breast-height age.

**Height definition note:** the model uses dominant height (m) based on
the mean height of the 100 tallest trees per hectare.

**Base-age note:** site index is dominant height at plantation age 25
years by default, matching the source study. The underlying difference
equation is base-age invariant, so other positive `base_age` values can
also be used. The package article plots this model with `base_age = 50`
for visual comparison with other curves.

**Domain note:** the source tables and curves are presented for
plantation ages up to 70 years. This implementation warns when
`age > 70` and returns extrapolated values.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

Inputs and outputs are metric and follow the source model scale
directly.

## References

Prégent, G., Auger, I., & others. (2016). *Tarif de cubage, tables de
rendement et modèles de croissance pour les plantations d'épinette de
Norvège au Québec*. Mémoire de recherche forestière no 176. Gouvernement
du Québec.

## Examples

``` r
si_pregent2016(
  age = c(20, 30, 40),
  height = c(6, 10, 14)
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  8.10
#> 2  7.98
#> 3  8.14

si_pregent2016(
  age = c(20, 30, 40),
  si = c(8, 10, 12)
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   5.92
#> 2  12.3 
#> 3  19.2 
```
