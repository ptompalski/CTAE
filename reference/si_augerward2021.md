# Auger and Ward (2021) site index model for Quebec plantations

Implementation of the dominant-height difference equations reported by
Auger and Ward (2021) for jack pine plantations (`PINU.BAN`) and black
spruce plantations (`PICE.MAR`) in Quebec.

## Usage

``` r
si_augerward2021(age, height = NULL, si = NULL, species, base_age = 25)
```

## Arguments

- age:

  Numeric vector. Plantation age (years), with `age > 0`. A warning is
  emitted when `age > 100`, because that exceeds the recommended range
  in the source document.

- height:

  Optional numeric vector. Dominant height (m). If provided, `si` is
  predicted.

- si:

  Optional numeric vector. Site index (m) at `base_age` years. If
  provided, `height` is predicted.

- species:

  Character vector of species codes (e.g., `"PINU.BAN"` or
  `"PICE.MAR"`).

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

**Species coverage:** `PINU.BAN`, `PICE.MAR`.

**Geographic use:** Quebec plantations.

**Age definition note:** `age` is plantation age (years since planting),
not breast-height age.

**Height definition note:** the model uses dominant height (m) based on
the mean height of the 100 tallest planted trees per hectare.

**Base-age note:** the source document defines `IQS` at plantation age
25 years. The equations are written as difference equations, so other
positive `base_age` values can also be used.

**Domain note:** the source recommends limiting use to plantation ages
up to 100 years. This implementation warns when `age > 100` and returns
extrapolated values.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

Inputs and outputs are metric and follow the source model scale
directly.

## References

Auger, I., & Ward, C. (2021). *Tables de rendement pour les plantations
d'epinette noire et les plantations de pin gris au Quebec*. Avis
technique SSS-06. Gouvernement du Quebec. Version corrigee diffusee en
2024.

## Examples

``` r
si_augerward2021(
  age = c(20, 30),
  si = c(9, 12),
  species = c("PICE.MAR", "PINU.BAN")
)
#> # A tibble: 2 × 1
#>   height
#>    <dbl>
#> 1   7.13
#> 2  14.0 

si_augerward2021(
  age = c(20, 30),
  height = c(7, 10),
  species = c("PICE.MAR", "PINU.BAN")
)
#> # A tibble: 2 × 1
#>      si
#>   <dbl>
#> 1  8.85
#> 2  8.37
```
