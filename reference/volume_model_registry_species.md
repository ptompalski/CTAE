# Volume model registry with species coverage

Volume model registry with species coverage

## Usage

``` r
volume_model_registry_species()
```

## Value

A tibble like
[`volume_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/volume_model_registry.md)
plus:

- `species` (list-column of character vectors)

- `n_species` (integer)

- `species_text` (collapsed string for printing)
